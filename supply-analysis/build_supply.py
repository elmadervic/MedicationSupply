"""
Build a long-form supply table from the raw sources.
"""
from __future__ import annotations

import re
import sys

import pandas as pd

from config import INTERIM, RAW
from countries import resolve
from normalize import normalise

COLUMN_OVERRIDES: dict[str, dict[str, str]] = {}

CEP_HINTS = {
    "substance": ["substance", "monograph name", "product", "name"],
    "holder": ["certificate (cep) holder", "holder", "company", "applicant"],
    "status": ["status cep", "status", "state", "validity"],
    "cep_no": ["certificate (cep) number", "certificate number", "cep number"],
    "cep_type": ["type cep", "cep type", "type"],
    "country": ["country", "pays", "iso"],
}


def _pick(df: pd.DataFrame, hints: list[str], required: bool, label: str) -> str | None:
    cols = {str(c).lower().strip(): c for c in df.columns}
    for h in hints:
        for low, orig in cols.items():
            if low == h:
                return orig
    for h in hints:
        for low, orig in cols.items():
            if h in low:
                return orig
    if required:
        raise KeyError(f"no column matched {label} among {list(df.columns)}")
    return None


def _read_delimited(path) -> pd.DataFrame:
    last = None
    for enc in ("utf-8", "utf-8-sig", "cp1252", "latin-1"):
        for sep in ("\t", ";", "|", ","):
            try:
                df = pd.read_csv(path, sep=sep, encoding=enc, engine="python",
                                 dtype=str, on_bad_lines="skip")
            except Exception as exc:
                last = exc
                continue
            if df.shape[1] > 1:
                print(f"  [{path.name}: enc={enc} sep={sep!r} -> {df.shape}]")
                return df
    raise ValueError(f"could not parse {path}: {last}")


def build_cep(exclude_types=("TSE",)) -> pd.DataFrame:
    path = RAW / "edqm_cep.txt"
    if not path.exists():
        print("  edqm_cep.txt missing - skipping API layer (see MANUAL_SOURCES)")
        return pd.DataFrame()

    df = pd.read_csv(path, sep="\t", encoding="utf-8-sig", dtype=str,
                     engine="python", on_bad_lines="skip")
    df.columns = [str(c).strip() for c in df.columns]
    print(f"  [edqm_cep.txt -> {df.shape}] columns: {list(df.columns)}")

    ov = COLUMN_OVERRIDES.get("edqm_cep", {})
    c_sub = ov.get("substance") or _pick(df, CEP_HINTS["substance"], True, "substance")
    c_hold = ov.get("holder") or _pick(df, CEP_HINTS["holder"], True, "holder")
    c_stat = ov.get("status") or _pick(df, CEP_HINTS["status"], False, "status")
    c_no = ov.get("cep_no") or _pick(df, CEP_HINTS["cep_no"], False, "cep_no")
    c_type = ov.get("cep_type") or _pick(df, CEP_HINTS["cep_type"], False, "cep_type")
    print(f"  CEP columns -> substance={c_sub!r} holder={c_hold!r} "
          f"status={c_stat!r} cep_no={c_no!r} type={c_type!r}")

    if c_type:
        before = len(df)
        types = df[c_type].fillna("").str.strip().str.upper()
        print(f"  CEP types present: {types.value_counts().to_dict()}")
        drop = {t.upper() for t in exclude_types}
        df = df[~types.isin(drop)]
        print(f"  excluded {before - len(df):,} rows of type {sorted(drop)} "
              "(not active-substance certificates)")
        kept = df[c_type].fillna("").str.strip().str.upper().value_counts().to_dict()
        print(f"  kept: {kept}")
    else:
        print("  WARNING: no 'Type CEP' column found - TSE certificates cannot "
              "be excluded and supplier counts will be inflated")

    out = pd.DataFrame({
        "substance_raw": df[c_sub],
        "supplier_name": df[c_hold].fillna("").str.strip(),
        "supplier_id": df[c_no] if c_no else df[c_hold],
        "status": df[c_stat].fillna("unknown") if c_stat else "unknown",
        "cep_type": df[c_type] if c_type else "",
    })
    out["country_iso2"] = [resolve(v) for v in df[c_hold]]
    out["source"] = "edqm_cep"
    out["role"] = "api_cep"
    return out


def build_ema_epar_manufacturers() -> pd.DataFrame:
    path = RAW / "manufacturers.csv"
    if not path.exists():
        print("  manufacturers.csv missing - skipping EPAR layer")
        return pd.DataFrame()
    df = _read_delimited(path)
    step_map = {"biological_active_substance": "bio_api", "batch_release": "batch_release"}
    out = pd.DataFrame({
        "substance_raw": df.get("active_substance"),
        "supplier_name": df.get("manufacturer_name", pd.Series(dtype=str)).fillna("").str.strip(),
        "supplier_id": df.get("manufacturer_name"),
        "status": "authorised",
    })
    out["country_iso2"] = [resolve(v) for v in df.get("country", pd.Series(dtype=str))]
    out["source"] = "ema_epar"
    out["role"] = df.get("manufacturer_step", pd.Series(dtype=str)).map(step_map).fillna("unknown")
    return out


def build_hpra() -> pd.DataFrame:
    path = RAW / "hpra_products.xml"
    if not path.exists():
        print("  hpra_products.xml missing - skipping IE layer")
        return pd.DataFrame()
    try:
        df = pd.read_xml(path)
    except Exception as exc:
        print(f"  hpra parse failed ({exc}); check inspect_sources output")
        return pd.DataFrame()
    c_type = _pick(df, ["producttype"], False, "product type")
    if c_type:
        before = len(df)
        df = df[df[c_type].astype(str).str.upper().str.startswith("HM")]
        print(f"  hpra: kept {len(df):,} human of {before:,} products "
              f"({before - len(df):,} veterinary/other dropped)")
    c_sub = _pick(df, ["activesubstance", "active substance", "substance", "ingredient"], False, "substance")
    c_mah = _pick(df, ["licenceholder", "paholder", "holder", "applicant", "company"], False, "holder")
    if not (c_sub and c_mah):
        print(f"  hpra: could not locate substance/holder columns in {list(df.columns)[:15]}")
        return pd.DataFrame()
    out = pd.DataFrame({
        "substance_raw": df[c_sub].astype(str).str.replace(r"\s+", " ", regex=True).str.strip(),
        "supplier_name": df[c_mah].astype(str).str.strip(),
        "supplier_id": df[c_mah],
        "status": "authorised",
    })
    out["country_iso2"] = [resolve(v) for v in df[c_mah]]
    out["source"] = "hpra"
    out["role"] = "mah_national"
    return out


def main() -> int:
    frames = [f for f in (build_cep(), build_ema_epar_manufacturers(), build_hpra())
              if not f.empty]
    if not frames:
        print("nothing to build - no parsable supply sources in data/raw")
        return 1
    sup = pd.concat(frames, ignore_index=True)

    norm = [normalise(v) for v in sup["substance_raw"]]
    sup["norm_key"] = [n.key for n in norm]
    sup["is_combination"] = [n.is_combination for n in norm]
    sup["norm_trace"] = ["; ".join(n.trace) for n in norm]

    sup = sup[sup["norm_key"].astype(bool)].copy()
    sup["supplier_name"] = sup["supplier_name"].astype(str).str.replace(r"\s+", " ", regex=True).str.strip()
    sup["supplier_norm"] = (sup["supplier_name"].str.lower()
                            .str.replace(r"\b(gmbh|ag|s\.?a\.?|s\.?p\.?a\.?|ltd|limited|inc|llc|bv|nv|kft|sro|oy|ab|as|plc|co|corp|pvt|private|pharmaceuticals?|pharma)\b", " ", regex=True)
                            .str.replace(r"[^a-z0-9 ]+", " ", regex=True)
                            .str.replace(r"\s+", " ", regex=True).str.strip())

    dest = INTERIM / "supply_long.csv"
    sup.to_csv(dest, index=False)
    print(f"\nwrote {dest}  ({len(sup):,} rows, "
          f"{sup['norm_key'].nunique():,} substances, "
          f"{sup['country_iso2'].isna().sum():,} of {len(sup):,} rows "
          "have no resolvable country)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
