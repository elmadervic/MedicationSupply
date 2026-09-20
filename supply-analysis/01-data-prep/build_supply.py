"""
Build a long-form supply table from the raw sources.

Each row is one substance x supplier x country x role. Where the source file
publishes an ATC code it is carried through as-is; otherwise the substance is
looked up in the cross-source ATC map built by common/atc.py. See the
atc_codes / atc_origin columns.
"""
from __future__ import annotations

import re
import sys
import xml.etree.ElementTree as ET

import pandas as pd

# The shared modules live in supply-analysis/common -- put that directory on
# the import path so this script can still be run directly, from any working
# directory, exactly as the README describes.
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "common"))

from atc import build_atc_map, chapters, join_codes, split_codes
from config import INTERIM, RAW
from countries import resolve
from normalize import normalise

COLUMNS = [
    "substance_raw", "norm_key", "is_combination",
    "atc_codes", "atc_level1", "atc_origin",
    "supplier_name", "supplier_norm", "supplier_id", "country_iso2",
    "source", "role", "status", "cep_type", "norm_trace",
]

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
    out["atc_native"] = ""
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
    native = df.get("atc_code", pd.Series(dtype=str))
    out["atc_native"] = [join_codes(split_codes(v)) for v in native]
    print(f"  EPAR: {(out['atc_native'] != '').sum():,} of {len(out):,} rows carry a native ATC code")
    return out


def _localname(tag: str) -> str:
    """Strip the XML namespace: '{https://...}ATC' -> 'ATC'."""
    return tag.rsplit("}", 1)[-1]


def _child_texts(product: ET.Element, wrapper: str) -> list[str]:
    """
    Text of every entry under <wrapper>.

    HPRA wraps repeated fields in a container element:

        <ActiveSubstances><ActiveSubstance>Sodium chloride</ActiveSubstance></ActiveSubstances>
        <ATCs><ATC>V07AB</ATC></ATCs>

    pandas.read_xml flattens only the top level, so it reads the container's
    own text - which is just the whitespace before the first child - and every
    value is lost. That is why this layer produced zero usable rows until the
    parsing was rewritten to walk the children.
    """
    out = []
    for child in product:
        if _localname(child.tag) != wrapper:
            continue
        kids = list(child)
        if kids:
            out += [(k.text or "").strip() for k in kids]
        elif (child.text or "").strip():
            out.append(child.text.strip())
    return [" ".join(v.split()) for v in out if v and v.strip()]


def _child_text(product: ET.Element, tag: str) -> str:
    for child in product:
        if _localname(child.tag) == tag:
            return " ".join((child.text or "").split())
    return ""


def build_hpra() -> pd.DataFrame:
    path = RAW / "hpra_products.xml"
    if not path.exists():
        print("  hpra_products.xml missing - skipping IE layer")
        return pd.DataFrame()
    try:
        root = ET.parse(path).getroot()
    except Exception as exc:
        print(f"  hpra parse failed ({exc}); check inspect_sources output")
        return pd.DataFrame()

    products = [el for el in root if _localname(el.tag) == "Product"]
    rows = []
    n_human = n_no_substance = 0
    for product in products:
        if not _child_text(product, "ProductType").upper().startswith("HM"):
            continue
        n_human += 1
        holder = _child_text(product, "PAHolder")
        substances = _child_texts(product, "ActiveSubstances")
        if not substances:
            n_no_substance += 1
            continue
        codes = join_codes(c for v in _child_texts(product, "ATCs") for c in split_codes(v))
        for substance in substances:
            rows.append({
                "substance_raw": substance,
                "supplier_name": holder,
                "supplier_id": _child_text(product, "LicenceNumber") or holder,
                "status": "authorised",
                "atc_native": codes,
            })

    print(f"  hpra: {len(products):,} products, {n_human:,} human, "
          f"{n_no_substance:,} with no active substance listed "
          f"-> {len(rows):,} substance rows")
    if not rows:
        return pd.DataFrame()

    out = pd.DataFrame(rows)
    out["country_iso2"] = pd.NA
    out["source"] = "hpra"
    out["role"] = "mah_national"
    print(f"  hpra: {(out['atc_native'] != '').sum():,} of {len(out):,} rows carry a native ATC code; "
          "no country assigned (holder names are not reliable country evidence)")
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

    sup = attach_atc(sup)

    for col in COLUMNS:
        if col not in sup:
            sup[col] = ""
    sup = sup[COLUMNS]

    dest = INTERIM / "supply_long.csv"
    sup.to_csv(dest, index=False)
    print(f"\nwrote {dest}  ({len(sup):,} rows, "
          f"{sup['norm_key'].nunique():,} substances, "
          f"{sup['country_iso2'].isna().sum():,} of {len(sup):,} rows "
          "have no resolvable country)")
    return 0


def attach_atc(sup: pd.DataFrame) -> pd.DataFrame:
    """
    Fill atc_codes / atc_level1 / atc_origin.

    A row keeps the code its own source published ("native"). Only rows
    without one fall back to the cross-source substance lookup ("lookup"),
    so a source is never overruled by the map.
    """
    print("\nATC codes:")
    amap = build_atc_map()

    native = sup.get("atc_native", pd.Series("", index=sup.index)).fillna("").astype(str)
    looked_up = [join_codes(amap.get(k, [])) for k in sup["norm_key"]]

    sup["atc_codes"] = [n if n else l for n, l in zip(native, looked_up)]
    sup["atc_origin"] = ["native" if n else ("lookup" if l else "")
                         for n, l in zip(native, looked_up)]
    sup["atc_level1"] = [chapters(c) for c in sup["atc_codes"]]
    sup = sup.drop(columns=["atc_native"], errors="ignore")

    have = sup["atc_codes"].astype(bool)
    print(f"  {have.sum():,} of {len(sup):,} rows have an ATC code ({have.mean():.1%})")
    by_source = (sup.assign(has=have)
                 .groupby("source")
                 .agg(rows=("has", "size"), with_atc=("has", "sum"),
                      share=("has", "mean")))
    for src, row in by_source.iterrows():
        print(f"    {src:<10} {int(row['with_atc']):>6,} / {int(row['rows']):>6,}  {row['share']:.1%}")
    print(f"  origin: {sup['atc_origin'].replace('', 'none').value_counts().to_dict()}")
    multi = sup["atc_codes"].str.contains(r"\|", na=False)
    print(f"  {multi.sum():,} rows map to more than one code "
          f"(max {max((c.count('|') + 1) for c in sup['atc_codes'] if c) if have.any() else 0} on one substance)")
    return sup


if __name__ == "__main__":
    sys.exit(main())
