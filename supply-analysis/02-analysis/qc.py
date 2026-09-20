"""
Quality control checks on the supply data.
"""
from __future__ import annotations

import json
import sys

import pandas as pd
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "common"))

from atc import ATC_RE
from config import COUNTRY_ROLES, INTERIM, MANIFEST, OUT, RAW
from normalize import normalise
from ulcm import read_ulcm

FAILS: list[str] = []


def check(name: str, ok: bool, detail: str, warn_only: bool = False) -> None:
    tag = "PASS" if ok else ("WARN" if warn_only else "FAIL")
    if not ok and not warn_only:
        FAILS.append(name)
    print(f"[{tag}] {name}: {detail}")


def main() -> int:
    sup = pd.read_csv(INTERIM / "supply_long.csv", dtype=str)
    full = pd.read_csv(OUT / "substance_supply.csv")

    if MANIFEST.exists():
        man = json.loads(MANIFEST.read_text())
        errs = [k for k, v in man["sources"].items() if "error" in v]
        check("snapshot complete", not errs,
              f"snapshot {man['snapshot_utc']}; missing: {errs or 'none'}",
              warn_only=True)
    else:
        check("snapshot complete", False, "no manifest.json")

    geo = sup[sup["role"].isin(COUNTRY_ROLES)]
    unresolved = geo["country_iso2"].isna().mean()
    check("country resolution", unresolved < 0.05,
          f"{geo['country_iso2'].isna().sum():,} of {len(geo):,} supplier rows "
          f"in {COUNTRY_ROLES} have no country "
          f"(sample: {geo.loc[geo['country_iso2'].isna(), 'supplier_name'].dropna().head(3).tolist()})")
    countryless = sup[~sup["role"].isin(COUNTRY_ROLES)]
    check("country-less roles", True,
          f"{len(countryless):,} rows in roles without a published country "
          f"({sorted(countryless['role'].dropna().unique())}) - counted for "
          "substance and ATC coverage only",
          warn_only=True)

    dup = sup.duplicated(subset=["norm_key", "role", "supplier_norm"]).mean()
    check("duplicate supplier rows", dup < 0.30,
          f"{dup:.1%} of rows duplicate an existing substance/role/supplier "
          "(expected >0: one holder can hold several CEPs for one substance)",
          warn_only=True)

    collapse = (sup.groupby("norm_key")["substance_raw"].nunique()
                .sort_values(ascending=False))
    worst = collapse.head(8)
    check("normalisation collapse", collapse.max() < 60,
          f"largest key absorbs {collapse.max()} distinct raw strings; "
          f"top: {dict(worst)}", warn_only=True)

    atc = sup["atc_codes"].fillna("")
    have = atc.astype(bool)
    by_source = sup.assign(has=have).groupby("source")["has"].mean()
    check("ATC coverage", have.mean() > 0.40,
          f"{have.sum():,} of {len(sup):,} rows carry an ATC code "
          f"({have.mean():.1%}); by source: "
          f"{ {k: f'{v:.0%}' for k, v in by_source.items()} }",
          warn_only=True)

    codes = [c for cell in atc for c in str(cell).split("|") if c]
    bad = sorted({c for c in codes if not ATC_RE.match(c)})
    check("ATC format", not bad,
          f"{len(codes):,} codes written, {len(set(codes)):,} distinct, "
          f"{len(bad)} malformed{': ' + str(bad[:5]) if bad else ''}")

    native = sup[sup["atc_origin"] == "native"]
    lookup = sup[sup["atc_origin"] == "lookup"]
    check("ATC provenance", True,
          f"{len(native):,} rows from the source's own ATC column, "
          f"{len(lookup):,} from the cross-source substance lookup, "
          f"{int((~have).sum()):,} with none",
          warn_only=True)

    cov_by_sub = full.groupby("norm_key")["role"].apply(lambda s: s.notna().any())
    cov = float(cov_by_sub.mean())
    check("ULCM coverage", cov > 0.40,
          f"{cov:.1%} of {len(cov_by_sub):,} ULCM substances have any supply record")
    gaps = (full[full["norm_key"].isin(cov_by_sub[~cov_by_sub].index)]
            .drop_duplicates("norm_key").copy())
    if len(gaps):
        gaps["atc1"] = gaps["atc_codes"].astype(str).str[0]
        by = gaps["atc1"].value_counts().head(6).to_dict()
        print(f"       uncovered by ATC level 1: {by}")

    ncomb = full["is_combination"].sum() if "is_combination" in full else 0
    check("combination handling", True,
          f"{ncomb:,} combination substances in ULCM",
          warn_only=True)

    ul = read_ulcm(RAW / "ulcm.xlsx", verbose=False)
    keys = [normalise(v).key for v in ul["substance"].astype(str)]
    empty = sum(1 for k in keys if not k)
    check("ULCM parse", empty / max(len(keys), 1) < 0.02,
          f"{empty}/{len(keys)} ULCM rows normalised to an empty key")

    if "single_country" in full:
        sc = full[(full["single_country"] == True) & (full["role"] == "api_cep")]
        print(f"\n{len(sc)} substances show a single API-holder country. ")
        print(sc[["ulcm_substance_raw", "top_country", "n_suppliers"]].head(10).to_string(index=False))

    print("\n" + ("QC FAILED: " + ", ".join(FAILS) if FAILS else "QC passed"))
    return 1 if FAILS else 0


if __name__ == "__main__":
    sys.exit(main())
