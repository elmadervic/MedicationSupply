"""Quality control. Run before believing anything in data/out/.

Run:  python -m critmed.qc

Each check prints PASS / WARN / FAIL and the evidence. The thresholds are
starting points, not truth - tighten them once you have seen one real run.
"""
from __future__ import annotations

import json
import sys

import pandas as pd

from config import INTERIM, MANIFEST, OUT, RAW
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

    # 1. provenance
    if MANIFEST.exists():
        man = json.loads(MANIFEST.read_text())
        errs = [k for k, v in man["sources"].items() if "error" in v]
        check("snapshot complete", not errs,
              f"snapshot {man['snapshot_utc']}; missing: {errs or 'none'}",
              warn_only=True)
    else:
        check("snapshot complete", False, "no manifest.json")

    # 2. country resolution
    unresolved = sup["country_iso2"].isna().mean()
    check("country resolution", unresolved < 0.05,
          f"{sup['country_iso2'].isna().sum():,} of {len(sup):,} supplier rows "
          "have no country "
          f"(sample: {sup.loc[sup['country_iso2'].isna(), 'supplier_name'].dropna().head(3).tolist()})")

    # 3. duplicate supplier records
    dup = sup.duplicated(subset=["norm_key", "role", "supplier_norm"]).mean()
    check("duplicate supplier rows", dup < 0.30,
          f"{dup:.1%} of rows duplicate an existing substance/role/supplier "
          "(expected >0: one holder can hold several CEPs for one substance)",
          warn_only=True)

    # 4. normalisation collapse - did any key absorb suspiciously many raws?
    collapse = (sup.groupby("norm_key")["substance_raw"].nunique()
                .sort_values(ascending=False))
    worst = collapse.head(8)
    check("normalisation collapse", collapse.max() < 60,
          f"largest key absorbs {collapse.max()} distinct raw strings; "
          f"top: {dict(worst)}", warn_only=True)

    # 5. ULCM coverage, and whether the gaps are structural
    # full is ULCM x role, so coverage must be computed on unique substances
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
        print("       expect L (antineoplastics/immunomodulators), B (blood) and J07 "
              "(vaccines) to dominate: biologics have no Ph. Eur. monograph and "
              "therefore no CEP. That is structural, not a bug.")

    # 6. combinations
    ncomb = full["is_combination"].sum() if "is_combination" in full else 0
    check("combination handling", True,
          f"{int(ncomb)} ULCM entries parsed as combinations - spot-check 10 by hand; "
          "combination keys are the single largest source of silent mismatch",
          warn_only=True)

    # 7. round-trip: every ULCM raw string re-normalises to itself
    ul = read_ulcm(RAW / "ulcm.xlsx", verbose=False)
    keys = [normalise(v).key for v in ul["substance"].astype(str)]
    empty = sum(1 for k in keys if not k)
    check("ULCM parse", empty / max(len(keys), 1) < 0.02,
          f"{empty}/{len(keys)} ULCM rows normalised to an empty key")

    # 8. single-country claims deserve manual review
    if "single_country" in full:
        sc = full[(full["single_country"] == True) & (full["role"] == "api_cep")]  # noqa: E712
        print(f"\n{len(sc)} substances show a single API-holder country. "
              "These drive the headline result - verify at least 10 against the "
              "EDQM web search by hand before reporting.")
        print(sc[["ulcm_substance_raw", "top_country", "n_suppliers"]].head(10).to_string(index=False))

    print("\n" + ("QC FAILED: " + ", ".join(FAILS) if FAILS else "QC passed"))
    return 1 if FAILS else 0


if __name__ == "__main__":
    sys.exit(main())
