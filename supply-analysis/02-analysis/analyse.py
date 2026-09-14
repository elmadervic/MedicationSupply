"""
Analyze supply data and produce summary tables and figures.
"""
from __future__ import annotations

import sys

import pandas as pd

# The shared modules live in supply-analysis/common -- put that directory on
# the import path so this script can still be run directly, from any working
# directory, exactly as the README describes.
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "common"))

from config import INTERIM, OUT, RAW
from countries import is_eu_eea
from normalize import normalise
from ulcm import read_ulcm

ROLES = ["api_cep", "bio_api", "batch_release", "mah_national"]


def load_ulcm() -> pd.DataFrame:
    path = RAW / "ulcm.xlsx"
    if not path.exists():
        raise SystemExit("ulcm.xlsx missing - run python fetch.py")
    ul = read_ulcm(path)

    out = pd.DataFrame({"ulcm_substance_raw": ul["substance"].astype(str),
                        "atc": ul["atc_code"].astype(str)})
    norm = [normalise(v) for v in out["ulcm_substance_raw"]]
    out["norm_key"] = [n.key for n in norm]
    out["is_combination"] = [n.is_combination for n in norm]
    out = out[out["norm_key"].astype(bool)]
    return (out.groupby("norm_key")
              .agg(ulcm_substance_raw=("ulcm_substance_raw", "first"),
                   atc_codes=("atc", lambda s: "|".join(sorted(set(x for x in s if x and x != "nan")))),
                   ulcm_rows=("atc", "size"),
                   is_combination=("is_combination", "first"))
              .reset_index())


def hhi(shares: pd.Series) -> float:
    p = shares / shares.sum()
    return float((p.pow(2).sum()) * 10000)


def per_substance(sup: pd.DataFrame, ulcm: pd.DataFrame) -> pd.DataFrame:
    sup = sup[sup["country_iso2"].notna()].copy()
    rows = []
    for (key, role), grp in sup.groupby(["norm_key", "role"]):
        counts = grp.groupby("country_iso2")["supplier_norm"].nunique()
        if counts.empty:
            continue
        top = counts.idxmax()
        rows.append({
            "norm_key": key,
            "role": role,
            "n_suppliers": int(counts.sum()),
            "n_countries": int((counts > 0).sum()),
            "top_country": top,
            "top_country_share": float(counts.max() / counts.sum()),
            "hhi_country": round(hhi(counts), 1),
            "single_country": bool((counts > 0).sum() == 1),
            "eu_eea_share": float(counts[[is_eu_eea(c) for c in counts.index]].sum() / counts.sum()),
            "countries": "|".join(sorted(counts.index)),
        })
    res = pd.DataFrame(rows)
    full = ulcm.merge(res, on="norm_key", how="left")
    return full


def main() -> int:
    src = INTERIM / "supply_long.csv"
    if not src.exists():
        raise SystemExit("supply_long.csv missing - run python build_supply.py")
    sup = pd.read_csv(src, dtype=str)
    ulcm = load_ulcm()
    print(f"  ULCM substances after normalisation: {len(ulcm):,}")

    full = per_substance(sup, ulcm)
    full.to_csv(OUT / "substance_supply.csv", index=False)

    in_scope = sup[sup["norm_key"].isin(set(ulcm["norm_key"])) & sup["country_iso2"].notna()]
    ct = (in_scope.groupby(["country_iso2", "role"])
          .agg(n_substances=("norm_key", "nunique"),
               n_suppliers=("supplier_norm", "nunique"))
          .reset_index()
          .sort_values(["role", "n_substances"], ascending=[True, False]))
    ct["eu_eea"] = ct["country_iso2"].map(is_eu_eea)
    ct.to_csv(OUT / "country_totals.csv", index=False)

    dist = (full.dropna(subset=["n_countries"])
            .assign(n_countries=lambda d: d["n_countries"].astype(int))
            .groupby(["role", "n_countries"]).size()
            .rename("n_substances").reset_index())
    dist.to_csv(OUT / "distribution.csv", index=False)

    sens = []
    for label, subset in [("all_statuses", sup),
                          ("valid_only", sup[sup["status"].astype(str).str.lower().str.contains("valid", na=False)])]:
        f = per_substance(subset, ulcm)
        api = f[f["role"] == "api_cep"]
        sens.append({
            "variant": label,
            "rows": len(subset),
            "substances_with_api_data": int(api["n_countries"].notna().sum()),
            "median_countries": float(api["n_countries"].median()),
            "pct_single_country": float(api["single_country"].mean() * 100) if len(api) else float("nan"),
            "median_hhi": float(api["hhi_country"].median()),
        })
    pd.DataFrame(sens).to_csv(OUT / "sensitivity.csv", index=False)

    lines = [f"ULCM substances (normalised, deduplicated): {len(ulcm):,}"]
    for role in ROLES:
        r = full[full["role"] == role]
        if r.empty:
            lines.append(f"\n[{role}] no data")
            continue
        lines += [
            f"\n[{role}]",
            f"  substances covered          {len(r):,} ({len(r)/len(ulcm):.1%} of ULCM)",
            f"  median countries/substance  {r['n_countries'].median():.0f}",
            f"  single-country substances   {int(r['single_country'].sum()):,} ({r['single_country'].mean():.1%})",
            f"  median HHI (country)        {r['hhi_country'].median():,.0f}",
            f"  HHI > 2500 (concentrated)   {(r['hhi_country'] > 2500).mean():.1%}",
            f"  mean EU/EEA share           {r['eu_eea_share'].mean():.1%}",
        ]
        top = ct[ct["role"] == role].head(10)
        lines.append("  top countries by substances supplied:")
        for _, row in top.iterrows():
            lines.append(f"    {row['country_iso2']}  {int(row['n_substances']):>4d}")
    uncovered = full[full["role"].isna()]
    lines.append(f"\nULCM substances with NO supply data in any source: {len(uncovered):,}")

    text = "\n".join(lines)
    (OUT / "summary.txt").write_text(text, encoding="utf-8")
    print("\n" + text)
    return 0


if __name__ == "__main__":
    sys.exit(main())
