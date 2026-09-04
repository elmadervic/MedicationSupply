"""Country counts and concentration statistics for the Union list substances.

Run:  python -m critmed.analyse

Outputs (data/out/):
  substance_supply.csv     one row per ULCM substance x role
  country_totals.csv       one row per country
  distribution.csv         how many substances are supplied from exactly k countries
  sensitivity.csv          the same headline numbers with CEP status filtered
  summary.txt             the numbers to hand over for the double-check
"""
from __future__ import annotations

import sys

import pandas as pd

from config import INTERIM, OUT, RAW
from countries import is_eu_eea
from normalize import normalise
from ulcm import read_ulcm

ROLES = ["api_cep", "bio_api", "batch_release", "mah_national"]


def load_ulcm() -> pd.DataFrame:
    path = RAW / "ulcm.xlsx"
    if not path.exists():
        raise SystemExit("ulcm.xlsx missing - run python -m critmed.fetch")
    ul = read_ulcm(path)

    out = pd.DataFrame({"ulcm_substance_raw": ul["substance"].astype(str),
                        "atc": ul["atc_code"].astype(str)})
    norm = [normalise(v) for v in out["ulcm_substance_raw"]]
    out["norm_key"] = [n.key for n in norm]
    out["is_combination"] = [n.is_combination for n in norm]
    out = out[out["norm_key"].astype(bool)]
    # One substance appears on several rows: different ATC codes (acetylcysteine
    # is both R05CB01 and V03AB23) and different routes. Collapse to substance
    # level, keeping every ATC seen.
    return (out.groupby("norm_key")
              .agg(ulcm_substance_raw=("ulcm_substance_raw", "first"),
                   atc_codes=("atc", lambda s: "|".join(sorted(set(x for x in s if x and x != "nan")))),
                   ulcm_rows=("atc", "size"),
                   is_combination=("is_combination", "first"))
              .reset_index())


def hhi(shares: pd.Series) -> float:
    """Herfindahl-Hirschman index on 0-10000, the convention used in the
    2010 DOJ/FTC guidelines (1500 / 2500 thresholds)."""
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
    # left join keeps ULCM substances with zero coverage - that absence is a
    # finding, not a row to discard
    full = ulcm.merge(res, on="norm_key", how="left")
    return full


def main() -> int:
    src = INTERIM / "supply_long.csv"
    if not src.exists():
        raise SystemExit("supply_long.csv missing - run python -m critmed.build_supply")
    sup = pd.read_csv(src, dtype=str)
    ulcm = load_ulcm()
    print(f"  ULCM substances after normalisation: {len(ulcm):,}")

    full = per_substance(sup, ulcm)
    full.to_csv(OUT / "substance_supply.csv", index=False)

    # --- how many countries supply how many substances -----------------------
    in_scope = sup[sup["norm_key"].isin(set(ulcm["norm_key"])) & sup["country_iso2"].notna()]
    ct = (in_scope.groupby(["country_iso2", "role"])
          .agg(n_substances=("norm_key", "nunique"),
               n_suppliers=("supplier_norm", "nunique"))
          .reset_index()
          .sort_values(["role", "n_substances"], ascending=[True, False]))
    ct["eu_eea"] = ct["country_iso2"].map(is_eu_eea)
    ct.to_csv(OUT / "country_totals.csv", index=False)

    # --- distribution of country counts -------------------------------------
    dist = (full.dropna(subset=["n_countries"])
            .assign(n_countries=lambda d: d["n_countries"].astype(int))
            .groupby(["role", "n_countries"]).size()
            .rename("n_substances").reset_index())
    dist.to_csv(OUT / "distribution.csv", index=False)

    # --- sensitivity: valid CEPs only vs everything -------------------------
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

    # --- headline numbers ----------------------------------------------------
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
