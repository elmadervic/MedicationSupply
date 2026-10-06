"""
Analyze supply data and produce summary tables and figures.
"""
from __future__ import annotations

import sys

import pandas as pd

from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "common"))

from atc import level1
from config import COUNTRY_ROLES, INTERIM, OUT, RAW, ROLES
from countries import is_eu_eea
from normalize import normalise
from ulcm import read_ulcm


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


def explode_atc(sup: pd.DataFrame) -> pd.DataFrame:
    out = sup.copy()
    out["atc_codes"] = out["atc_codes"].fillna("")
    out = out[out["atc_codes"].astype(bool)].copy()
    out["atc_code"] = out["atc_codes"].str.split("|")
    out = out.explode("atc_code")
    out = out[out["atc_code"].astype(bool)]
    out["atc_level1"] = out["atc_code"].map(level1)
    return out


def _country_stats(grp: pd.DataFrame) -> dict:
    known = grp[grp["country_iso2"].notna()]
    if known.empty:
        return {"n_countries": pd.NA, "top_country": pd.NA,
                "top_country_share": pd.NA, "hhi_country": pd.NA,
                "eu_eea_share": pd.NA}
    counts = known.groupby("country_iso2")["supplier_norm"].nunique()
    return {
        "n_countries": int((counts > 0).sum()),
        "top_country": counts.idxmax(),
        "top_country_share": round(float(counts.max() / counts.sum()), 4),
        "hhi_country": round(hhi(counts), 1),
        "eu_eea_share": round(float(counts[[is_eu_eea(c) for c in counts.index]].sum() / counts.sum()), 4),
    }


def per_atc(sup_atc: pd.DataFrame, ulcm_keys: set[str], level: str = "atc_code") -> pd.DataFrame:
    rows = []
    for (code, role), grp in sup_atc.groupby([level, "role"]):
        row = {level: code}
        if level != "atc_level1":
            row["atc_level1"] = level1(code)
        row.update({
            "role": role,
            "n_substances": grp["norm_key"].nunique(),
            "n_suppliers": grp["supplier_norm"].nunique(),
            "n_ulcm_substances": grp.loc[grp["norm_key"].isin(ulcm_keys), "norm_key"].nunique(),
            **_country_stats(grp),
        })
        rows.append(row)
    return (pd.DataFrame(rows)
            .sort_values([level, "role"])
            .reset_index(drop=True))


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

    ulcm_keys = set(ulcm["norm_key"])
    sup_atc = explode_atc(sup)
    per_atc(sup_atc, ulcm_keys, "atc_code").to_csv(OUT / "atc_supply.csv", index=False)
    chapters = per_atc(sup_atc, ulcm_keys, "atc_level1")
    chapters.to_csv(OUT / "atc_chapter_totals.csv", index=False)

    lines = [f"ULCM substances (normalised, deduplicated): {len(ulcm):,}"]

    have = sup["atc_codes"].fillna("").astype(bool)
    lines.append(f"\n[ATC coverage] {have.sum():,} of {len(sup):,} supply rows "
                 f"carry an ATC code ({have.mean():.1%}); "
                 f"{sup_atc['atc_code'].nunique():,} distinct codes")
    for source, grp in sup.assign(has=have).groupby("source"):
        lines.append(f"    {source:<10} {int(grp['has'].sum()):>6,} / {len(grp):>6,}  "
                     f"{grp['has'].mean():>6.1%}")
    for role in ROLES:
        r = full[full["role"] == role]
        if r.empty:
            n_rows = int((sup["role"] == role).sum())
            if role not in COUNTRY_ROLES and n_rows:
                subs = sup.loc[sup["role"] == role, "norm_key"]
                lines.append(
                    f"\n[{role}] {n_rows:,} rows covering {subs.nunique():,} substances "
                    f"({subs.isin(ulcm_keys).sum():,} rows on ULCM substances) - "
                    "no country published for this source, so it is excluded "
                    "from the concentration measures above")
            else:
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
    api_chapters = (chapters[chapters["role"] == "api_cep"]
                    .dropna(subset=["hhi_country"])
                    .sort_values("hhi_country", ascending=False))
    if len(api_chapters):
        lines.append("\n[api_cep by ATC chapter] most country-concentrated first")
        lines.append(f"    {'ch':<3} {'subst':>6} {'suppl':>6} {'ctry':>5} {'HHI':>7} {'top':>4} {'EU/EEA':>7}")
        for _, row in api_chapters.iterrows():
            lines.append(f"    {row['atc_level1']:<3} {int(row['n_substances']):>6,} "
                         f"{int(row['n_suppliers']):>6,} {int(row['n_countries']):>5} "
                         f"{row['hhi_country']:>7,.0f} {str(row['top_country']):>4} "
                         f"{row['eu_eea_share']:>6.0%}")

    uncovered = full[full["role"].isna()]
    lines.append(f"\nULCM substances with NO supply data in any source: {len(uncovered):,}")

    text = "\n".join(lines)
    (OUT / "summary.txt").write_text(text, encoding="utf-8")
    print("\n" + text)
    return 0 


if __name__ == "__main__":
    sys.exit(main())
