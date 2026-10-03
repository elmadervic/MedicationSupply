"""
Compare the replication pipeline's per-ATC-code concentration with the
register analysis (results/tables/hhi_step1_priority.csv), for the two
sources both pipelines cover: EDQM CEP and EMA EPAR.
"""
import sys
from pathlib import Path

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "common"))

from config import OUT, ROOT

REGISTERS_HHI = ROOT / "results" / "tables" / "hhi_step1_priority.csv"
CRITICAL = ROOT / "data" / "registers" / "raw" / "critical.csv"

PAIRS = {
    "CEP": ["api_cep"],
    "EPAR": ["bio_api", "batch_release"],
}


def spearman(x: pd.Series, y: pd.Series) -> float:
    return x.rank().corr(y.rank())


def replication_by_code(atc: pd.DataFrame, roles: list[str], universe: set[str]) -> pd.DataFrame:
    """One row per ATC-5 code; API roles take priority over batch release, as in the register analysis."""
    sub = atc[atc.role.isin(roles) & atc.atc_code.isin(universe) & atc.hhi_country.notna()]
    order = {r: i for i, r in enumerate(roles)}
    sub = sub.assign(_o=sub.role.map(order)).sort_values("_o").drop_duplicates("atc_code")
    return sub.set_index("atc_code")[["n_countries", "hhi_country"]]


def main() -> None:
    reg = pd.read_csv(REGISTERS_HHI)
    universe = set(pd.read_csv(CRITICAL)["ATC level 5"])
    atc = pd.read_csv(OUT / "atc_supply.csv")
    atc = atc[atc.atc_code.str.len() == 7]

    rows = []
    for source, roles in PAIRS.items():
        r = reg[reg.source == source].set_index("atc_code")
        p = replication_by_code(atc, roles, universe)
        j = r.join(p, how="inner", rsuffix="_rep")
        rows.append({
            "source": source,
            "codes_registers": len(r),
            "codes_replication": len(p),
            "codes_both": len(j),
            "spearman_hhi": round(spearman(j.hhi, j.hhi_country), 2),
            "spearman_n_countries": round(spearman(j.n_countries, j.n_countries_rep), 2),
            "share_same_n_countries": round((j.n_countries == j.n_countries_rep).mean(), 2),
            "median_hhi_registers": round(j.hhi.median()),
            "median_hhi_replication": round(j.hhi_country.median()),
            "single_country_registers": round((j.hhi == 10000).mean(), 2),
            "single_country_replication": round((j.hhi_country == 10000).mean(), 2),
        })

    out = pd.DataFrame(rows)
    out.to_csv(OUT / "comparison_with_registers.csv", index=False)
    print(out.to_string(index=False))


if __name__ == "__main__":
    main()
