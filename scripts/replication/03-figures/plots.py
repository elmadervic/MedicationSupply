"""
Plotting functions for supply analysis.
"""
from __future__ import annotations

import sys
from pathlib import Path

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.lines import Line2D
from matplotlib.patches import Patch

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "common"))

from config import COUNTRY_ROLES, INTERIM, OUT
from countries import is_eu_eea

FIGURES = OUT / "figures"

SURFACE = "#fcfcfb"
INK = "#0b0b0b"
INK_2 = "#52514e"
MUTED = "#898781"
GRID = "#e1e0d9"
AXIS = "#c3c2b7"

BLUE = "#2a78d6"
ORANGE = "#eb6834"
AQUA = "#1baf7a"
YELLOW = "#eda100"
CRITICAL = "#d03b3b"

BLUE_RAMP = ["#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b"]

ROLE_COLOR = {"api_cep": BLUE, "bio_api": ORANGE, "batch_release": AQUA}
ROLE_LABEL = {
    "api_cep": "API (CEP)",
    "bio_api": "Biological API",
    "batch_release": "Batch release",
    "mah_national": "National MA holder",
}

GROUP_COLOR = {"EU/EEA": BLUE, "China": ORANGE, "India": AQUA, "Other": MUTED}
GROUP_ORDER = ["EU/EEA", "China", "India", "Other"]

REGION_COLOR = {"EU/EEA": BLUE, "Non-EEA": ORANGE}
REGION_ORDER = ["EU/EEA", "Non-EEA"]

ATC_CHAPTERS = {
    "A": "Alimentary tract & metabolism",
    "B": "Blood & blood forming organs",
    "C": "Cardiovascular system",
    "D": "Dermatologicals",
    "G": "Genito-urinary & sex hormones",
    "H": "Systemic hormonal preparations",
    "J": "Antiinfectives (systemic)",
    "L": "Antineoplastic & immunomodulating",
    "M": "Musculo-skeletal system",
    "N": "Nervous system",
    "P": "Antiparasitic products",
    "R": "Respiratory system",
    "S": "Sensory organs",
    "V": "Various",
}


def style() -> None:
    plt.rcParams.update({
        "figure.facecolor": SURFACE,
        "axes.facecolor": SURFACE,
        "savefig.facecolor": SURFACE,
        "font.family": "sans-serif",
        "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
        "font.size": 9,
        "text.color": INK,
        "axes.labelcolor": INK_2,
        "axes.edgecolor": AXIS,
        "axes.linewidth": 0.8,
        "axes.titlesize": 10,
        "axes.titleweight": "bold",
        "axes.titlecolor": INK,
        "axes.grid": True,
        "axes.axisbelow": True,
        "grid.color": GRID,
        "grid.linewidth": 0.8,
        "xtick.color": MUTED,
        "ytick.color": MUTED,
        "xtick.labelcolor": INK_2,
        "ytick.labelcolor": INK_2,
        "xtick.major.size": 0,
        "ytick.major.size": 0,
        "legend.frameon": False,
        "legend.fontsize": 8.5,
        "figure.dpi": 150,
    })


def despine(ax, keep=("left", "bottom")) -> None:
    for side in ("top", "right", "left", "bottom"):
        ax.spines[side].set_visible(side in keep)


def chapter_label(letter: str) -> str:
    name = ATC_CHAPTERS.get(letter, "")
    return f"{letter}  {name}" if name else letter


def titles(fig, title: str, subtitle: str, bottom: float = 0.04) -> None:
    fig.tight_layout(rect=[0, bottom, 1, 0.92])
    fig.suptitle(title, fontsize=13, fontweight="bold", y=1.005)
    fig.text(0.5, 0.936, subtitle, ha="center", fontsize=9, color=INK_2)


def save(fig, name: str) -> None:
    FIGURES.mkdir(parents=True, exist_ok=True)
    path = FIGURES / name
    fig.savefig(path, bbox_inches="tight", dpi=200)
    plt.close(fig)
    print(f"  wrote {path.relative_to(OUT.parent.parent)}")


def ulcm_names() -> dict[str, str]:
    path = OUT / "substance_supply.csv"
    if not path.exists():
        return {}
    df = pd.read_csv(path, dtype=str).drop_duplicates("norm_key")
    return dict(zip(df["norm_key"], df["ulcm_substance_raw"]))


def load() -> tuple[pd.DataFrame, pd.DataFrame]:
    src = INTERIM / "supply_long.csv"
    if not src.exists():
        raise SystemExit("supply_long.csv missing - run build_supply.py first")
    sup = pd.read_csv(src, dtype=str)

    sup["atc_codes"] = sup["atc_codes"].fillna("")
    coded = sup[sup["atc_codes"].astype(bool)].copy()
    coded["atc_code"] = coded["atc_codes"].str.split("|")
    coded = coded.explode("atc_code")
    coded = coded[coded["atc_code"].astype(bool)].copy()
    coded["chapter"] = coded["atc_code"].str[0]
    coded = coded[coded["chapter"].isin(ATC_CHAPTERS)]
    return sup, coded


def country_group(iso2: str) -> str:
    if iso2 == "CN":
        return "China"
    if iso2 == "IN":
        return "India"
    return "EU/EEA" if is_eu_eea(iso2) else "Other"


def _stacked_shares(df: pd.DataFrame, group_col: str, order: list[str]) -> pd.DataFrame:
    counts = (df.groupby(["chapter", group_col])["supplier_norm"]
              .nunique().unstack(fill_value=0))
    for col in order:
        if col not in counts:
            counts[col] = 0
    counts = counts[order]
    shares = counts.div(counts.sum(axis=1), axis=0) * 100
    return shares.loc[[c for c in sorted(shares.index, reverse=True)]]


def _stacked_panel(ax, shares, colors, title: str, show_ylabels: bool) -> None:
    ys = np.arange(len(shares))
    left = np.zeros(len(shares))
    for col in shares.columns:
        vals = shares[col].to_numpy()
        ax.barh(ys, vals, left=left, height=0.62, color=colors[col],
                edgecolor=SURFACE, linewidth=1.4, zorder=3)
        for y, v, l in zip(ys, vals, left):
            if v >= 9:
                ax.text(l + v / 2, y, f"{v:.0f}", ha="center", va="center",
                        fontsize=7.5, color="white", fontweight="bold", zorder=4)
        left += vals
    ax.set_yticks(ys)
    ax.set_yticklabels([chapter_label(c) for c in shares.index], fontsize=8)
    if not show_ylabels:
        ax.tick_params(labelleft=False)
    ax.set_xlim(0, 100)
    ax.set_xticks([0, 25, 50, 75, 100])
    ax.set_xticklabels(["0", "25", "50", "75", "100%"])
    ax.set_title(title, pad=8)
    ax.xaxis.grid(True)
    ax.yaxis.grid(False)
    despine(ax, keep=("bottom",))


def fig_country_group_by_chapter(coded: pd.DataFrame) -> None:
    df = coded[coded["role"].isin(COUNTRY_ROLES) & coded["country_iso2"].notna()].copy()
    df["group"] = df["country_iso2"].map(country_group)
    roles = [r for r in COUNTRY_ROLES if (df["role"] == r).any()]

    fig, axes = plt.subplots(1, len(roles), figsize=(5.2 * len(roles), 6.4), sharey=True)
    axes = np.atleast_1d(axes)
    for ax, role in zip(axes, roles):
        shares = _stacked_shares(df[df["role"] == role], "group", GROUP_ORDER)
        _stacked_panel(ax, shares, GROUP_COLOR, ROLE_LABEL[role], ax is axes[0])

    handles = [Patch(facecolor=GROUP_COLOR[g], label=g) for g in GROUP_ORDER]
    fig.legend(handles=handles, loc="lower center", ncol=4, bbox_to_anchor=(0.5, -0.02))
    titles(fig, "Where the suppliers sit, by ATC chapter",
           "share of distinct suppliers per chapter (%)", bottom=0.06)
    save(fig, "country_group_by_chapter.png")


def fig_eea_by_chapter(coded: pd.DataFrame) -> None:
    df = coded[coded["role"].isin(COUNTRY_ROLES) & coded["country_iso2"].notna()].copy()
    df["region"] = np.where([is_eu_eea(c) for c in df["country_iso2"]], "EU/EEA", "Non-EEA")
    roles = [r for r in COUNTRY_ROLES if (df["role"] == r).any()]

    fig, axes = plt.subplots(1, len(roles), figsize=(5.2 * len(roles), 6.4), sharey=True)
    axes = np.atleast_1d(axes)
    for ax, role in zip(axes, roles):
        shares = _stacked_shares(df[df["role"] == role], "region", REGION_ORDER)
        _stacked_panel(ax, shares, REGION_COLOR, ROLE_LABEL[role], ax is axes[0])

    handles = [Patch(facecolor=REGION_COLOR[r], label=r) for r in REGION_ORDER]
    fig.legend(handles=handles, loc="lower center", ncol=2, bbox_to_anchor=(0.5, -0.02))
    titles(fig, "EU/EEA versus non-EEA supply, by ATC chapter",
           "share of distinct suppliers per chapter (%)", bottom=0.06)
    save(fig, "eea_by_chapter.png")


def fig_hhi_by_chapter() -> None:
    path = OUT / "atc_chapter_totals.csv"
    if not path.exists():
        print("  atc_chapter_totals.csv missing - run analyse.py; skipping HHI figure")
        return
    ch = pd.read_csv(path)
    ch = ch[ch["role"].isin(COUNTRY_ROLES) & ch["hhi_country"].notna()]
    ch = ch[ch["atc_level1"].isin(ATC_CHAPTERS)]
    if ch.empty:
        return

    roles = [r for r in COUNTRY_ROLES if (ch["role"] == r).any()]
    chapters = sorted(ch["atc_level1"].unique(), reverse=True)
    ys = np.arange(len(chapters))
    height = 0.8 / len(roles)

    fig, ax = plt.subplots(figsize=(9.5, 7.2))
    for i, role in enumerate(roles):
        sub = ch[ch["role"] == role].set_index("atc_level1")["hhi_country"]
        vals = [sub.get(c, np.nan) for c in chapters]
        offset = (i - (len(roles) - 1) / 2) * height
        ax.barh(ys + offset, vals, height=height * 0.88, color=ROLE_COLOR[role],
                label=ROLE_LABEL[role], zorder=3)

    ax.axvline(2500, color=CRITICAL, linewidth=1.2, linestyle=(0, (4, 3)), zorder=4)
    ax.text(2500, len(chapters) - 0.35, "  2500 = concentrated",
            color=CRITICAL, fontsize=8, va="bottom")
    ax.set_yticks(ys)
    ax.set_yticklabels([chapter_label(c) for c in chapters], fontsize=8)
    ax.set_xlabel("HHI of supplier countries")
    ax.set_title("Country concentration by ATC chapter", pad=10)
    ax.xaxis.grid(True)
    ax.yaxis.grid(False)
    despine(ax, keep=("bottom",))
    ax.legend(loc="lower right")
    fig.tight_layout()
    save(fig, "hhi_by_chapter.png")


def fig_api_diversification(coded: pd.DataFrame, threshold: float = 60.0,
                            few: int = 3, top: int = 18) -> None:
    df = coded[(coded["role"] == "api_cep") & coded["country_iso2"].notna()]
    names = ulcm_names()
    if names:
        df = df[df["norm_key"].isin(names)]
    if df.empty:
        return
    grp = df.groupby("norm_key")
    stats = pd.DataFrame({
        "n_suppliers": grp["supplier_norm"].nunique(),
        "n_countries": grp["country_iso2"].nunique(),
    })
    cn_in = (df[df["country_iso2"].isin(["CN", "IN"])]
             .groupby("norm_key")["supplier_norm"].nunique())
    stats["pct_china_india"] = (cn_in.reindex(stats.index).fillna(0)
                                / stats["n_suppliers"] * 100)
    stats = stats[stats["n_suppliers"] > 0]
    hot = (stats["pct_china_india"] >= threshold) & (stats["n_suppliers"] <= few)

    fig, (ax, ax2) = plt.subplots(
        1, 2, figsize=(14.5, 7), gridspec_kw={"width_ratios": [1.55, 1]})

    ax.scatter(stats.loc[~hot, "n_suppliers"], stats.loc[~hot, "pct_china_india"],
               s=26, color=BLUE, alpha=0.7, linewidths=0.8, edgecolors=SURFACE,
               zorder=3, label="other substances")
    ax.scatter(stats.loc[hot, "n_suppliers"], stats.loc[hot, "pct_china_india"],
               s=40, color=CRITICAL, alpha=0.95, linewidths=0.8, edgecolors=SURFACE,
               zorder=4, label=f"<={few} suppliers and >={threshold:.0f}% CN+IN")
    ax.axhline(threshold, color=MUTED, linewidth=0.9, linestyle=(0, (4, 3)), zorder=2)
    ax.set_xlabel("distinct API suppliers holding a CEP")
    ax.set_ylabel("share of those suppliers in China or India (%)")
    ax.set_ylim(-4, 106)
    ax.set_title(f"{len(stats)} critical substances with CEP data", pad=8)
    ax.xaxis.grid(False)
    despine(ax)
    ax.legend(loc="lower right")

    flagged = (stats[hot].sort_values(["n_suppliers", "pct_china_india"],
                                      ascending=[True, False]).head(top))
    ys = np.arange(len(flagged))[::-1]
    ax2.barh(ys, flagged["n_suppliers"].to_numpy(), height=0.62,
             color=CRITICAL, zorder=3)
    for y, (key, row) in zip(ys, flagged.iterrows()):
        ax2.text(row["n_suppliers"] + 0.06, y,
                 f"  {row['n_suppliers']:.0f} supplier"
                 f"{'s' if row['n_suppliers'] != 1 else ''}"
                 f" · {row['pct_china_india']:.0f}% CN+IN",
                 va="center", fontsize=7.5, color=INK_2)
    ax2.set_yticks(ys)
    ax2.set_yticklabels([names.get(k, k)[:34].title() for k in flagged.index], fontsize=8)
    ax2.set_xlim(0, max(flagged["n_suppliers"].max(), 1) * 2.9)
    ax2.set_xticks([])
    ax2.set_title(f"Most exposed: {int(hot.sum())} substances flagged", pad=8)
    ax2.grid(False)
    despine(ax2, keep=())

    titles(fig, "API diversification versus China/India dependence",
           "one point per Union-list critical substance, API suppliers holding an EDQM certificate")
    save(fig, "api_diversification.png")


def fig_bubble_matrix(coded: pd.DataFrame, top_n: int = 24) -> None:
    df = coded[(coded["role"] == "api_cep") & coded["country_iso2"].notna()]
    if df.empty:
        return
    agg = (df.groupby(["country_iso2", "chapter"])
           .agg(suppliers=("supplier_norm", "nunique"),
                substances=("norm_key", "nunique"))
           .reset_index())
    top = (df.groupby("country_iso2")["supplier_norm"].nunique()
           .sort_values(ascending=False).head(top_n).index)
    agg = agg[agg["country_iso2"].isin(top)]

    countries = list(df[df["country_iso2"].isin(top)]
                     .groupby("country_iso2")["supplier_norm"].nunique()
                     .sort_values().index)
    chapters = sorted(agg["chapter"].unique())
    xi = {c: i for i, c in enumerate(chapters)}
    yi = {c: i for i, c in enumerate(countries)}

    cmap = matplotlib.colors.LinearSegmentedColormap.from_list("blues", BLUE_RAMP)
    smax = max(agg["substances"].max(), 1)

    fig, ax = plt.subplots(figsize=(9.5, 9))
    sizes = agg["suppliers"] / agg["suppliers"].max() * 420 + 18
    sc = ax.scatter([xi[c] for c in agg["chapter"]], [yi[c] for c in agg["country_iso2"]],
                    s=sizes, c=agg["substances"], cmap=cmap, vmin=0, vmax=smax,
                    linewidths=1.0, edgecolors=SURFACE, zorder=3)

    ax.set_xticks(range(len(chapters)))
    ax.set_xticklabels(chapters, fontsize=9)
    ax.set_yticks(range(len(countries)))
    ax.set_yticklabels(countries, fontsize=8.5)
    ax.set_xlim(-0.7, len(chapters) - 0.3)
    ax.set_ylim(-0.7, len(countries) - 0.3)
    ax.set_xlabel("ATC anatomical main group")
    ax.set_title("API suppliers by country and ATC chapter", pad=10)
    ax.grid(True, which="major", color=GRID, linewidth=0.6)
    despine(ax, keep=())

    cb = fig.colorbar(sc, ax=ax, pad=0.02, fraction=0.035)
    cb.set_label("distinct substances", fontsize=8.5, color=INK_2)
    cb.outline.set_visible(False)
    cb.ax.tick_params(labelsize=8, color=MUTED, labelcolor=INK_2)

    ref = [1, max(agg["suppliers"].max() // 2, 2), agg["suppliers"].max()]
    handles = [Line2D([], [], marker="o", linestyle="none", markersize=np.sqrt(
        v / agg["suppliers"].max() * 420 + 18), markerfacecolor=MUTED,
        markeredgecolor=SURFACE, label=str(v)) for v in ref]
    ax.legend(handles=handles, title="suppliers", loc="upper left",
              bbox_to_anchor=(1.12, 1.0), labelspacing=1.4, title_fontsize=8.5)
    fig.tight_layout()
    save(fig, "bubble_matrix.png")


def fig_by_source(sup: pd.DataFrame, coded: pd.DataFrame) -> None:
    fig, axes = plt.subplots(1, 3, figsize=(14.5, 4.4))

    ax = axes[0]
    rows = sup.groupby("source").size().sort_values()
    ax.barh(range(len(rows)), rows.to_numpy(), height=0.45, color=BLUE, zorder=3)
    ax.set_yticks(range(len(rows)))
    ax.set_yticklabels(rows.index, fontsize=9)
    for i, v in enumerate(rows):
        ax.text(v, i, f" {v:,}", va="center", fontsize=8, color=INK_2)
    ax.set_title("Rows per source", pad=8)
    ax.set_xlim(0, rows.max() * 1.18)
    ax.xaxis.grid(True)
    ax.yaxis.grid(False)
    despine(ax, keep=("bottom",))

    ax = axes[1]
    subs = sup.groupby("source")["norm_key"].nunique().sort_values()
    ax.barh(range(len(subs)), subs.to_numpy(), height=0.45, color=ORANGE, zorder=3)
    ax.set_yticks(range(len(subs)))
    ax.set_yticklabels(subs.index, fontsize=9)
    for i, v in enumerate(subs):
        ax.text(v, i, f" {v:,}", va="center", fontsize=8, color=INK_2)
    ax.set_title("Distinct substances per source", pad=8)
    ax.set_xlim(0, subs.max() * 1.18)
    ax.xaxis.grid(True)
    ax.yaxis.grid(False)
    despine(ax, keep=("bottom",))

    ax = axes[2]
    cov = (sup.assign(has=sup["atc_codes"].fillna("").astype(bool))
           .groupby("source")["has"].mean().sort_values() * 100)
    ax.barh(range(len(cov)), cov.to_numpy(), height=0.45, color=AQUA, zorder=3)
    ax.set_yticks(range(len(cov)))
    ax.set_yticklabels(cov.index, fontsize=9)
    for i, v in enumerate(cov):
        ax.text(v, i, f" {v:.0f}%", va="center", fontsize=8, color=INK_2)
    ax.set_title("Rows carrying an ATC code", pad=8)
    ax.set_xlim(0, 118)
    ax.set_xticks([0, 25, 50, 75, 100])
    ax.xaxis.grid(True)
    ax.yaxis.grid(False)
    despine(ax, keep=("bottom",))

    titles(fig, "Supply table by source",
           "what each raw source contributes to supply_long.csv", bottom=0.02)
    save(fig, "supply_by_source.png")


def main() -> int:
    style()
    sup, coded = load()
    print(f"  {len(sup):,} supply rows, {len(coded):,} rows x ATC code")

    fig_by_source(sup, coded)
    fig_hhi_by_chapter()
    fig_country_group_by_chapter(coded)
    fig_eea_by_chapter(coded)
    fig_api_diversification(coded)
    fig_bubble_matrix(coded)
    return 0


if __name__ == "__main__":
    sys.exit(main())
