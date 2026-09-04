"""Read the Union list of critical medicines workbook.

The sheet is not a flat table. Its real shape, confirmed against v2.1 rev 1:

  rows 0..n     document metadata (reference number, title, publication date)
  header row    contains 'Route of administration' and 'Date of inclusion'
  body          ATC group heading rows ("A - Alimentary tract and metabolism",
                "B03B - Vitamin B12 and folic acid") interleaved with substance
                rows ("B03BA03 | HYDROXOCOBALAMIN | oral | 2023-12-01")
  columns       ~16,000 entirely empty columns trailing to the right

Group rows and substance rows are distinguished by whether the ATC cell is a
full level-5 code. Counting group rows as substances inflates the denominator
by roughly half, so this is not cosmetic.
"""
from __future__ import annotations

import re

import pandas as pd

# ATC level 5, e.g. B03BA03. Group rows carry shorter codes or none at all.
ATC5 = re.compile(r"^[A-Z]\d{2}[A-Z]{2}\d{2}$")
HEADER_MARKERS = ("route of administration", "date of inclusion")


def _norm(v) -> str:
    return "" if pd.isna(v) else " ".join(str(v).split())


def read_ulcm(path, sheet=0, verbose: bool = True) -> pd.DataFrame:
    """Return one row per ULCM entry: atc_code, substance, route, date_included."""
    raw = pd.read_excel(path, sheet_name=sheet, header=None)
    raw = raw.dropna(axis=1, how="all")
    if verbose:
        print(f"  ULCM raw sheet: {raw.shape[0]} rows x {raw.shape[1]} non-empty cols")

    # locate the header row by content, not position
    header_idx = None
    for i in range(min(40, len(raw))):
        cells = " | ".join(_norm(v).lower() for v in raw.iloc[i])
        if all(m in cells for m in HEADER_MARKERS):
            header_idx = i
            break
    if header_idx is None:
        raise ValueError(
            "no header row containing 'Route of administration' and "
            "'Date of inclusion' in the first 40 rows - open the sheet and "
            "check whether EMA changed the layout"
        )
    header = [_norm(v) for v in raw.iloc[header_idx]]
    if verbose:
        print(f"  header row {header_idx}: {header}")

    body = raw.iloc[header_idx + 1:].reset_index(drop=True)
    body.columns = range(body.shape[1])

    # identify columns by header text where possible, by position otherwise
    def _find(*needles, default=None):
        for j, h in enumerate(header):
            if any(n in h.lower() for n in needles):
                return j
        return default

    c_route = _find("route of administration")
    c_date = _find("date of inclusion")
    # ATC code and substance sit left of route; the ATC column is the one whose
    # cells actually look like ATC codes.
    left = [j for j in range(body.shape[1]) if c_route is None or j < c_route]
    scores = {j: body[j].map(lambda v: bool(ATC5.match(_norm(v)))).mean() for j in left}
    c_atc = max(scores, key=scores.get) if scores else 0
    c_sub = next((j for j in left if j != c_atc), None)
    if c_sub is None:
        raise ValueError(f"could not identify a substance column; header={header}")
    if verbose:
        print(f"  columns -> atc={c_atc} substance={c_sub} "
              f"route={c_route} date={c_date} "
              f"(ATC-shaped share in col {c_atc}: {scores[c_atc]:.0%})")

    out = pd.DataFrame({
        "atc_code": body[c_atc].map(_norm),
        "substance": body[c_sub].map(_norm),
        "route": body[c_route].map(_norm) if c_route is not None else "",
        "date_included": body[c_date] if c_date is not None else pd.NaT,
    })

    is_substance = out["atc_code"].str.match(ATC5) & out["substance"].astype(bool)
    dropped = (~is_substance).sum()
    out = out[is_substance].reset_index(drop=True)
    if verbose:
        print(f"  {len(out)} substance rows kept, {dropped} group/blank rows dropped")
        if len(out):
            print(f"  first rows:\n{out.head(3).to_string(index=False)}")
    return out
