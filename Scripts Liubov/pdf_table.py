"""Turn a table-bearing PDF into a DataFrame.

Companion to the PDF branch of inspect_sources: that tells you which layout
you have, this reads it.

    from critmed.pdf_table import read_pdf_tables
    df = read_pdf_tables(RAW / "bfarm_versorgungsrelevant.pdf")

The BfArM substance list runs 27 pages and prints its header once, on page 1.
Treating every page's first row as a header eats one substance per page and
yields 27 differently-named frames, so the header is captured once and carried.
"""
from __future__ import annotations

import re

import pandas as pd

RULED = {}  # pdfplumber defaults: find cells from ruling lines
TEXT_ALIGNED = {"vertical_strategy": "text", "horizontal_strategy": "text"}

_WS = re.compile(r"\s+")


def _clean(cell) -> str:
    return "" if cell is None else " ".join(str(cell).split())


def _key(row) -> str:
    return "|".join(_WS.sub(" ", str(c).strip().lower()) for c in row)


def _tidy_rows(tbl) -> tuple[list[list[str]], int]:
    """Keep only rows of the modal width; report how many were discarded.

    Merged and wrapped cells produce short or long rows. Zipping those into a
    DataFrame shifts every value one column left and the error is silent, so
    drop them and report the loss rather than absorb it.
    """
    rows = [[_clean(c) for c in r] for r in tbl if r]
    if not rows:
        return [], 0
    widths = pd.Series([len(r) for r in rows])
    modal = int(widths.mode().iloc[0])
    kept = [r for r in rows if len(r) == modal]
    dropped = len(rows) - len(kept)
    # the text-aligned strategy emits a cell per whitespace gap, so much of
    # what it returns is blank filler
    kept = [r for r in kept if any(c for c in r)]
    return kept, dropped


def _looks_like_header(row: list[str]) -> bool:
    """A header row is fully populated and carries no digit-only cells."""
    if not row or any(not c for c in row):
        return False
    return not any(c.replace(".", "").isdigit() for c in row)


def read_pdf_tables(path, pages=None, strategy: str = "auto",
                    header: list[str] | None = None) -> pd.DataFrame:
    """Concatenate every table in the PDF into one DataFrame.

    strategy: "ruled" | "text" | "auto" (try ruled, fall back to text).
    header:   explicit column names, skipping header detection entirely.

    Adds a `_page` column and reports rows dropped as ragged or as repeated
    headers.
    """
    import pdfplumber

    settings_order = {"ruled": [RULED], "text": [TEXT_ALIGNED],
                      "auto": [RULED, TEXT_ALIGNED]}[strategy]

    with pdfplumber.open(path) as pdf:
        page_list = pdf.pages if pages is None else [pdf.pages[i] for i in pages]
        for settings in settings_order:
            cols = list(header) if header else None
            hdr_key = _key(cols) if cols else None
            frames, dropped, repeats = [], 0, 0

            for pno, page in enumerate(page_list, start=1):
                for tbl in page.extract_tables(table_settings=settings) or []:
                    rows, drop = _tidy_rows(tbl)
                    dropped += drop
                    if not rows:
                        continue
                    if cols is None:
                        # the header is printed once, on the first table
                        if not _looks_like_header(rows[0]):
                            continue
                        cols, hdr_key = rows[0], _key(rows[0])
                        rows = rows[1:]
                    elif _key(rows[0]) == hdr_key:
                        rows, repeats = rows[1:], repeats + 1
                    body = [r for r in rows if len(r) == len(cols)]
                    dropped += len(rows) - len(body)
                    if not body:
                        continue
                    df = pd.DataFrame(body, columns=cols)
                    df["_page"] = pno
                    frames.append(df)

            if frames:
                out = pd.concat(frames, ignore_index=True)
                label = "ruled" if settings is RULED else "text-aligned"
                print(f"  [{path.name}: {label} strategy -> {out.shape}, "
                      f"header={cols}, {dropped} ragged rows dropped, "
                      f"{repeats} repeated headers removed]")
                return out.reset_index(drop=True)

    raise ValueError(
        f"no tables found in {path}. Check inspect_sources output: if there is "
        "no text layer the file is a scan and needs OCR. If it has a text "
        "layer but no header row, pass header=[...] explicitly."
    )
