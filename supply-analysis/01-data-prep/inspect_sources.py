"""
Analyze the raw source files in data/raw/ and print a summary of their contents.
"""
from __future__ import annotations

import json
import sys

import pandas as pd

# The shared modules live in supply-analysis/common -- put that directory on
# the import path so this script can still be run directly, from any working
# directory, exactly as the README describes.
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "common"))

from config import MANUAL_SOURCES, RAW


def _preview_frame(df: pd.DataFrame, name: str, note: str = "") -> None:
    print(f"\n=== {name} {note}")
    print(f"shape: {df.shape[0]:,} rows x {df.shape[1]} cols")
    print("columns:")
    for c in df.columns:
        nn = df[c].notna().sum()
        ex = df[c].dropna().astype(str).head(2).tolist()
        print(f"  - {str(c)[:60]:62s} non-null={nn:>7,d}  e.g. {ex}")


def _read_excel_guess_header(path, max_scan: int = 12) -> pd.DataFrame:
    best, best_score, best_h = None, -1, 0
    for h in range(max_scan):
        try:
            df = pd.read_excel(path, header=h)
        except Exception:
            continue
        df = df.dropna(axis=1, how="all").dropna(axis=0, how="all")
        if df.empty:
            continue
        named = sum(1 for c in df.columns if not str(c).lower().startswith("unnamed"))
        score = named / max(len(df.columns), 1) - 0.001 * h
        if score > best_score:
            best, best_score, best_h = df, score, h
    if best is None:
        raise ValueError(f"could not read {path}")
    print(f"    [header row guessed: {best_h}; {best.shape[1]} non-empty columns]")
    return best


def _preview_pdf(path, max_pages: int = 3, max_tables: int = 3) -> None:
    try:
        import pdfplumber
    except ImportError:
        print("  pdfplumber not installed - pip install pdfplumber")
        return

    with pdfplumber.open(path) as pdf:
        n = len(pdf.pages)
        print(f"  pages: {n}")
        meta = {k: v for k, v in (pdf.metadata or {}).items()
                if k in {"Title", "Author", "CreationDate", "ModDate", "Producer"}}
        if meta:
            print(f"  metadata: {meta}")

        probe = "".join((p.extract_text() or "") for p in pdf.pages[:min(3, n)])
        if not probe.strip():
            print("  NO TEXT LAYER on the first pages - this is a scan. "
                  "extract_tables() will return nothing; rasterise and OCR instead "
                  "(pdftoppm -png -r 300, then pytesseract).")
            return
        print(f"  text layer: yes ({len(probe):,} chars on first "
              f"{min(3, n)} page(s))")

        found = 0
        for pno, page in enumerate(pdf.pages[:max_pages], start=1):
            for tno, tbl in enumerate(page.extract_tables(), start=1):
                if not tbl or len(tbl) < 2:
                    continue
                found += 1
                widths = {len(r) for r in tbl}
                print(f"\n  -- page {pno} table {tno}: {len(tbl)} rows, "
                      f"col counts {sorted(widths)}")
                if len(widths) > 1:
                    print("     ragged column counts: merged or wrapped cells, "
                          "so a naive DataFrame(tbl[1:], columns=tbl[0]) will "
                          "misalign. Filter to the modal width first.")
                for row in tbl[:4]:
                    cells = [("" if c is None else str(c).replace("\n", " "))[:26]
                             for c in row]
                    print("     | " + " | ".join(cells))
                if found >= max_tables:
                    break
            if found >= max_tables:
                break

        if not found:
            print("  extract_tables() found nothing - the list is probably laid "
                  "out with whitespace, not ruling lines. Use "
                  "`pdftotext -layout` and split on column positions, or pass "
                  'table_settings={"vertical_strategy": "text"}.')

        print("\n  -- layout text sample (page 1)")
        txt = pdf.pages[0].extract_text(layout=True) or pdf.pages[0].extract_text() or ""
        shown = 0
        for line in txt.splitlines():
            if not line.strip():
                continue
            print(f"     {line.rstrip()[:120]}")
            shown += 1
            if shown >= 12:
                break


def _sniff_text(path) -> None:
    raw = path.read_bytes()[:4000]
    for enc in ("utf-8", "utf-8-sig", "cp1252", "latin-1"):
        try:
            head = raw.decode(enc)
            break
        except UnicodeDecodeError:
            continue
    else:
        print("  could not decode"); return
    lines = head.splitlines()[:4]
    print(f"  encoding guess: {enc}")
    for i, ln in enumerate(lines):
        counts = {d: ln.count(d) for d in ["\t", ";", "|", ","] if ln.count(d)}
        print(f"  line {i}: delims={counts}")
        print(f"    {ln[:220]}")


def main() -> int:
    files = sorted(RAW.glob("*"))
    if not files:
        print("data/raw is empty - run python fetch.py first")
        return 1

    for path in files:
        if path.name == "manifest.json":
            continue
        print(f"\n{'#' * 70}\n# {path.name}  ({path.stat().st_size:,} bytes)")
        try:
            if path.suffix in {".xlsx", ".xls"}:
                sheets = pd.ExcelFile(path).sheet_names
                print(f"  sheets: {sheets}")
                _preview_frame(_read_excel_guess_header(path), path.name)
            elif path.suffix == ".txt" or path.suffix == ".csv":
                _sniff_text(path)
                for sep in ["\t", ";", "|", ","]:
                    try:
                        df = pd.read_csv(path, sep=sep, engine="python",
                                         encoding="utf-8", on_bad_lines="skip", nrows=5000)
                    except Exception:
                        continue
                    if df.shape[1] > 1:
                        _preview_frame(df, path.name, f"(sep={sep!r}, first 5000 rows)")
                        break
            elif path.suffix == ".pdf":
                _preview_pdf(path)
            elif path.suffix == ".xml":
                df = pd.read_xml(path)
                _preview_frame(df, path.name)
            elif path.suffix == ".json":
                data = json.loads(path.read_text(encoding="utf-8"))
                df = pd.json_normalize(data)
                _preview_frame(df, path.name)
        except Exception as exc:
            print(f"  ERROR: {type(exc).__name__}: {exc}")

    missing = [k for k, s in MANUAL_SOURCES.items() if not (RAW / s["filename"]).exists()]
    if missing:
        print("\nMISSING MANUAL DOWNLOADS: " + ", ".join(missing))
    return 0


if __name__ == "__main__":
    sys.exit(main())
