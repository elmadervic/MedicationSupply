"""Utilities for handling ATC (Anatomical Therapeutic Chemical) codes."""
from __future__ import annotations

import re

import pandas as pd

from config import RAW, REGISTERS_RAW
from normalize import normalise

ATC_RE = re.compile(r"^[A-V]\d{2}(?:[A-Z]{1,2}(?:\d{2})?)?$")

_SEPARATORS = re.compile(r"[;,/|+]+|\s+")


def split_codes(cell) -> list[str]:
    if cell is None or (isinstance(cell, float) and pd.isna(cell)):
        return []
    text = str(cell).strip()
    if not text or text.lower() == "nan":
        return []
    out = []
    for token in _SEPARATORS.split(text.upper()):
        token = token.strip(" .-")
        if token and ATC_RE.match(token) and token not in out:
            out.append(token)
    return sorted(out)


def join_codes(codes) -> str:
    return "|".join(sorted({c for c in codes if c}))


def level1(code: str) -> str:
    code = str(code or "").strip().upper()
    return code[0] if code and code[0].isalpha() else ""


def chapters(codes_cell: str) -> str:
    return "|".join(sorted({level1(c) for c in str(codes_cell or "").split("|") if c}))


def _find_header_row(path, *needles, limit: int = 25) -> int:
    probe = pd.read_excel(path, header=None, nrows=limit, dtype=str)
    for i in range(len(probe)):
        row = " | ".join(str(v).lower() for v in probe.iloc[i] if str(v) != "nan")
        if all(n in row for n in needles):
            return i
    raise ValueError(f"no header row containing {needles} in the first {limit} rows of {path}")


def _pick(df: pd.DataFrame, *needles) -> str | None:
    for n in needles:
        for c in df.columns:
            if n == str(c).lower().strip():
                return c
    for n in needles:
        for c in df.columns:
            if n in str(c).lower():
                return c
    return None


def _ulcm_pairs(verbose):
    from ulcm import read_ulcm

    path = RAW / "ulcm.xlsx"
    if not path.exists():
        return []
    ul = read_ulcm(path, verbose=False)
    return list(zip(ul["substance"].astype(str), ul["atc_code"].astype(str)))


def _epar_manufacturer_pairs(verbose):
    path = RAW / "manufacturers.csv"
    if not path.exists():
        return []
    df = pd.read_csv(path, sep=";", dtype=str, engine="python", on_bad_lines="skip")
    if "atc_code" not in df.columns or "active_substance" not in df.columns:
        return []
    return list(zip(df["active_substance"].astype(str), df["atc_code"].astype(str)))


def _ema_medicines_pairs(verbose):
    path = RAW / "ema_medicines.xlsx"
    if not path.exists():
        return []
    header = _find_header_row(path, "active substance", "atc code")
    df = pd.read_excel(path, header=header, dtype=str)
    c_sub = _pick(df, "active substance")
    c_atc = _pick(df, "atc code (human)", "atc code")
    if not (c_sub and c_atc):
        return []
    return list(zip(df[c_sub].astype(str), df[c_atc].astype(str)))


def _drugbank_cep_pairs(verbose):
    path = REGISTERS_RAW / "EXPORT_WEB_CEP_with_ATC_drugbank.csv"
    if not path.exists():
        if verbose:
            print(f"  [skip] {path.name} not found - CEP coverage will be lower")
        return []
    df = pd.read_csv(path, dtype=str, engine="python", on_bad_lines="skip")
    c_sub = _pick(df, "substance")
    c_atc = _pick(df, "atc_code", "atc")
    if not (c_sub and c_atc):
        return []
    return list(zip(df[c_sub].astype(str), df[c_atc].astype(str)))


SOURCES = {
    "ulcm": _ulcm_pairs,
    "epar_manufacturers": _epar_manufacturer_pairs,
    "ema_medicines": _ema_medicines_pairs,
    "drugbank_cep": _drugbank_cep_pairs,
}


def build_atc_map(verbose: bool = True) -> dict[str, list[str]]:
    mapping: dict[str, set[str]] = {}
    if verbose:
        print("  building substance -> ATC lookup:")
    for name, loader in SOURCES.items():
        try:
            pairs = loader(verbose)
        except Exception as exc:
            print(f"  [skip] {name}: {exc}")
            continue
        added = 0
        for substance, cell in pairs:
            codes = split_codes(cell)
            if not codes:
                continue
            key = normalise(substance).key
            if not key:
                continue
            mapping.setdefault(key, set()).update(codes)
            added += 1
        if verbose:
            print(f"    {name:<20} {added:>5,} usable rows -> {len(mapping):,} keys so far")
    if verbose:
        print(f"  lookup holds {len(mapping):,} substances")
    return {k: sorted(v) for k, v in mapping.items()}
