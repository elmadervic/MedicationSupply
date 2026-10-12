"""
EudraGMDP scraper — June 2026 snapshot, with operational scope extraction.

Same source and same search filters as scrape_eudragmdp.py (the May 2025 run):
  - GMP Compliance search (searchGMPCompliance.do), compliant certs only.
  - filter 'eea'   : Art.111(5)+111(1), EEA countries.
  - filter 'third' : Art.111(4), all countries.

What's NEW vs the May 2025 run:
  Phase 2 now keeps, per certificate, a RICH record instead of just substance
  names — so we never have to re-fetch for a forgotten field again:
    - issuing_authority, last_inspection_date
    - withdrawn / withdrawn_date
    - substances            (list, as before — for INN mapping later)
    - scope                 ({substance: [ {code,label}, ... ]}) from Part 2,
                             section "3. MANUFACTURING OPERATIONS - ACTIVE SUBSTANCES"
  Output goes to Data/June_2026/ and NEVER touches Data/May_2025/.

INN matching is deliberately NOT done here — we extract raw now, map names later.

Usage:  python scrape_eudragmdp_2026.py [eea|third]   (default: both)
"""

import json, re, time, threading
import requests
from bs4 import BeautifulSoup
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

# Reuse the proven Phase-1 machinery (path-agnostic: cache_file is passed in)
from scrape_eudragmdp import _submit_form, _fetch_range, CERT_URL, FILTERS

OUTDIR = Path("Data/June_2026")
OUTDIR.mkdir(parents=True, exist_ok=True)


# ── Phase 1: collect certificate keys (into June_2026) ────────────────────────

def collect_keys(filter_key: str, n_workers: int = 5) -> list[dict]:
    cache_file = OUTDIR / f"eudragmdp_keys_{filter_key}.json"
    if cache_file.exists():
        data = json.loads(cache_file.read_text())
        if data:
            print(f"[{filter_key}] loaded {len(data)} keys from cache")
            return data
        cache_file.unlink()

    print(f"[{filter_key}] collecting keys ({n_workers} parallel sessions)…")
    _, total = _submit_form(filter_key)
    total_pages = (total + 9) // 10
    print(f"[{filter_key}] {total} certs / {total_pages} pages")

    chunk = (total_pages + n_workers - 1) // n_workers
    ranges = [(i * chunk, min((i + 1) * chunk, total_pages)) for i in range(n_workers)]
    lock = threading.Lock()
    shared = {"records": [], "seen": set(), "done": 0, "total_pages": total_pages}

    with ThreadPoolExecutor(max_workers=n_workers) as pool:
        futures = [pool.submit(_fetch_range, filter_key, s, e, i, lock, shared, cache_file)
                   for i, (s, e) in enumerate(ranges)]
        for f in as_completed(futures):
            f.result()

    cache_file.write_text(json.dumps(shared["records"], indent=2))
    print(f"[{filter_key}] saved {len(shared['records'])} unique keys -> {cache_file}")
    return shared["records"]


# ── Phase 2: rich certificate parsing (substances + operational scope) ────────

_LANG_RE = re.compile(r"\(([a-z]{2})\)$", re.I)
_CODE_RE = re.compile(r"^(3(?:\.\d+)+)\b")          # 3.1, 3.1.1, 3.2.7 …
_SCOPE_HEADER = re.compile(r"^3\.\s*MANUFACTURING OPERATIONS\s*-\s*ACTIVE SUBSTANCES", re.I)


def _clean(s: str) -> str:
    return re.sub(r"\s+", " ", s.replace("\xa0", " ")).strip()


def parse_cert(html: str) -> dict:
    lines = [l.strip() for l in BeautifulSoup(html, "html.parser")
             .get_text(separator="\n").split("\n") if l.strip()]

    out = {"cert_type": None, "issuing_authority": None, "last_inspection_date": None,
           "withdrawn": False, "withdrawn_date": None,
           "substances": [], "scope": {}}

    for i, l in enumerate(lines):
        m = re.search(r"withdrawn date is\s*:\s*([\d-]+)", l)
        if m:
            out["withdrawn"] = True
            out["withdrawn_date"] = m.group(1)
        if out["issuing_authority"] is None and l.upper().startswith("CERTIFICATE NUMBER"):
            out["issuing_authority"] = lines[i - 1] if i > 0 else None
        if out["last_inspection_date"] is None and "latest of which was conducted on" in l:
            for j in range(i, min(i + 3, len(lines))):
                d = re.search(r"(\d{4}-\d{2}-\d{2})", lines[j])
                if d:
                    out["last_inspection_date"] = d.group(1)
                    break

    # substances: Part 2 "Names of substances subject to inspection"
    in_names = False
    for l in lines:
        if "Names of substances subject to" in l:
            in_names = True
            continue
        if in_names:
            if _LANG_RE.search(l):
                nm = _LANG_RE.sub("", l).strip()
                nm = re.sub(r"^\[\w+?\]", "", nm).strip()
                if nm:
                    out["substances"].append(nm)
            else:
                in_names = False

    # scope: section 3 "MANUFACTURING OPERATIONS - ACTIVE SUBSTANCES"
    in_scope = False
    cur = None
    for l in lines:
        if _SCOPE_HEADER.match(l):
            in_scope = True
            continue
        if not in_scope:
            continue
        if l.startswith("Active Substance:"):
            cur = _clean(l.split(":", 1)[1])
            out["scope"].setdefault(cur, [])
            continue
        if cur is None:
            continue
        m = _CODE_RE.match(l)
        if m:
            code = m.group(1)
            label = _clean(l[m.end():]).lstrip(":").strip()
            out["scope"][cur].append({"code": code, "label": label})
        elif re.match(r"^[4-9]\.\s", l):
            in_scope = False  # reached next top-level section (e.g. "4. OTHER ACTIVITIES")
            # NB: must match only real section headers; free-text operation
            # values can start with a digit (e.g. "1,0 % LECITHIN") and must
            # NOT terminate section 3 — that bug truncated ~36% of substances.

    # API certs name an active substance / carry section-3 scope; the rest are
    # finished-product / IMP / importation certs (site-capability only).
    # fallback authority: short-format (older) certs have no "CERTIFICATE NUMBER"
    # line; there the issuing authority is the line just before "Part 1".
    if not out["issuing_authority"]:
        _NAV = {"back to search", "print preview", "print preview (short version)",
                "gmp certificates", "non-compliance report", "search"}
        for i, l in enumerate(lines):
            if l.lower().startswith("part 1") and i > 0:
                for j in range(i - 1, max(-1, i - 5), -1):
                    if lines[j].lower() not in _NAV:
                        out["issuing_authority"] = lines[j]
                        break
                break

    out["cert_type"] = "API" if (out["substances"] or out["scope"]) else "finished"
    return out


def enrich(records: list[dict], filter_key: str, workers: int = 6) -> None:
    cache_file = OUTDIR / f"eudragmdp_certs_{filter_key}.json"
    done = {}
    if cache_file.exists():
        done = {int(k): v for k, v in json.loads(cache_file.read_text()).items()}
        print(f"[{filter_key}] resuming — {len(done)} certs already parsed")

    to_fetch = [r for r in records if r["key"] not in done]
    total = len(to_fetch)
    print(f"[{filter_key}] fetching {total} certificate pages ({workers} workers)…")

    lock = threading.Lock()
    counter = {"n": 0}

    def fetch_one(rec):
        key = rec["key"]
        for attempt in range(3):
            try:
                r = requests.get(CERT_URL.format(key=key),
                                 headers={"User-Agent": "Mozilla/5.0"}, timeout=20)
                if r.status_code == 200:
                    return key, parse_cert(r.text)
            except Exception:
                time.sleep(3 * (attempt + 1))
        return key, None

    with ThreadPoolExecutor(max_workers=workers) as pool:
        futures = [pool.submit(fetch_one, r) for r in to_fetch]
        for fut in as_completed(futures):
            key, parsed = fut.result()
            with lock:
                if parsed is not None:
                    done[key] = parsed
                counter["n"] += 1
                n = counter["n"]
                if n % 250 == 0:
                    cache_file.write_text(json.dumps(done, indent=2, ensure_ascii=False))
                    api = sum(1 for v in done.values() if v.get("substances"))
                    print(f"[{filter_key}] {n}/{total} fetched — {api} API certs — checkpoint")

    cache_file.write_text(json.dumps(done, indent=2, ensure_ascii=False))
    api = sum(1 for v in done.values() if v.get("substances"))
    print(f"[{filter_key}] DONE — {len(done)} certs parsed, {api} with active substances "
          f"-> {cache_file}")


# ── main ──────────────────────────────────────────────────────────────────────

if __name__ == "__main__":
    import sys
    targets = [sys.argv[1]] if len(sys.argv) > 1 else ["eea", "third"]
    for fk in targets:
        assert fk in FILTERS, f"unknown filter {fk}"
        print(f"\n===== FILTER: {fk} =====")
        recs = collect_keys(fk)
        enrich(recs, fk)
    print("\nAll done.")
