"""Snapshot every source and record what was fetched, when, and its hash.

Run:  python -m critmed.fetch

The manifest is the reproducibility artefact. When your numbers disagree with
someone else's, the first question is always "same snapshot?" and this answers
it without argument.
"""
from __future__ import annotations

import hashlib
import json
import sys
import time
from datetime import datetime, timezone

import requests

from config import AUTO_SOURCES, MANIFEST, MANUAL_SOURCES, RAW

UA = "critmed-research/0.1 (academic supply-chain study; contact: your.email@tuwien.ac.at)"
TIMEOUT = 120


def sha256(path) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def _get(url: str) -> requests.Response:
    r = requests.get(url, headers={"User-Agent": UA}, timeout=TIMEOUT)
    r.raise_for_status()
    return r


def fetch_fda(dest) -> dict:
    """openFDA caps a single call at 100 records and a single query at 5000.

    Page with skip= until exhausted. Keep the raw records; do not filter here.
    """
    base = "https://api.fda.gov/drug/shortages.json"
    records, skip, limit = [], 0, 100
    while True:
        r = _get(f"{base}?limit={limit}&skip={skip}")
        payload = r.json()
        batch = payload.get("results", [])
        records.extend(batch)
        total = payload.get("meta", {}).get("results", {}).get("total")
        skip += limit
        if not batch or (total is not None and skip >= total) or skip >= 26000:
            break
        time.sleep(0.3)  # be polite; unauthenticated limit is 240 req/min
    dest.write_text(json.dumps(records, ensure_ascii=False), encoding="utf-8")
    return {"n_records": len(records), "reported_total": total}


def main() -> int:
    manifest = {
        "snapshot_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "sources": {},
    }
    failures = []

    for key, (url, filename, role) in AUTO_SOURCES.items():
        dest = RAW / filename
        try:
            if key == "fda_shortages":
                extra = fetch_fda(dest)
            else:
                r = _get(url)
                dest.write_bytes(r.content)
                extra = {"content_type": r.headers.get("Content-Type", "")}
            manifest["sources"][key] = {
                "url": url,
                "file": filename,
                "role": role,
                "bytes": dest.stat().st_size,
                "sha256": sha256(dest),
                "fetched_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"),
                **extra,
            }
            print(f"  ok   {key:24s} {dest.stat().st_size:>10,d} B")
        except Exception as exc:  # noqa: BLE001 - we want the reason in the manifest
            failures.append(key)
            manifest["sources"][key] = {"url": url, "file": filename, "role": role,
                                        "error": f"{type(exc).__name__}: {exc}"}
            print(f"  FAIL {key:24s} {type(exc).__name__}: {exc}")

    for key, spec in MANUAL_SOURCES.items():
        dest = RAW / spec["filename"]
        if dest.exists():
            manifest["sources"][key] = {
                "url": spec["url"], "file": spec["filename"], "role": spec["role"],
                "manual": True, "bytes": dest.stat().st_size, "sha256": sha256(dest),
                "mtime_local": datetime.fromtimestamp(dest.stat().st_mtime).isoformat(timespec="seconds"),
            }
            print(f"  ok   {key:24s} {dest.stat().st_size:>10,d} B  (manual)")
        else:
            failures.append(key)
            manifest["sources"][key] = {"url": spec["url"], "file": spec["filename"],
                                        "role": spec["role"], "manual": True,
                                        "error": "missing", "howto": spec["howto"]}
            print(f"  TODO {key:24s} MISSING -> {spec['howto']}")

    MANIFEST.write_text(json.dumps(manifest, indent=2, ensure_ascii=False), encoding="utf-8")
    print(f"\nmanifest: {MANIFEST}")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
