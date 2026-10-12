"""
EDQM CEP database — raw download.

Fetches the full Certificate of Suitability (CEP) database from EDQM's own
"download the online certification database" export and saves it VERBATIM
(no parsing, no filtering — that happens in cep_clean.ipynb).

  Source  : https://extranet.edqm.eu/publications/recherches_CEP.shtml
  Endpoint: https://extranet.edqm.eu/4DLink1/4DCGI/EXPORT_WEB_CEP.txt
            (refreshed daily ~12:00-13:00 CET)
  Format  : tab-separated, UTF-8 with BOM, CRLF line endings

Usage:  python download_cep.py
Output: raw/EXPORT_WEB_CEP.txt   (the input cep_clean.ipynb expects)
"""
from datetime import date
from pathlib import Path
import requests

URL = "https://extranet.edqm.eu/4DLink1/4DCGI/EXPORT_WEB_CEP.txt"
OUT = Path(__file__).parent / "raw" / "EXPORT_WEB_CEP.txt"


def main():
    r = requests.get(URL, timeout=120)
    r.raise_for_status()
    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_bytes(r.content)                      # bytes as served: keep BOM + CRLF
    n_rows = r.content.count(b"\n") - 1            # minus header
    print(f"{date.today()}  saved {OUT}  ({len(r.content):,} bytes, {n_rows:,} rows)")


if __name__ == "__main__":
    main()
