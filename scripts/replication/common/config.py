"""
Source registry and paths.
"""
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
RAW = ROOT / "data" / "replication" / "raw"
INTERIM = ROOT / "data" / "replication" / "interim"
OUT = ROOT / "results" / "replication"
for _p in (RAW, INTERIM, OUT):
    _p.mkdir(parents=True, exist_ok=True)

REGISTERS_RAW = ROOT / "data" / "registers" / "raw"

ROLES = ["api_cep", "bio_api", "batch_release", "mah_national"]
COUNTRY_ROLES = ["api_cep", "bio_api", "batch_release"]


AUTO_SOURCES = {
    "ulcm": (
        "https://www.ema.europa.eu/en/documents/other/union-list-critical-medicines-en.xlsx",
        "ulcm.xlsx",
        "universe",
    ),
    "ema_medicines": (
        "https://www.ema.europa.eu/en/documents/report/medicines-output-medicines-report_en.xlsx",
        "ema_medicines.xlsx",
        "supply",
    ),
    "hpra_products": (
        "http://www.hpra.ie/img/uploaded/swedocuments/latestHMlist.xml",
        "hpra_products.xml",
        "supply",
    ),
}

MANUAL_SOURCES = {
    "edqm_cep": dict(
        filename="edqm_cep.txt",
        role="supply",
        url="https://extranet.edqm.eu/publications/recherches_CEP.shtml",
        howto=(
            "Open the page, scroll to the bottom, click 'Download CEP data file'. "
            "Save as data/replication/raw/edqm_cep.txt. The file is regenerated daily between "
            "12:00 and 13:00 CET, so note the time you downloaded it."
        ),
    ),
}

MANIFEST = RAW / "manifest.json"
