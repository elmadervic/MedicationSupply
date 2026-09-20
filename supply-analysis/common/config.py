"""
Source registry and paths.
"""
from pathlib import Path

# common/ -> supply-analysis/ -> repository root
ROOT = Path(__file__).resolve().parent.parent.parent
RAW = ROOT / "data" / "raw"
INTERIM = ROOT / "data" / "interim"
OUT = ROOT / "data" / "out"
for _p in (RAW, INTERIM, OUT):
    _p.mkdir(parents=True, exist_ok=True)

REGISTERS_RAW = ROOT / "data" / "manufacturer-registers" / "raw"

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
    "ema_shortages": (
        "https://www.ema.europa.eu/en/documents/report/medicines-output-shortages-report_en.xlsx",
        "ema_shortages.xlsx",
        "outcome",
    ),
    "bfarm_versorgungsrelevant": (
        "https://www.bfarm.de/SharedDocs/Downloads/DE/Arzneimittel/Zulassung/"
        "amInformationen/Lieferengpaesse/ListeVersorgungsrelevanteWirkstoffe.pdf"
        "?__blob=publicationFile",
        "bfarm_versorgungsrelevant.pdf",
        "universe",
    ),
    "hpra_products": (
        "http://www.hpra.ie/img/uploaded/swedocuments/latestHMlist.xml",
        "hpra_products.xml",
        "supply",
    ),
    "fda_shortages": (
        "https://api.fda.gov/drug/shortages.json?limit=1000",
        "fda_shortages.json",
        "outcome",
    ),
}

MANUAL_SOURCES = {
    "edqm_cep": dict(
        filename="edqm_cep.txt",
        role="supply",
        url="https://extranet.edqm.eu/publications/recherches_CEP.shtml",
        howto=(
            "Open the page, scroll to the bottom, click 'Download CEP data file'. "
            "Save as data/raw/edqm_cep.txt. The file is regenerated daily between "
            "12:00 and 13:00 CET, so note the time you downloaded it."
        ),
    ),
    "bfarm_shortages": dict(
        filename="bfarm_shortages.csv",
        role="outcome",
        url="https://anwendungen.pharmnet-bund.de/lieferengpassmeldungen/faces/public/meldungen.xhtml",
        howto=(
            "Choose 'alle Meldungen' (NOT 'Aktuelle Lieferengpaesse' - that view "
            "silently drops resolved reports) and export to CSV. Save as "
            "data/raw/bfarm_shortages.csv."
        ),
    ),
}

MANIFEST = RAW / "manifest.json"
