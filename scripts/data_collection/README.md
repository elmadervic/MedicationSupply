# Data collection

Scripts that collect the raw register data on where the active pharmaceutical
ingredients (APIs) of critical medicines are manufactured. There is one folder per source:

| Folder | Source | What it gives | Script(s) |
|--------|--------|---------------|-----------|
| [`BfArM/`](BfArM/) | BfArM AMGuiFree (German drug register) | per authorised product: API producer, finished-product manufacturer and MA holder, each with country | `scrape_bfarm_api_origin.py` |
| [`CEP/`](CEP/) | EDQM Certificates of Suitability | full CEP database; holder country per substance | `download_cep.py` → `cep_clean.ipynb` |
| [`CEP/`](CEP/) | EDQM CEP export + DrugBank + NLM RxNav | ATC codes for every CEP substance (`data/registers/raw/EXPORT_WEB_CEP_with_ATC_drugbank.csv`) | `cep_atc_drugbank.R` |
| [`EPAR/`](EPAR/) | EMA EPAR product information, Annex II | manufacturers and manufacturing step per centrally authorised medicine | `ema_epar_pipeline.ipynb` |
| [`HPRA/`](HPRA/) | HPRA product register and package leaflets | manufacturers and countries per Irish product (`ireland_critical_atc_review.csv`) | `hpra_scraper.ipynb` |

BfArM and CEP have their own README covering usage, outputs and caveats; EPAR, HPRA and `cep_atc_drugbank.R` are described in the main README. The register
files used in the paper are in `data/registers/raw/`. They were checked by hand
after scraping and some rows were corrected, so a new run of these scripts does
not reproduce them exactly. The registers are also live: a new run gives the
current version, not the snapshot used in the paper. Snapshot dates are in each
README and in the main README.

## Setup

```bash
pip install requests beautifulsoup4 playwright pandas numpy
playwright install chromium        # headless browser for BfArM
```

## Input: critical-medicines list

`BfArM/scrape_bfarm_api_origin.py` reads the list of critical medicines from
`Data/critical_with_inn.csv`, located **next to the source folders** (i.e.
`scripts/data_collection/Data/critical_with_inn.csv`). The file has 299 rows and
the columns `ATC_code,INN`. It is not included here and must be placed there
before running the script.

CEP needs no input list. It downloads the whole register, and the critical
substances are selected later in the analysis.
