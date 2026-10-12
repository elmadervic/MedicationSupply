# Raw data extraction

Scripts that collect the raw register data on where the active pharmaceutical
ingredients (APIs) of critical medicines are manufactured. There is one folder per source:

| Folder | Source | What it gives | Script(s) |
|--------|--------|---------------|-----------|
| [`BfArM/`](BfArM/) | BfArM AMGuiFree (German drug register) | per authorised product: API producer, finished-product manufacturer and MA holder, each with country | `scrape_bfarm_api_origin.py` |
| [`EudraGMDP/`](EudraGMDP/) | EMA EudraGMDP (EU GMP certificates) | every GMP-compliant site: substances, manufacturing scope, issuing authority, dates | `scrape_eudragmdp_2026.py` (+ `scrape_eudragmdp.py`, which it imports) |
| [`CEP/`](CEP/) | EDQM Certificates of Suitability | full CEP database; holder country per substance | `download_cep.py` → `cep_clean.ipynb` |

Each folder has its own README covering usage, outputs and caveats. The raw
data is not stored in this repository. Running the scripts recreates it, but
the registers are live, so a new run gives the current version and not the
snapshot used in the paper. The snapshot dates are in each README.

## Setup

```bash
pip install requests beautifulsoup4 playwright pandas numpy
playwright install chromium        # headless browser for BfArM and EudraGMDP
```

## Input: critical-medicines list

`BfArM/scrape_bfarm_api_origin.py` reads the list of critical medicines from
`Data/critical_with_inn.csv`, located **next to the source folders** (i.e.
`Raw_data_extraction/Data/critical_with_inn.csv`). The file has 299 rows and
the columns `ATC_code,INN`. It is not included here and must be placed there
before running the script.

EudraGMDP and CEP need no input list. They download the whole register, and
the critical substances are selected later in the analysis.
