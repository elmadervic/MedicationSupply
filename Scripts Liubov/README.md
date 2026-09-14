## Setup

```bash
python -m venv venv && source venv/bin/activate
pip install pandas openpyxl requests lxml pycountry pdfplumber
```

## Data not fetched by `fetch.py`

Three files in `data/raw/` have to be put there by hand before `build_supply.py`
runs.

| file | origin |
|---|---|
| `edqm_cep.txt` | downloaded manually on 23.08.2026 |
| `bfarm_shortages.csv` | downloaded manually on 23.08.2026|
| `manufacturers.csv` | copied from an earlier project, see below |

config.py` records where each
came from and how to repeat the download.

`manufacturers.csv` is not produced in this project. It is the output of the student project, which
scraped the EMA EPAR medicine pages, extracted the Annex II manufacturer blocks
from the product-information PDFs and geocoded the addresses. 

## Run order

Run these from inside `Scripts Liubov/` (with `venv` activated), as plain scripts —
not with `python -m`, since this folder isn't an importable package.

```bash
python fetch.py
```

```bash
python inspect_sources.py > schema_report.txt
```

Prints the real column names, row counts and sample values of every file in
`data/raw/`. 


```bash
python build_supply.py
```

Parses every source, normalises substance names, resolves countries, writes
`data/interim/supply_long.csv` — one row per substance × supplier × country ×
role. 


```bash
python analyse.py
```

Writes to `data/out/`:

| file | contents |
|---|---|
| `summary.txt` | headline numbers per role |
| `substance_supply.csv` | one row per substance per role |
| `country_totals.csv` | substances and suppliers per country |
| `distribution.csv` | how many substances have exactly *k* supplier countries |
| `sensitivity.csv` | the same numbers with valid-only vs all CEP statuses |

```bash
python qc.py
```

Prints PASS / WARN / FAIL for each check below, then exits non-zero if any of
them failed. Warnings never affect the exit code.

| check | level | flags when |
|---|---|---|
| snapshot complete | FAIL | `manifest.json` is missing. A source that errored during `fetch.py` only warns |
| country resolution | FAIL | 5% or more of supplier rows have no resolvable country. Prints three unresolved names |
| duplicate supplier rows | WARN | 30% or more of rows repeat a substance / role / supplier. Some repetition is expected: one holder can hold several CEPs for one substance |
| normalisation collapse | WARN | one normalised key absorbed 60 or more distinct raw strings. Prints the worst eight |
| ULCM coverage | FAIL | 40% or fewer ULCM substances have any supply record. |
| combination handling | — | never fails. Reports how many ULCM entries parsed as combinations, to be spot-checked by hand |
| ULCM parse | FAIL | 2% or more of ULCM rows normalise to an empty key |

All manual checks after `qc.py` were performaed and confirmed the script's results.