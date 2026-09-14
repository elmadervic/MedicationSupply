# Medication supply concentration

Code and data for the paper. The work has two independent strands, one per
folder.

`supply-analysis/` (Python) works at the level of the active substance: it
fetches the public sources into `data/raw/`, builds one supply table in
`data/interim/`, and writes the headline numbers to `data/out/`.

`manufacturer-registers/` (R) works at the level of the manufacturing site: it
combines four site registers into one table and produces the concentration
analyses and plots.

Each strand's own instructions follow, unchanged, in the two sections below.

## Substance-level supply analysis (Python)

### Setup

```bash
python -m venv venv && source venv/bin/activate
pip install pandas openpyxl requests lxml pycountry pdfplumber
```

### Data not fetched by `fetch.py`

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

### Run order

Run these from inside `supply-analysis/` (with `venv` activated), as plain scripts —
not with `python -m`, since this folder isn't an importable package.

```bash
python fetch.py
```

```bash
python inspect_sources.py > ../data/out/schema_report.txt
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
## Manufacturer registers (R)

R analysis scripts for the manufacturer-register work: they combine four
manufacturing-site registers into one table and produce the concentration
analyses and plots.

### Reproducibility caveat

**The scraping scripts will not reproduce the data files 100% exactly.** The
data was checked manually and some rows were corrected by hand. 

### Input data

These scripts read from a `Data/` folder relative to the working directory.
**That folder is not in this repository**.

| file | source |
|---|---|
| `EXPORT_WEB_CEP_with_ATC_drugbank.csv` | EDQM CEP holders |
| `bfarm_api_origin_critical_rest_LONG.csv` | BfArM / German register |
| `EMA_data_critical.csv` | EMA EPARs |
| `ireland_critical_atc_review.csv` | HPRA / Ireland |

### Run order

`ComebineAll4SourcesV2.R` first, it writes
`manufacturer_registers_combined.csv`, which `PlotbySource_V2.R` reads. The
rest are independent and read the four source files directly.

| script | produces |
|---|---|
| `ComebineAll4SourcesV2.R` | `manufacturer_registers_combined.csv` — one standardised table, filtered to critical ATC codes |
| `PlotbySource_V2.R` | `Data/plots_by_source.png` — descriptive plots per source |
| `HHI_V2.R` | `Data/hhi_*.csv` and `Data/hhi_*.png` — country-level HHI per ATC code, and which source drives the worst-case concentration |
| `EEAnonEEASteps_V2.R` | `Data/manufacturing_sites_by_chapter_step_source.png` — sites by ATC chapter, split EEA-API / EEA-batch-release / non-EEA |
| `IndiaChina_Steps_V2.R` | `Data/manufacturing_sites_by_chapter_country_group_source.png` — site share by China / India / EU-EEA / Other |
| `api_diversification_by_source.R` | `Data/api_diversification_*.png` — API-producer count vs. China+India share per substance |
| `bubblePlot.R` | `Data/bubble_matrix_*.png` — country × ATC main group bubble matrix |
| `summaryCodeNameCountry_V2.R` | `Data/atc_summary_report_plot*.png` — summary by code, name and country |

Plots and intermediate CSVs are written back into `Data/`.

### Step handling

For BfArM and EPAR, step-1 (API) records take priority: where an ATC code has
any step-1 disclosure, its step-2 rows are dropped; codes with only step-2
keep those rows. Ireland and CEP have no step field, so all their
manufacturing records are used unchanged - the Ireland panels should be read
with that caveat.

### Requirements

```r
install.packages(c(
  "readr", "dplyr", "tidyr", "ggplot2", "janitor", "stringr",
  "purrr", "patchwork", "tidytext", "forcats", "scales"
))
```
