# Medication supply

Two independent analyses of critical-medicine supply:

- **`manufacturer-registers/`** (R) — combines four manufacturing-site
  registers into one table and produces the concentration analyses and plots.
- **`supply-analysis/`** (Python) — substance-level supply analysis built from
  seven public sources.

All data lives under `data/`, split by analysis. Run every script **from the
repository root** so the relative paths resolve.

```
data/
├── manufacturer-registers/     # R analysis
│   ├── raw/                    # the four registers + critical.csv (read-only)
│   └── out/                    # tables and figures written by the scripts
├── raw/                        # Python analysis: downloaded/manual sources
├── interim/                    # Python analysis: supply_long.csv
└── out/                        # Python analysis: summary tables
```


## Manufacturer registers (R)

R analysis scripts for the manufacturer-register work: they combine four
manufacturing-site registers into one table and produce the concentration
analyses and plots.

### Reproducibility

**The scraping scripts will not reproduce the resulting data 100% exactly.** The
data was checked manually and some rows were corrected by hand.

### Input data

Read from `data/manufacturer-registers/raw/`. Each script sets `DATA_DIR` and
`OUT_DIR` at the top, so if the data sits elsewhere change those two lines
instead of the read calls.

| file | source |
|---|---|
| `EXPORT_WEB_CEP_with_ATC_drugbank.csv` | EDQM CEP holders |
| `bfarm_api_origin_critical_rest_LONG.csv` | BfArM / German register |
| `EMA_data_critical.csv` | EMA EPARs |
| `ireland_critical_atc_review.csv` | HPRA / Ireland |
| `critical.csv` | reference list of critical ATC codes |

The same folder also holds `EXPORT_WEB_CEP.txt` (the raw EDQM export) and
`DrugBank_FullDatabase.xml` (795 MB, git-ignored), the two upstream inputs the
CEP file was built from. No script in this repository reads them.

`critical.csv` is used to filter to critical ATC codes wherever it is present.
If it is missing, `ComebineAll4SourcesV2.R`, `HHI_V2.R` and
`summaryCodeNameCountry_V2.R` fall back to the union of the ATC codes found in
the EMA / Germany / Ireland files, which are already pre-filtered.

### Run order

`ComebineAll4SourcesV2.R` first, it writes
`manufacturer_registers_combined.csv` into
`data/manufacturer-registers/out/`, which `PlotbySource_V2.R` and
`bubblePlot.R` read from there. The rest are independent and read the four
source files directly.

```bash
Rscript manufacturer-registers/01-data-prep/ComebineAll4SourcesV2.R
```

| script | produces (in `data/manufacturer-registers/out/`) |
|---|---|
| `01-data-prep/ComebineAll4SourcesV2.R` | `manufacturer_registers_combined.csv` — one standardised table, filtered to critical ATC codes |
| `03-figures/PlotbySource_V2.R` | `plots_by_source.png` — descriptive plots per source |
| `02-analysis/HHI_V2.R` | `hhi_*.csv` and `hhi_*.png` — country-level HHI per ATC code, and which source drives the worst-case concentration |
| `03-figures/EEAnonEEASteps_V2.R` | `manufacturing_sites_by_chapter_step_source.png` — sites by ATC chapter, split EEA-API / EEA-batch-release / non-EEA |
| `03-figures/IndiaChina_Steps_V2.R` | `manufacturing_sites_by_chapter_country_group_source.png` — site share by China / India / EU-EEA / Other |
| `02-analysis/api_diversification_by_source.R` | `api_diversification_*.png` — API-producer count vs. China+India share per substance |
| `03-figures/bubblePlot.R` | `bubble_matrix_*.png` — country × ATC main group bubble matrix |
| `03-figures/summaryCodeNameCountry_V2.R` | `atc_summary_report_plot*.png` — summary by code, name and country, plus `germany_critical.csv` |

Plots and intermediate CSVs are written to `data/manufacturer-registers/out/`,
which the scripts create if it does not exist. Nothing is written back into
`raw/`.

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
  "purrr", "patchwork", "tidytext", "forcats", "scales", "ggrepel"
))
```


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

`config.py` records where each
came from and how to repeat the download.

`manufacturers.csv` is not produced in this project. It is the output of the student project, which
scraped the EMA EPAR medicine pages, extracted the Annex II manufacturer blocks
from the product-information PDFs and geocoded the addresses. 

### Run order

Run these from the repository root (with `venv` activated), as plain scripts —
not with `python -m`, since `supply-analysis/` isn't an importable package. The
scripts resolve `data/` relative to their own location, so the working
directory does not actually matter.

```bash
python supply-analysis/01-data-prep/fetch.py
```

```bash
python supply-analysis/01-data-prep/inspect_sources.py > data/out/schema_report.txt
```

Prints the real column names, row counts and sample values of every file in
`data/raw/`. 


```bash
python supply-analysis/01-data-prep/build_supply.py
```

Parses every source, normalises substance names, resolves countries, writes
`data/interim/supply_long.csv` — one row per substance × supplier × country ×
role. 


```bash
python supply-analysis/02-analysis/analyse.py
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
python supply-analysis/02-analysis/qc.py
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

All manual checks after `qc.py` were performed and confirmed the script's results.
