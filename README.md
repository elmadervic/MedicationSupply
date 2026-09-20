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
pip install pandas openpyxl requests lxml pycountry pdfplumber matplotlib
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

Parses every source, normalises substance names, resolves countries, attaches
ATC codes, writes `data/interim/supply_long.csv` — one row per substance ×
supplier × country × role.

#### ATC codes

Every row carries three ATC columns:

| column | meaning |
|---|---|
| `atc_codes` | pipe-separated, e.g. `A10AB01\|A10AC01`. Empty when no code is known |
| `atc_level1` | the anatomical main group letter(s) of those codes |
| `atc_origin` | `native` if the row's own source published the code, `lookup` if it came from the cross-source substance map, empty if neither |

Rows are **not** exploded per code: a substance with six ATC codes stays one
row, so supplier and country counts in this table remain correct. `analyse.py`
explodes them internally where it needs to group by code.

Codes are kept at the level the source published them — `J07B` (level 4) and
`L01EA06` (level 5) both occur, and HPRA often publishes both for one product
(`B05BB|B05BB01`). Nothing is truncated or extrapolated.

Only the EPAR and HPRA sources publish ATC codes of their own. The EDQM CEP
export has none, so those rows depend on a substance → ATC map that
`common/atc.py` assembles from every file that does carry codes: `ulcm.xlsx`,
`manufacturers.csv`, `ema_medicines.xlsx`, and
`data/manufacturer-registers/raw/EXPORT_WEB_CEP_with_ATC_drugbank.csv` from the
R analysis. That last one is optional — if the folder is absent the lookup
simply loses that contribution and CEP coverage drops.

Current coverage: **76.6%** of rows overall — 99% for EPAR and HPRA, 32% for
CEP, which is the ceiling of what substance-name matching reaches against the
Ph. Eur. monograph titles the CEP export uses.


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
| `atc_supply.csv` | one row per ATC code per role — substances, suppliers, countries, HHI |
| `atc_chapter_totals.csv` | the same aggregated to the ATC level-1 chapter |

```bash
python supply-analysis/03-figures/plots.py
```

Writes PNGs to `data/out/figures/`. Mirrors the manufacturer-register figure
set, with role in place of register source. `mah_national` is absent from every
country-based figure for the reason given under *The Irish (HPRA) layer*.

| figure | shows | R counterpart |
|---|---|---|
| `supply_by_source.png` | rows, distinct substances and ATC coverage per raw source | `plots_by_source.png` |
| `hhi_by_chapter.png` | country HHI per ATC chapter, grouped by role, against the 2500 concentration line | `hhi_by_chapter_by_source.png` |
| `country_group_by_chapter.png` | supplier share split China / India / EU-EEA / Other, per chapter, per role | `manufacturing_sites_by_chapter_country_group_source.png` |
| `eea_by_chapter.png` | EU/EEA versus non-EEA supplier share per chapter, per role | `manufacturing_sites_by_chapter_step_source.png` |
| `api_diversification.png` | API suppliers vs. China+India share per critical substance, plus the most exposed ones named | `api_diversification_*.png` |
| `bubble_matrix.png` | country x ATC chapter, dot size = suppliers, dot colour = substances | `bubble_matrix_*.png` |

`api_diversification.png` is restricted to Union-list substances, as its R
counterpart is restricted to critical ATC codes. Without that filter the
"most exposed" ranking fills up with normalisation artefacts - CEP substance
strings carrying process descriptions (`"Acetazolamide, process B"`) split into
one-off combination keys that each look like a single-supplier substance.

```bash
python supply-analysis/02-analysis/qc.py
```

Prints PASS / WARN / FAIL for each check below, then exits non-zero if any of
them failed. Warnings never affect the exit code.

| check | level | flags when |
|---|---|---|
| snapshot complete | FAIL | `manifest.json` is missing. A source that errored during `fetch.py` only warns |
| country resolution | FAIL | 5% or more of supplier rows *in the roles that publish a country* have no resolvable country. Prints three unresolved names |
| country-less roles | — | never fails. Reports how many rows sit in roles with no published country (`mah_national`) |
| ATC coverage | WARN | fewer than 40% of rows carry an ATC code. Prints the share per source |
| ATC format | FAIL | any written code fails the ATC pattern. Prints up to five |
| ATC provenance | — | never fails. Reports the native / lookup / none split |
| duplicate supplier rows | WARN | 30% or more of rows repeat a substance / role / supplier. Some repetition is expected: one holder can hold several CEPs for one substance |
| normalisation collapse | WARN | one normalised key absorbed 60 or more distinct raw strings. Prints the worst eight |
| ULCM coverage | FAIL | 40% or fewer ULCM substances have any supply record. |
| combination handling | — | never fails. Reports how many ULCM entries parsed as combinations, to be spot-checked by hand |
| ULCM parse | FAIL | 2% or more of ULCM rows normalise to an empty key |

All manual checks after `qc.py` were performed and confirmed the script's results.

### The Irish (HPRA) layer

Two things to know about the `mah_national` rows.

**They used to be missing entirely.** HPRA wraps repeated fields in a container
element (`<ActiveSubstances><ActiveSubstance>…`), and `pandas.read_xml`
flattens only the top level — it read the container's own text, which is just
the whitespace before the first child. Every substance name came out blank, so
every row was dropped at the empty-key filter and the layer silently
contributed nothing. `build_hpra` now walks the children directly, which is
also what recovers the `<ATCs>` block.

**They have no country.** The XML has no address field, so the only candidate
is the holder's company name — and resolving bare company names is actively
wrong: it reads legal-form suffixes as ISO codes (`B. Braun Melsungen AG` →
Antigua, `Eli Lilly Nederland B.V.` → Bouvet Island, `Teva SA` → Saudi
Arabia). That two-letter fallback exists for the EDQM holder strings, which
genuinely end in an ISO code (`CAMBREX KARLSKOGA AB Karlskoga SE`), and is
correct there. Keeping only the names that do resolve would bias the result
rather than reduce it, since those are mostly the ones containing a country
word. So `country_iso2` is left empty for this source and `mah_national` is
excluded from every country-concentration measure via `COUNTRY_ROLES` in
`config.py`. It still counts towards substance and ATC coverage.

Adding the layer therefore left `substance_supply.csv`, `country_totals.csv`
and `distribution.csv` byte-identical; only the row count in `sensitivity.csv`
moved.
