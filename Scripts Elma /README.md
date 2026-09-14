# Scripts Elma

R analysis scripts for the manufacturer-register work: they combine four
manufacturing-site registers into one table and produce the concentration
analyses and plots.

## Reproducibility caveat

**The scraping scripts will not reproduce the data files 100% exactly.** The
data was checked manually and some rows were corrected by hand. 

## Input data

These scripts read from a `Data/` folder relative to the working directory.
**That folder is not in this repository**.

| file | source |
|---|---|
| `EXPORT_WEB_CEP_with_ATC_drugbank.csv` | EDQM CEP holders |
| `bfarm_api_origin_critical_rest_LONG.csv` | BfArM / German register |
| `EMA_data_critical.csv` | EMA EPARs |
| `ireland_critical_atc_review.csv` | HPRA / Ireland |

## Run order

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

## Step handling

For BfArM and EPAR, step-1 (API) records take priority: where an ATC code has
any step-1 disclosure, its step-2 rows are dropped; codes with only step-2
keep those rows. Ireland and CEP have no step field, so all their
manufacturing records are used unchanged - the Ireland panels should be read
with that caveat.

## Requirements

```r
install.packages(c(
  "readr", "dplyr", "tidyr", "ggplot2", "janitor", "stringr",
  "purrr", "patchwork", "tidytext", "forcats", "scales"
))
```
