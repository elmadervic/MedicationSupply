## =================================================================
## Combine manufacturer registers from FOUR source files into one
## standardized table, filtered to critical ATC codes.
##
## Sources used (Combined_data.csv is NO LONGER used):
##   1. EXPORT_WEB_CEP_with_ATC_drugbank.csv   (EDQM CEP holders)
##   2. bfarm_api_origin_critical_rest_LONG.csv (BfArM / German register)
##   3. EMA_data_critical.csv                   (EMA EPARs)
##   4. ireland_critical_atc_review.csv         (HPRA / Ireland)
##
## IMPORTANT: none of these four files is a standalone "critical.csv"
## reference list of ATC codes (that file isn't part of this set).
## EMA/Germany/Ireland's filenames indicate they are ALREADY filtered
## to critical substances, so critical_codes is derived as the union
## of the ATC codes found in those three files, and that union is
## then used to filter the CEP data (which is NOT pre-filtered).
## =================================================================

## -----------------------------------------------------------------
## Paths. Run this script from the repository root.
##   DATA_DIR - the four source registers + critical.csv (read-only)
##   OUT_DIR  - everything this script writes (tables and figures)
## -----------------------------------------------------------------
DATA_DIR <- "data/manufacturer-registers/raw"
OUT_DIR  <- "data/manufacturer-registers/out"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

library(readr)
library(dplyr)
library(janitor)
library(stringr)
library(tidyr)
library(purrr)

## -----------------------------------------------------------------
## 0. ATC LEVEL 1 REFERENCE TABLE (fixed WHO top-level groups)
## -----------------------------------------------------------------
atc_level1 <- tribble(
  ~level1_code, ~level1_name,
  "A", "Alimentary tract & metabolism",
  "B", "Blood & blood forming organs",
  "C", "Cardiovascular system",
  "D", "Dermatologicals",
  "G", "Genito urinary system & sex hormones",
  "H", "Systemic hormonal preparations",
  "J", "Antiinfectives (systemic)",
  "L", "Antineoplastic & immunomodulating",
  "M", "Musculo-skeletal system",
  "N", "Nervous system",
  "P", "Antiparasitic products",
  "R", "Respiratory system",
  "S", "Sensory organs",
  "V", "Various"
)

## -----------------------------------------------------------------
## 1. EMA -- EMA_data_critical.csv
##    Already clean: manufacturer_name, full-name country,
##    numeric manufacturer_step (1 = active substance, 2 = finished
##    product / batch release). Already pre-filtered to critical ATCs.
## -----------------------------------------------------------------
ema_raw <- read_csv(file.path(DATA_DIR, "EMA_data_critical.csv"), show_col_types = FALSE) |>
  clean_names()

ema_std <- ema_raw |>
  filter(!is.na(atc_code), atc_code != "") |>
  transmute(
    atc_code          = atc_code,
    # FIX: manufacturer_name has 16 rows with an embedded trailing
    # newline (e.g. "Exelead, Inc.\n"). Left in, these raw newlines
    # survive the later write_csv() below and can corrupt row parsing
    # on the read.csv() side in plots_by_source.R (a bare newline
    # inside an unquoted-looking field can be misread as a row break),
    # which was producing a lower distinct-manufacturer count there
    # (140) than a script that never round-trips through CSV (144).
    # str_squish() collapses all whitespace (including newlines) to
    # single spaces and trims ends, so no raw newline ever reaches the
    # CSV write step.
    mfr_company       = str_squish(manufacturer_name),
    mfr_country       = country,
    manufacturer_step = as.integer(manufacturer_step),
    source            = "ema"
  )

cat("ema_std rows:", nrow(ema_std), "\n")

## -----------------------------------------------------------------
## 2. Germany -- bfarm_api_origin_critical_rest_LONG.csv
##    Long format: one row per (product, role, company). 'role' can be
##    Zulassungsinhaber (marketing authorization holder -- NOT a
##    manufacturer, excluded), Wirkstoffherstellung (active substance
##    manufacture -> step 1), or Hersteller/Endfreigabe (manufacturer /
##    batch release -> step 2). 'land' is in German and needs
##    translating. Already pre-filtered to critical ATCs.
## -----------------------------------------------------------------
germany_raw <- read_csv(file.path(DATA_DIR, "bfarm_api_origin_critical_rest_LONG.csv"), show_col_types = FALSE) |>
  clean_names()

de_to_en_country <- c(
  "Argentinien" = "Argentina", "Australien" = "Australia", "Belgien" = "Belgium",
  "Brasilien" = "Brazil", "Bulgarien" = "Bulgaria", "Chile" = "Chile",
  "China" = "China", "Deutschland" = "Germany", "Dänemark" = "Denmark",
  "Finnland" = "Finland", "Frankreich" = "France", "Griechenland" = "Greece",
  "Indien" = "India", "Irland" = "Ireland", "Island" = "Iceland",
  "Israel" = "Israel", "Italien" = "Italy", "Japan" = "Japan",
  "Kanada" = "Canada", "Korea, Republik" = "South Korea", "Kroatien" = "Croatia",
  "Lettland" = "Latvia", "Litauen" = "Lithuania", "Malta" = "Malta",
  "Mexiko" = "Mexico", "Monaco" = "Monaco", "Niederlande" = "Netherlands",
  "Norwegen" = "Norway", "Polen" = "Poland", "Portugal" = "Portugal",
  "Puerto Rico" = "Puerto Rico", "Rumänien" = "Romania", "Schweden" = "Sweden",
  "Schweiz" = "Switzerland", "Singapur" = "Singapore", "Slowakei" = "Slovakia",
  "Slowenien" = "Slovenia", "Spanien" = "Spain", "Südafrika" = "South Africa",
  "Südkorea" = "South Korea", "Taiwan" = "Taiwan",
  "Tschechische Republik" = "Czech Republic", "Türkei" = "Turkey",
  "Ukraine" = "Ukraine", "Ungarn" = "Hungary", "Vereinigte Staaten" = "United States",
  "Vereinigtes Königreich" = "United Kingdom",
  "Vereinigtes Königreich (Nordirland)" = "United Kingdom",
  "Zypern" = "Cyprus", "Österreich" = "Austria"
  # extend if you spot unmatched values in the check below
)

## check for any German country names not covered by the map
missing_de_countries <- germany_raw |>
  filter(!is.na(land), !land %in% names(de_to_en_country)) |>
  distinct(land) |>
  pull(land)

if (length(missing_de_countries) > 0) {
  cat("German country names not in de_to_en_country, will show as NA:\n")
  print(missing_de_countries)
}

germany_std <- germany_raw |>
  filter(!is.na(atc_code), atc_code != "") |>
  filter(role != "Zulassungsinhaber") |>          # keep manufacturers only, drop MA holders
  mutate(
    manufacturer_step = case_when(
      role == "Wirkstoffherstellung"    ~ 1L,     # active substance manufacture
      role == "Hersteller/Endfreigabe"  ~ 2L,     # finished product / batch release
      TRUE                              ~ NA_integer_
    )
  ) |>
  transmute(
    atc_code          = atc_code,
    mfr_company       = name,
    mfr_country       = unname(de_to_en_country[land]),
    manufacturer_step = manufacturer_step,
    source            = "germany"
  )

cat("germany_std rows:", nrow(germany_std), "\n")

## -----------------------------------------------------------------
## 3. Ireland -- ireland_critical_atc_review.csv
##    Use matched_critical_atc (single clean code), NOT the messy
##    combined atc_code text field (e.g. "J02AC Triazole derivatives,
##    J02AC01 fluconazole"). mfr_company and mfr_country are parallel
##    "|"-separated lists (multiple manufacturers per product). Split
##    and pair them up; where the two lists don't have the same length
##    (~19 rows), fall back to keeping the row unsplit rather than
##    guessing a pairing. Already pre-filtered to critical ATCs.
## -----------------------------------------------------------------
ireland_raw <- read_csv(file.path(DATA_DIR, "ireland_critical_atc_review.csv"), show_col_types = FALSE) |>
  clean_names()

## FIX: one row has matched_critical_atc = "L01BA01, L04AX03" -- two
## codes glued into a single un-split string (a data-quality issue in
## the source file itself). Split on comma too, same treatment as
## CEP's atc_code field, so that row's codes are still counted rather
## than silently failing the critical.csv match as one glued string.
ireland_raw <- ireland_raw |>
  filter(!is.na(matched_critical_atc), matched_critical_atc != "") |>
  mutate(atc_split = str_split(matched_critical_atc, ",\\s*")) |>
  unnest(atc_split) |>
  mutate(matched_critical_atc = str_trim(atc_split)) |>
  select(-atc_split)

split_trim <- function(x) {
  if (is.na(x)) return(NA_character_)
  # FIX: some Ireland manufacturer names have an embedded newline in
  # the middle of the string (e.g. "Fannin Limited\nFannin House,\n",
  # 81 rows affected) -- str_trim() only strips leading/trailing
  # whitespace, leaving that internal newline in place and putting it
  # at risk during the later write_csv()/read.csv() round trip in the
  # plots_by_source.R pipeline. str_squish() collapses ALL whitespace
  # (including internal newlines) to single spaces and trims the ends.
  str_squish(str_split(x, "\\|")[[1]])
}

ireland_pairs <- ireland_raw |>
  filter(!is.na(matched_critical_atc), matched_critical_atc != "") |>
  mutate(row_id = row_number()) |>
  mutate(
    company_list = map(mfr_company, split_trim),
    country_list = map(mfr_country, split_trim)
  ) |>
  mutate(
    lengths_match = map2_lgl(company_list, country_list, ~ length(.x) == length(.y))
  )

## rows where company/country lists line up 1:1 -> expand to one row each
ireland_matched <- ireland_pairs |>
  filter(lengths_match) |>
  select(row_id, atc_code = matched_critical_atc, company_list, country_list) |>
  unnest(cols = c(company_list, country_list)) |>
  rename(mfr_company = company_list, mfr_country = country_list)

## rows where lists don't line up -> keep the original unsplit strings
## (still squish whitespace so embedded newlines don't survive into the CSV)
ireland_unmatched <- ireland_pairs |>
  filter(!lengths_match) |>
  transmute(
    row_id,
    atc_code    = matched_critical_atc,
    mfr_company = str_squish(mfr_company),
    mfr_country = str_squish(mfr_country)
  )

if (nrow(ireland_unmatched) > 0) {
  cat("Ireland rows kept unsplit (company/country list length mismatch):",
      nrow(ireland_unmatched), "\n")
}

ireland_std <- bind_rows(ireland_matched, ireland_unmatched) |>
  transmute(
    atc_code          = atc_code,
    mfr_company       = mfr_company,
    mfr_country       = mfr_country,
    manufacturer_step = NA_integer_,   # not disclosed in HPRA data
    source            = "ireland"
  )

cat("ireland_std rows:", nrow(ireland_std), "\n")

## -----------------------------------------------------------------
## 4. Determine the critical ATC code universe.
##
## FIX: this used to always derive critical_codes as the union of
## EMA/Germany/Ireland's own codes, because no Data/critical.csv was
## available. That derived union is only an approximation of the real
## reference list -- atc_summary_report_plot.R filters CEP (and
## Ireland) against an actual Data/critical.csv, and the two different
## reference universes were the root cause of the remaining CEP and
## Ireland discrepancies between that script's output and this one's.
## Now: use Data/critical.csv directly when it exists (matching
## atc_summary_report_plot.R exactly), and only fall back to the
## derived union if it doesn't -- so the two pipelines agree whenever
## the real reference file is present.
## -----------------------------------------------------------------
if (file.exists(file.path(DATA_DIR, "critical.csv"))) {
  critical <- read.csv(file.path(DATA_DIR, "critical.csv"), stringsAsFactors = FALSE)
  critical_codes <- unique(critical$ATC.level.5)
  critical_codes <- critical_codes[!is.na(critical_codes) & critical_codes != ""]
  cat("Critical ATC codes loaded from critical.csv:", length(critical_codes), "\n")
} else {
  critical_codes <- unique(c(ema_std$atc_code, germany_std$atc_code, ireland_std$atc_code))
  cat("critical.csv not found in", DATA_DIR, "-- falling back to the derived union of",
      "EMA/Germany/Ireland's own ATC codes:", length(critical_codes), "\n")
}

## -----------------------------------------------------------------
## 5. CEP -- EXPORT_WEB_CEP_with_ATC_drugbank.csv
##    NOT pre-filtered to critical substances, and can have MULTIPLE
##    ATC codes in one cell (comma-separated) -> split into one row
##    per code before filtering. holder_country comes from the
##    trailing 2-letter ISO code in certificate_cep_holder.
## -----------------------------------------------------------------
cep_atc <- read_csv(file.path(DATA_DIR, "EXPORT_WEB_CEP_with_ATC_drugbank.csv"), show_col_types = FALSE) |>
  clean_names()

cat("CEP rows before splitting multi-code cells:", nrow(cep_atc), "\n")

cep_atc <- cep_atc %>% filter(status_cep == "Valid" )
cep_atc <- cep_atc |>
  filter(!is.na(atc_code), atc_code != "") |>
  separate_rows(atc_code, sep = ",\\s*")   # one row per individual ATC code

cat("CEP rows after splitting multi-code cells:", nrow(cep_atc), "\n")

cep_atc <- cep_atc |> filter(atc_code %in% critical_codes)
cat("CEP rows after filtering to critical ATC codes:", nrow(cep_atc), "\n")

iso2_to_name <- c(
  AT = "Austria", BE = "Belgium", BG = "Bulgaria", HR = "Croatia",
  CY = "Cyprus", CZ = "Czech Republic", DK = "Denmark", EE = "Estonia",
  FI = "Finland", FR = "France", DE = "Germany", GR = "Greece",
  HU = "Hungary", IE = "Ireland", IT = "Italy", LV = "Latvia",
  LT = "Lithuania", LU = "Luxembourg", MT = "Malta", NL = "Netherlands",
  PL = "Poland", PT = "Portugal", RO = "Romania", SK = "Slovakia",
  SI = "Slovenia", ES = "Spain", SE = "Sweden", IS = "Iceland",
  LI = "Liechtenstein", NO = "Norway", GB = "United Kingdom",
  US = "United States", CN = "China", IN = "India", JP = "Japan",
  CH = "Switzerland", CA = "Canada", AU = "Australia", KR = "South Korea",
  BR = "Brazil", MX = "Mexico", IL = "Israel", TR = "Turkey",
  ZA = "South Africa", AR = "Argentina", NZ = "New Zealand",
  SG = "Singapore", TW = "Taiwan", RU = "Russia", UA = "Ukraine",
  # FIX: MY and TH appear in the critical-filtered CEP holder data (5
  # rows) but were missing here, unlike atc_summary_report_plot.R's
  # more complete iso2_to_name -- those rows got country = NA and were
  # silently dropped downstream, undercounting both CEP countries and
  # manufacturers relative to that script. Synced with the fuller list.
  AE = "United Arab Emirates", CO = "Colombia", HK = "Hong Kong",
  ID = "Indonesia", JO = "Jordan", MA = "Morocco", MC = "Monaco",
  MH = "Marshall Islands", MO = "Macao", MY = "Malaysia",
  OM = "Oman", PK = "Pakistan", PR = "Puerto Rico",
  SA = "Saudi Arabia", TH = "Thailand"
)

cep_atc <- cep_atc |>
  mutate(holder_country = str_extract(certificate_cep_holder, "[A-Z]{2}$"))

missing_codes <- cep_atc |>
  filter(!is.na(holder_country), !holder_country %in% names(iso2_to_name)) |>
  distinct(holder_country) |>
  pull(holder_country)

if (length(missing_codes) > 0) {
  cat("Country codes not in iso2_to_name, will show as NA:\n")
  print(missing_codes)
}

cep_std <- cep_atc |>
  transmute(
    atc_code          = atc_code,
    mfr_company       = certificate_cep_holder,
    mfr_country       = unname(iso2_to_name[holder_country]),
    manufacturer_step = NA_integer_,   # CEP data doesn't disclose step 1/2
    source            = "cep"
  )

cat("cep_std rows:", nrow(cep_std), "\n")

## -----------------------------------------------------------------
## 6. COMBINE ALL FOUR STANDARDIZED FRAMES, ADD ATC LEVEL 1, SAVE
##
## FIX: this used to re-filter the WHOLE combined table (including
## Germany/EMA) against critical_codes. That was a safe no-op when
## critical_codes was a derived superset of Germany/EMA's own codes,
## but now that critical_codes can come from the real Data/critical.csv
## (step 4 above), it's no longer guaranteed to be a superset -- and
## Germany/EMA's own files are already pre-filtered to critical
## substances upstream (per their file names), matching how
## atc_summary_report_plot.R treats them. So only Ireland and CEP get
## the extra critical_codes filter here; CEP was already filtered at
## the point it was built (step 5), and Ireland's matched_critical_atc
## is filtered explicitly below -- Germany/EMA are left as-is.
## -----------------------------------------------------------------
ireland_std <- ireland_std |> filter(atc_code %in% critical_codes)

manufacturer_registers <- bind_rows(ema_std, germany_std, ireland_std, cep_std) |>
  mutate(level1_code = str_extract(atc_code, "^[A-Z]")) |>
  left_join(atc_level1, by = "level1_code")

cat("Combined rows (critical only):", nrow(manufacturer_registers), "\n")
cat("Rows by source:\n")
print(table(manufacturer_registers$source, useNA = "ifany"))

write_csv(manufacturer_registers, file.path(OUT_DIR, "manufacturer_registers_combined.csv"))