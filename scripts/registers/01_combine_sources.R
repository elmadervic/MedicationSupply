source("scripts/registers/paths.R")

library(readr)
library(dplyr)
library(janitor)
library(stringr)
library(tidyr)
library(purrr)

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

ema_raw <- read_csv(file.path(RAW_DIR, "EMA_data_critical.csv"), show_col_types = FALSE) |>
  clean_names()

ema_std <- ema_raw |>
  filter(!is.na(atc_code), atc_code != "") |>
  transmute(
    atc_code          = atc_code,
    mfr_company       = str_squish(manufacturer_name),
    mfr_country       = country,
    manufacturer_step = as.integer(manufacturer_step),
    source            = "ema"
  )

cat("ema_std rows:", nrow(ema_std), "\n")

germany_raw <- read_csv(file.path(RAW_DIR, "bfarm_api_origin_critical_rest_LONG.csv"), show_col_types = FALSE) |>
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
)

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
  filter(role != "Zulassungsinhaber") |>
  mutate(
    manufacturer_step = case_when(
      role == "Wirkstoffherstellung"    ~ 1L,
      role == "Hersteller/Endfreigabe"  ~ 2L,
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

ireland_raw <- read_csv(file.path(RAW_DIR, "ireland_critical_atc_review.csv"), show_col_types = FALSE) |>
  clean_names()

ireland_raw <- ireland_raw |>
  filter(!is.na(matched_critical_atc), matched_critical_atc != "") |>
  mutate(atc_split = str_split(matched_critical_atc, ",\\s*")) |>
  unnest(atc_split) |>
  mutate(matched_critical_atc = str_trim(atc_split)) |>
  select(-atc_split)

split_trim <- function(x) {
  if (is.na(x)) return(NA_character_)
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

ireland_matched <- ireland_pairs |>
  filter(lengths_match) |>
  select(row_id, atc_code = matched_critical_atc, company_list, country_list) |>
  unnest(cols = c(company_list, country_list)) |>
  rename(mfr_company = company_list, mfr_country = country_list)

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
    manufacturer_step = NA_integer_,
    source            = "ireland"
  )

cat("ireland_std rows:", nrow(ireland_std), "\n")

if (file.exists(file.path(RAW_DIR, "critical.csv"))) {
  critical <- read.csv(file.path(RAW_DIR, "critical.csv"), stringsAsFactors = FALSE)
  critical_codes <- unique(critical$ATC.level.5)
  critical_codes <- critical_codes[!is.na(critical_codes) & critical_codes != ""]
  cat("Critical ATC codes loaded from critical.csv:", length(critical_codes), "\n")
} else {
  critical_codes <- unique(c(ema_std$atc_code, germany_std$atc_code, ireland_std$atc_code))
  cat("critical.csv not found in", RAW_DIR, "-- falling back to the derived union of",
      "EMA/Germany/Ireland's own ATC codes:", length(critical_codes), "\n")
}

cep_atc <- read_csv(file.path(RAW_DIR, "EXPORT_WEB_CEP_with_ATC_drugbank.csv"), show_col_types = FALSE) |>
  clean_names()

cat("CEP rows before splitting multi-code cells:", nrow(cep_atc), "\n")

cep_atc <- cep_atc %>% filter(status_cep == "Valid" )
cep_atc <- cep_atc |>
  filter(!is.na(atc_code), atc_code != "") |>
  separate_rows(atc_code, sep = ",\\s*")

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
    manufacturer_step = NA_integer_,
    source            = "cep"
  )

cat("cep_std rows:", nrow(cep_std), "\n")

ireland_std <- ireland_std |> filter(atc_code %in% critical_codes)

country_spelling <- c(
  "SPAIN" = "Spain", "UK" = "United Kingdom", "USA" = "United States",
  "The Netherlands" = "Netherlands", "Republic of Korea" = "South Korea"
)

manufacturer_registers <- bind_rows(ema_std, germany_std, ireland_std, cep_std) |>
  mutate(
    mfr_country = str_remove(mfr_country, "\\.$"),
    mfr_country = coalesce(unname(country_spelling[mfr_country]), mfr_country),
    level1_code = str_extract(atc_code, "^[A-Z]")
  ) |>
  left_join(atc_level1, by = "level1_code")

cat("Combined rows (critical only):", nrow(manufacturer_registers), "\n")
cat("Rows by source:\n")
print(table(manufacturer_registers$source, useNA = "ifany"))

write_csv(manufacturer_registers, file.path(PROC_DIR, "manufacturer_registers_combined.csv"))
