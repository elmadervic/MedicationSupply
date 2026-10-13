source("paths.R")

library(readr)
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(tidyr)
library(purrr)

# ------------------------------------------------------------------------------
# Settings
# ------------------------------------------------------------------------------
# Critical-medicines list now comes from the WHO EML (eml_export.xlsx) instead of
# the EU Union list (critical.csv). Rows already collected for the EU list are
# reused ("curated"); EML codes a source does not yet cover are topped up from
# that source's full register export ("topup").

EML_FILE              <- "eml_export.xlsx"
EMA_CURATED_FILE      <- "EMA_data_critical.csv"            # EU-list subset (comma-separated)
EMA_FULL_FILE         <- "data.csv"                         # full EMA export (semicolon-separated)
IRELAND_CURATED_FILE  <- "ireland_critical_atc_review.csv"  # EU-list subset, reviewed
IRELAND_FULL_FILE     <- "hpra_final.csv"                   # full HPRA export (~6,300 products)
# BfArM downloads: EU-list codes + EML codes queried afterwards (no overlap)
GERMANY_FILES         <- c("bfarm_api_origin_critical_rest_LONG.csv",
                           "bfarm_api_origin_eml_LONG.csv")
CEP_FILE              <- "EXPORT_WEB_CEP_with_ATC_drugbank.csv"
EU_LIST_FILE          <- "critical.csv"                     # optional, only used for a flag column

# EML therapeutic alternatives often have no ATC code in the export. If TRUE, they
# are mapped by exact INN match against the ATC names in the HPRA export
# (review the lookup written to PROC_DIR).
MAP_EML_ALTERNATIVES_BY_NAME <- FALSE

# If TRUE, use only the ATC codes the EML itself gives on current ("Added") rows
# (540 codes). If FALSE, also add codes found on a medicine's "Removed" rows
# (e.g. atenolol) and, with MAP_EML_ALTERNATIVES_BY_NAME, by INN name lookup.
EML_DIRECT_ONLY <- TRUE

# If TRUE, curated rows whose ATC code is on the EU list but not on the WHO EML are kept.
KEEP_NON_EML_CODES <- FALSE

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

read_existing <- function(files, reader) {
  files <- files[file.exists(file.path(RAW_DIR, files))]
  if (length(files) == 0) stop("None of these files found in ", RAW_DIR, ": ",
                               paste(files, collapse = ", "))
  map(set_names(files), ~ reader(file.path(RAW_DIR, .x)))
}

# Full HPRA export (used for the Ireland top-up and the EML INN -> ATC lookup)
hpra_all <- read_csv(file.path(RAW_DIR, IRELAND_FULL_FILE), show_col_types = FALSE) |>
  clean_names() |>
  distinct(product_id, .keep_all = TRUE)
cat("HPRA products:", nrow(hpra_all), "\n")

atc5_regex <- "\\b[A-Z][0-9]{2}[A-Z]{2}[0-9]{2}\\b"
atc4_regex <- "\\b[A-Z][0-9]{2}[A-Z]{2}\\b"

# HPRA ATC text, e.g. "R06AA Aminoalkyl ethers, R06AA52 diphenhydramine, combinations".
# Level-5 codes are used; level-4 codes only when a product has no level-5 code.
extract_atc_codes <- function(x) {
  map_chr(x, function(s) {
    if (is.na(s) || s == "") return(NA_character_)
    codes <- unique(str_extract_all(s, atc5_regex)[[1]])
    if (length(codes) == 0) codes <- unique(str_extract_all(s, atc4_regex)[[1]])
    if (length(codes) == 0) NA_character_ else paste(codes, collapse = ", ")
  })
}

# ------------------------------------------------------------------------------
# 1. WHO EML: critical ATC codes
# ------------------------------------------------------------------------------
eml_raw <- read_excel(file.path(RAW_DIR, EML_FILE)) |>
  clean_names() |>
  mutate(medicine_name = str_squish(medicine_name),
         medicine_key  = str_to_lower(medicine_name))

# A medicine is on the current list if it has at least one "Added" row
# ("Removed" rows are former listings / indications).
eml_current <- eml_raw |> filter(status == "Added")
cat("EML rows:", nrow(eml_raw), "| current (Added):", nrow(eml_current),
    "| medicines:", n_distinct(eml_current$medicine_key), "\n")

split_eml_atc <- function(df) {
  df |>
    filter(!is.na(atc_codes), atc_codes != "") |>
    separate_rows(atc_codes, sep = "[,;]\\s*") |>
    mutate(atc_code = str_squish(atc_codes)) |>
    filter(str_detect(atc_code, "^[A-Z][0-9]{2}[A-Z]{2}([0-9]{2})?$")) |>
    distinct(medicine_key, medicine_name, atc_code)
}

# (a) ATC codes given on current rows
eml_atc_direct <- split_eml_atc(eml_current) |>
  mutate(eml_atc_origin = "eml_direct")

# (b) current medicines whose ATC code only appears on another (Removed) row of
#     the same medicine, e.g. atenolol
eml_atc_same_name <- split_eml_atc(eml_raw) |>
  filter(medicine_key %in% eml_current$medicine_key,
         !medicine_key %in% eml_atc_direct$medicine_key) |>
  mutate(eml_atc_origin = "eml_other_row_same_medicine")

eml_atc <- if (EML_DIRECT_ONLY) eml_atc_direct else bind_rows(eml_atc_direct, eml_atc_same_name)

eml_no_atc <- eml_current |>
  filter(!medicine_key %in% eml_atc$medicine_key) |>
  group_by(medicine_key) |>
  summarise(medicine_name = first(medicine_name),
            eml_section   = paste(unique(eml_section), collapse = "; "),
            .groups = "drop")

# (c) remaining medicines (mostly therapeutic alternatives): exact INN lookup
#     against the HPRA ATC names
if (!EML_DIRECT_ONLY && MAP_EML_ALTERNATIVES_BY_NAME && nrow(eml_no_atc) > 0) {
  hpra_atc_names <- hpra_all |>
    distinct(atc_code) |>
    filter(!is.na(atc_code)) |>
    mutate(pair = str_match_all(
      atc_code, "([A-Z][0-9]{2}[A-Z]{2}[0-9]{2}) (.+?)(?=, [A-Z][0-9]{2}[A-Z]{2}|$)"
    )) |>
    transmute(pair = map(pair, ~ tibble(atc = .x[, 2], inn = .x[, 3]))) |>
    unnest(pair) |>
    transmute(atc_code = atc, medicine_key = str_to_lower(str_squish(inn))) |>
    distinct()
  
  eml_atc_lookup <- eml_no_atc |>
    inner_join(hpra_atc_names, by = "medicine_key", relationship = "many-to-many") |>
    distinct(medicine_key, medicine_name, atc_code) |>
    mutate(eml_atc_origin = "name_lookup_hpra")
  
  eml_atc    <- bind_rows(eml_atc, eml_atc_lookup)
  eml_no_atc <- eml_no_atc |> filter(!medicine_key %in% eml_atc_lookup$medicine_key)
  
  write_csv(eml_atc_lookup, file.path(PROC_DIR, "who_eml_atc_name_lookup_REVIEW.csv"))
  cat("EML medicines without ATC mapped by INN name (review file written):",
      n_distinct(eml_atc_lookup$medicine_key), "\n")
}

write_csv(eml_no_atc, file.path(PROC_DIR, "who_eml_medicines_without_atc.csv"))
cat("EML medicines still without an ATC code (excluded, listed in file):",
    nrow(eml_no_atc), "\n")

# One row per ATC code; if several origins, keep the most direct one
eml_codes <- eml_atc |>
  mutate(origin_rank = match(eml_atc_origin,
                             c("eml_direct", "eml_other_row_same_medicine", "name_lookup_hpra"))) |>
  group_by(atc_code) |>
  summarise(
    eml_medicine   = paste(sort(unique(medicine_name)), collapse = "; "),
    eml_atc_origin = eml_atc_origin[which.min(origin_rank)],
    .groups = "drop"
  )

critical_codes   <- eml_codes$atc_code
critical_codes_5 <- critical_codes[nchar(critical_codes) == 7]
critical_codes_4 <- critical_codes[nchar(critical_codes) == 5]

# Level-4 EML codes (e.g. A07CA) also match all level-5 codes below them
is_critical <- function(atc) {
  !is.na(atc) & (atc %in% critical_codes | str_sub(atc, 1, 5) %in% critical_codes_4)
}

# Map a register ATC code to the EML code it matched (for the join below)
eml_match_code <- function(atc) {
  if_else(atc %in% critical_codes, atc,
          if_else(str_sub(atc, 1, 5) %in% critical_codes_4, str_sub(atc, 1, 5), NA_character_))
}

cat("WHO EML critical ATC codes:", length(critical_codes),
    "(level 5:", length(critical_codes_5), "| level 4:", length(critical_codes_4), ")\n")

# EU Union list codes: from critical.csv; if it is missing, the union of the codes
# in the three EU-list source files (same fallback as the original script)
if (file.exists(file.path(RAW_DIR, EU_LIST_FILE))) {
  eu_list <- read.csv(file.path(RAW_DIR, EU_LIST_FILE), stringsAsFactors = FALSE)
  eu_list_codes <- unique(eu_list$ATC.level.5)
  eu_list_source <- EU_LIST_FILE
} else {
  eu_list_codes <- unique(c(
    read_csv(file.path(RAW_DIR, GERMANY_FILES[1]), col_types = cols(.default = "c"))$atc_code,
    read_csv(file.path(RAW_DIR, EMA_CURATED_FILE), show_col_types = FALSE)$atc_code,
    str_trim(unlist(str_split(
      read_csv(file.path(RAW_DIR, IRELAND_CURATED_FILE), show_col_types = FALSE)$matched_critical_atc,
      ",")))
  ))
  eu_list_source <- "union of the EU-list source files (critical.csv not found)"
}
eu_list_codes <- eu_list_codes[!is.na(eu_list_codes) & eu_list_codes != ""]
cat("EU Union list codes:", length(eu_list_codes), "from", eu_list_source,
    "| overlap with WHO EML:", sum(eu_list_codes %in% critical_codes), "\n")

# Codes kept in the main outputs: WHO EML (+ EU-list codes only if KEEP_NON_EML_CODES)
keep_code <- function(atc) is_critical(atc) | (KEEP_NON_EML_CODES & atc %in% eu_list_codes)
# Codes kept in the WHO + EU file (used for the EU / intersection robustness models)
keep_any  <- function(atc) is_critical(atc) | atc %in% eu_list_codes

# ------------------------------------------------------------------------------
# 2. EMA: curated EU-list rows + top-up from full export
# ------------------------------------------------------------------------------
ema_curated_raw <- read_csv(file.path(RAW_DIR, EMA_CURATED_FILE), show_col_types = FALSE) |>
  clean_names() |>
  filter(!is.na(atc_code), atc_code != "")

ema_curated <- ema_curated_raw |>
  transmute(
    atc_code          = atc_code,
    mfr_company       = str_squish(manufacturer_name),
    mfr_country       = country,
    manufacturer_step = as.integer(manufacturer_step),
    source            = "ema",
    data_origin       = "curated"
  )

ema_full <- read_delim(file.path(RAW_DIR, EMA_FULL_FILE), delim = ";", show_col_types = FALSE) |>
  clean_names() |>
  filter(!is.na(atc_code), atc_code != "") |>
  separate_rows(atc_code, sep = "\\s*[;,]\\s*") |>
  mutate(atc_code = str_squish(atc_code))

ema_topup_raw <- ema_full |>
  filter(is_critical(atc_code), !atc_code %in% ema_curated$atc_code)

ema_topup <- ema_topup_raw |>
  transmute(
    atc_code          = atc_code,
    mfr_company       = str_squish(manufacturer_name),
    mfr_country       = country,
    manufacturer_step = as.integer(manufacturer_step),
    source            = "ema",
    data_origin       = "topup"
  )

ema_std <- bind_rows(ema_curated, ema_topup)

# Per-source WHO file with the original EMA columns (used by the analysis scripts)
ema_cols <- c("medicine_name", "active_substance", "atc_code", "manufacturer_name",
              "address", "country", "manufacturer_step")
ema_who <- bind_rows(
  ema_curated_raw |> select(any_of(ema_cols)) |> mutate(data_origin = "curated"),
  ema_topup_raw   |> select(any_of(ema_cols)) |> mutate(data_origin = "topup")
) |>
  filter(keep_code(atc_code))
cat("ema_std rows:", nrow(ema_std), "(curated:", nrow(ema_curated),
    "| top-up:", nrow(ema_topup), "| new codes:", n_distinct(ema_topup$atc_code), ")\n")

# ------------------------------------------------------------------------------
# 3. Germany (BfArM): EU-list download (curated) + EML download (top-up)
# ------------------------------------------------------------------------------
germany_raw <- read_existing(GERMANY_FILES, \(f) read_csv(f, col_types = cols(.default = "c")) |>
                               clean_names()) |>
  bind_rows(.id = "de_file") |>
  mutate(data_origin = if_else(de_file == GERMANY_FILES[1], "curated", "topup")) |>
  # same product/role/company can come back from both searches
  distinct(atc_code, eingangsnummer, role, name, pu_nummer, plz_ort, .keep_all = TRUE)
cat("BfArM rows by file:\n"); print(table(germany_raw$de_file))

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
  "Zypern" = "Cyprus", "Österreich" = "Austria",
  "Estland" = "Estonia", "Luxemburg" = "Luxembourg", "Macao" = "Macao",
  "Malaysia" = "Malaysia", "Serbien" = "Serbia", "Thailand" = "Thailand"
)

missing_de_countries <- germany_raw |>
  filter(!is.na(land), land != "-", !land %in% names(de_to_en_country)) |>
  distinct(land) |>
  pull(land)

if (length(missing_de_countries) > 0) {
  cat("German country names not in de_to_en_country, will show as NA:\n")
  print(missing_de_countries)
}

germany_std <- germany_raw |>
  filter(!is.na(atc_code), atc_code != "") |>
  filter(role != "Zulassungsinhaber") |>
  filter(keep_any(atc_code)) |>
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
    source            = "germany",
    data_origin       = data_origin
  )

cat("germany_std rows:", nrow(germany_std), "\n")

# Per-source WHO file: both BfArM downloads, all original columns and roles
germany_who <- germany_raw |> filter(keep_code(atc_code))

# EML codes with no BfArM result in either download (not queried yet, or no
# German product found) -> list for a follow-up API query / check
germany_searched <- unique(c(germany_raw$search_atc, germany_raw$atc_code))
germany_to_query <- eml_codes |>
  filter(nchar(atc_code) == 7, !atc_code %in% germany_searched)
write_csv(germany_to_query, file.path(PROC_DIR, "germany_eml_codes_without_bfarm_result.csv"))
cat("WHO EML codes without any BfArM result (written to file):", nrow(germany_to_query), "\n")

# ------------------------------------------------------------------------------
# 4. Ireland (HPRA): curated EU-list rows + top-up from full export
# ------------------------------------------------------------------------------
split_trim <- function(x) {
  if (is.na(x)) return(NA_character_)
  str_squish(str_split(x, "\\|")[[1]])
}

# Input: one row per product with atc_code (comma-separated codes) and
# pipe-separated mfr_company / mfr_country
standardise_ireland <- function(df, origin) {
  pairs <- df |>
    filter(!is.na(atc_code), atc_code != "") |>
    separate_rows(atc_code, sep = ",\\s*") |>
    mutate(atc_code = str_trim(atc_code)) |>
    mutate(row_id = row_number()) |>
    mutate(
      company_list  = map(mfr_company, split_trim),
      country_list  = map(mfr_country, split_trim),
      lengths_match = map2_lgl(company_list, country_list, ~ length(.x) == length(.y))
    )
  
  matched <- pairs |>
    filter(lengths_match) |>
    select(row_id, atc_code, company_list, country_list) |>
    unnest(cols = c(company_list, country_list)) |>
    rename(mfr_company = company_list, mfr_country = country_list)
  
  unmatched <- pairs |>
    filter(!lengths_match) |>
    transmute(row_id, atc_code,
              mfr_company = str_squish(mfr_company),
              # "Belgium | Belgium" -> "Belgium"; mixed lists stay as they are
              mfr_country = map_chr(country_list, function(v) {
                u <- unique(v[!is.na(v) & v != ""])
                if (length(u) == 0) NA_character_ else paste(u, collapse = " | ")
              }))
  
  if (nrow(unmatched) > 0) {
    cat("Ireland (", origin, ") rows kept unsplit (company/country list length mismatch):",
        nrow(unmatched), "\n")
  }
  
  bind_rows(matched, unmatched) |>
    transmute(
      atc_code          = atc_code,
      mfr_company       = mfr_company,
      mfr_country       = mfr_country,
      manufacturer_step = NA_integer_,
      source            = "ireland",
      data_origin       = origin
    )
}

ireland_curated <- read_csv(file.path(RAW_DIR, IRELAND_CURATED_FILE), show_col_types = FALSE) |>
  clean_names() |>
  mutate(atc_code = matched_critical_atc) |>
  standardise_ireland("curated")

ireland_curated_codes <- unique(ireland_curated$atc_code)

ireland_topup <- hpra_all |>
  mutate(atc_code = extract_atc_codes(atc_code)) |>
  standardise_ireland("topup") |>
  filter(is_critical(atc_code), !atc_code %in% ireland_curated_codes)

ireland_std <- bind_rows(ireland_curated, ireland_topup)

# Per-source WHO file in the review-file layout (one row per product,
# matched_critical_atc = the product's WHO codes, comma-separated)
keep_codes_text <- function(x, exclude = character(0)) {
  map_chr(str_split(coalesce(x, ""), ",\\s*"), function(v) {
    v <- str_trim(v)
    v <- v[v != "" & keep_code(v) & !v %in% exclude]
    if (length(v) == 0) NA_character_ else paste(unique(v), collapse = ", ")
  })
}
ireland_cols <- c("product_id", "product_name", "licence_number", "licence_holder", "atc_code",
                  "matched_critical_atc", "mfr_company", "mfr_address", "mfr_country")
ireland_who <- bind_rows(
  read_csv(file.path(RAW_DIR, IRELAND_CURATED_FILE), show_col_types = FALSE) |>
    clean_names() |>
    mutate(matched_critical_atc = keep_codes_text(matched_critical_atc),
           data_origin = "curated") |>
    select(any_of(c(ireland_cols, "data_origin"))),
  hpra_all |>
    mutate(matched_critical_atc = keep_codes_text(extract_atc_codes(atc_code),
                                                  exclude = ireland_curated_codes),
           data_origin = "topup") |>
    select(any_of(c(ireland_cols, "data_origin")))
) |>
  filter(!is.na(matched_critical_atc))
cat("ireland_std rows:", nrow(ireland_std), "(curated:", nrow(ireland_curated),
    "| top-up:", nrow(ireland_topup), "| new codes:", n_distinct(ireland_topup$atc_code), ")\n")

# ------------------------------------------------------------------------------
# 5. CEP (EDQM): full file, filtered to WHO EML codes
# ------------------------------------------------------------------------------
cep_atc <- read_csv(file.path(RAW_DIR, CEP_FILE), show_col_types = FALSE) |>
  clean_names()

cat("CEP rows before splitting multi-code cells:", nrow(cep_atc), "\n")

cep_atc <- cep_atc |> filter(status_cep == "Valid")
cep_atc <- cep_atc |>
  filter(!is.na(atc_code), atc_code != "") |>
  separate_rows(atc_code, sep = ",\\s*")

cat("CEP rows after splitting multi-code cells:", nrow(cep_atc), "\n")

cep_atc <- cep_atc |>
  filter(keep_any(atc_code))
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
    source            = "cep",
    data_origin       = "full_register"
  )

cat("cep_std rows:", nrow(cep_std), "\n")

# ------------------------------------------------------------------------------
# 6. Combine
# ------------------------------------------------------------------------------
country_spelling <- c(
  "SPAIN" = "Spain", "UK" = "United Kingdom", "USA" = "United States",
  "The Netherlands" = "Netherlands", "Republic of Korea" = "South Korea"
)

# All rows for codes on the WHO EML and/or the EU list (flags say which)
registers_who_eu <- bind_rows(ema_std, germany_std, ireland_std, cep_std) |>
  mutate(
    on_who_eml      = is_critical(atc_code),
    on_eu_list      = atc_code %in% eu_list_codes,
    eml_match_code  = eml_match_code(atc_code)
  ) |>
  filter(keep_any(atc_code)) |>
  left_join(eml_codes, by = c("eml_match_code" = "atc_code")) |>
  select(-eml_match_code) |>
  mutate(
    mfr_country = str_remove(mfr_country, "\\.$"),
    mfr_country = coalesce(unname(country_spelling[mfr_country]), mfr_country),
    level1_code = str_extract(atc_code, "^[A-Z]")
  ) |>
  left_join(atc_level1, by = "level1_code")

# Main combined file: WHO EML codes only (unchanged)
manufacturer_registers <- registers_who_eu |> filter(keep_code(atc_code))

cat("Combined rows (WHO EML critical):", nrow(manufacturer_registers), "\n")
cat("Rows by source and origin:\n")
print(table(manufacturer_registers$source, manufacturer_registers$data_origin, useNA = "ifany"))

# Coverage of each EML code by source
eml_coverage <- eml_codes |>
  left_join(
    manufacturer_registers |>
      mutate(eml_code = eml_match_code(atc_code)) |>
      count(eml_code, source) |>
      pivot_wider(names_from = source, values_from = n, values_fill = 0, names_prefix = "n_"),
    by = c("atc_code" = "eml_code")
  ) |>
  mutate(across(starts_with("n_"), ~ coalesce(.x, 0L)),
         on_eu_list = atc_code %in% eu_list_codes)

cat("WHO EML codes with data from at least one source:",
    sum(rowSums(select(eml_coverage, starts_with("n_"))) > 0), "of", nrow(eml_coverage), "\n")

write_csv(eml_coverage, file.path(PROC_DIR, "who_eml_coverage_by_source.csv"))
write_csv(manufacturer_registers, file.path(PROC_DIR, "manufacturer_registers_combined.csv"))

# ------------------------------------------------------------------------------
# 7. Extra outputs for the analysis scripts
# ------------------------------------------------------------------------------
# WHO critical-code list. Use this (not unique(mfr$atc_code)) to filter other
# files such as GMDP or CEP: it also contains codes no register covers, and the
# level-5 codes found under the 5 level-4 EML codes (e.g. A07CA).
who_codes <- tibble(atc_code = sort(unique(c(eml_codes$atc_code,
                                             manufacturer_registers$atc_code)))) |>
  mutate(eml_code = eml_match_code(atc_code)) |>
  left_join(eml_codes, by = c("eml_code" = "atc_code"))
write_csv(who_codes, file.path(PROC_DIR, "who_eml_critical_codes.csv"))

# Per-source WHO files with original columns (same layout as the old EU-list raw files)
write_csv(germany_who, file.path(PROC_DIR, "germany_who_LONG.csv"))
write_csv(ema_who,     file.path(PROC_DIR, "ema_who.csv"))
write_csv(ireland_who, file.path(PROC_DIR, "ireland_who.csv"))

# WHO + EU file for the robustness models (02_shortage_models_all_lists.R)
write_csv(registers_who_eu, file.path(PROC_DIR, "manufacturer_registers_combined_WHO_EU.csv"))
cat("manufacturer_registers_combined_WHO_EU.csv:", nrow(registers_who_eu), "rows |",
    "codes on WHO list:", n_distinct(registers_who_eu$atc_code[registers_who_eu$on_who_eml]),
    "| on EU list:", n_distinct(registers_who_eu$atc_code[registers_who_eu$on_eu_list]),
    "| on both:", n_distinct(registers_who_eu$atc_code[registers_who_eu$on_who_eml &
                                                         registers_who_eu$on_eu_list]), "\n")

cat("Written to", PROC_DIR, ": manufacturer_registers_combined.csv, who_eml_critical_codes.csv (",
    nrow(who_codes), "codes ), germany_who_LONG.csv (", nrow(germany_who), "rows ),",
    "ema_who.csv (", nrow(ema_who), "rows ), ireland_who.csv (", nrow(ireland_who), "products )\n")