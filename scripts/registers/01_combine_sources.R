## =================================================================
## 01_combine_sources.R
## Combine the manufacturer registers for BOTH critical-medicine lists:
##   - EU Union list of critical medicines   (Data/critical.csv)
##   - WHO Model List of Essential Medicines (Data/eml_export.xlsx)
## into one table, Data/manufacturer_registers_combined.csv, with one
## row per ATC code x manufacturer x source and two flags:
##   on_eu_list, on_who_eml
## The analysis scripts pick EU, WHO or the overlap (on both lists)
## with their LIST setting.
##
## Sources: EMA (EPAR), Germany (BfArM), Ireland (HPRA), CEP (EDQM),
## GMDP (EudraGMDP API GMP certificates).
## For each source, the rows collected for the EU list are used first
## ("curated"). Codes that are ONLY on the WHO list are filled in from
## that source's WHO / full file ("topup"). Codes on the EU list always
## use the EU files only (TOPUP_EU_CODES), so EU and overlap results are
## the same as in the EU-only analysis. A missing top-up file is skipped
## with a message.
##
## Also writes (Data folder):
##   list_codes.csv                         every code on either list + flags (treemaps)
##   who_eml_coverage_by_source.csv         rows per WHO EML code and source
##   who_eml_medicines_without_atc.csv      EML medicines without an ATC code
##   germany_eml_codes_without_bfarm_result.csv
## =================================================================
library(readr)
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(tidyr)
library(purrr)

DATA_DIR <- "Data"

## ---- input files (all in DATA_DIR) -----------------------------------
EU_LIST_FILE         <- "critical.csv"
EML_FILE             <- "eml_export.xlsx"
EMA_CURATED_FILE     <- "EMA_data_critical.csv"                    # EU list
EMA_FULL_FILE        <- "data.csv"                                 # full EMA export (;) -> WHO top-up
GERMANY_FILES        <- c(curated = "bfarm_api_origin_critical_rest_LONG.csv",
                          topup   = "bfarm_api_origin_eml_LONG.csv")
IRELAND_CURATED_FILE <- "ireland_critical_atc_review.csv"          # EU list, reviewed
IRELAND_FULL_FILE    <- "hpra_final.csv"                           # full HPRA export -> WHO top-up
CEP_FILE             <- "EXPORT_WEB_CEP_with_ATC_drugbank.csv"
GMDP_FILES           <- c(curated = "Critical_API_sites_activities_LONG.csv",
                          topup   = "gmdp_EML_API_sites_activities_LONG.csv")

## ---- settings --------------------------------------------------------
## WHO EML: TRUE = only ATC codes given on current ("Added") EML rows.
## FALSE = also codes from "Removed" rows of a current medicine and, with
## MAP_EML_ALTERNATIVES_BY_NAME, an exact INN lookup in the HPRA export.
EML_DIRECT_ONLY              <- TRUE
MAP_EML_ALTERNATIVES_BY_NAME <- FALSE

## GMDP: keep a site for a code only if it makes or processes the substance
## (EU GMP API-certificate sections 3.1-3.4 or 3.5.1); packaging-only
## (3.5.2-3.5.4) and testing-only (3.6.x) sites are dropped.
GMDP_MANUFACTURING_ONLY <- TRUE
gmdp_production_codes   <- "^3\\.(1|2|3|4)\\.|^3\\.5\\.1$"

## Top-up files are used only for codes that are on the WHO list but NOT
## on the EU list (FALSE, default). TRUE also fills EU-list codes the EU
## files miss (e.g. 24 extra EMA codes in data.csv), which changes the EU
## results.
TOPUP_EU_CODES <- FALSE

in_data <- function(f) file.path(DATA_DIR, f)
missing_topup_files <- character(0)
have    <- function(f) {
  ok <- file.exists(in_data(f))
  if (!ok) {
    cat("NOTE:", f, "not found in", DATA_DIR, "-- skipped (WHO codes from this file missing)\n")
    missing_topup_files <<- union(missing_topup_files, f)
  }
  ok
}

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

atc5_regex <- "\\b[A-Z][0-9]{2}[A-Z]{2}[0-9]{2}\\b"
atc4_regex <- "\\b[A-Z][0-9]{2}[A-Z]{2}\\b"

## HPRA ATC text, e.g. "R06AA Aminoalkyl ethers, R06AA52 diphenhydramine, combinations".
## Level-5 codes are used; level-4 codes only when a product has no level-5 code.
extract_atc_codes <- function(x) {
  map_chr(x, function(s) {
    if (is.na(s) || s == "") return(NA_character_)
    codes <- unique(str_extract_all(s, atc5_regex)[[1]])
    if (length(codes) == 0) codes <- unique(str_extract_all(s, atc4_regex)[[1]])
    if (length(codes) == 0) NA_character_ else paste(codes, collapse = ", ")
  })
}

## =================================================================
## 1. The two lists
## =================================================================
## ---- EU Union list ----
eu_codes <- read.csv(in_data(EU_LIST_FILE), stringsAsFactors = FALSE)$ATC.level.5
eu_codes <- unique(str_trim(eu_codes[!is.na(eu_codes) & str_trim(eu_codes) != ""]))
cat("EU Union list codes:", length(eu_codes), "\n")

## ---- WHO EML ----
eml_raw <- read_excel(in_data(EML_FILE)) |>
  clean_names() |>
  mutate(medicine_name = str_squish(medicine_name),
         medicine_key  = str_to_lower(medicine_name))

## a medicine is on the current list if it has at least one "Added" row
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

eml_atc_direct <- split_eml_atc(eml_current) |> mutate(eml_atc_origin = "eml_direct")

## current medicines whose ATC code only appears on a "Removed" row (e.g. atenolol)
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

## full HPRA export: Ireland top-up and the optional EML INN -> ATC lookup
hpra_all <- if (have(IRELAND_FULL_FILE)) {
  read_csv(in_data(IRELAND_FULL_FILE), show_col_types = FALSE) |>
    clean_names() |>
    distinct(product_id, .keep_all = TRUE)
} else NULL

if (!EML_DIRECT_ONLY && MAP_EML_ALTERNATIVES_BY_NAME && nrow(eml_no_atc) > 0 && !is.null(hpra_all)) {
  hpra_atc_names <- hpra_all |>
    distinct(atc_code) |>
    filter(!is.na(atc_code)) |>
    mutate(pair = str_match_all(
      atc_code, "([A-Z][0-9]{2}[A-Z]{2}[0-9]{2}) (.+?)(?=, [A-Z][0-9]{2}[A-Z]{2}|$)")) |>
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
  write_csv(eml_atc_lookup, in_data("who_eml_atc_name_lookup_REVIEW.csv"))
  cat("EML medicines without ATC mapped by INN name (review file written):",
      n_distinct(eml_atc_lookup$medicine_key), "\n")
}

write_csv(eml_no_atc, in_data("who_eml_medicines_without_atc.csv"))
cat("EML medicines without an ATC code (excluded, listed in file):", nrow(eml_no_atc), "\n")

## one row per EML code; if several origins, keep the most direct one
eml_codes <- eml_atc |>
  mutate(origin_rank = match(eml_atc_origin,
                             c("eml_direct", "eml_other_row_same_medicine", "name_lookup_hpra"))) |>
  group_by(atc_code) |>
  summarise(eml_medicine   = paste(sort(unique(medicine_name)), collapse = "; "),
            eml_atc_origin = eml_atc_origin[which.min(origin_rank)],
            .groups = "drop")

who_codes   <- eml_codes$atc_code
who_codes_4 <- who_codes[nchar(who_codes) == 5]
cat("WHO EML codes:", length(who_codes), "(level 5:", sum(nchar(who_codes) == 7),
    "| level 4:", length(who_codes_4), ")\n")

## a register code is on the WHO list if it is an EML code or sits under an
## EML level-4 code (e.g. A07CA)
on_who <- function(atc) !is.na(atc) & (atc %in% who_codes | str_sub(atc, 1, 5) %in% who_codes_4)
on_eu  <- function(atc) !is.na(atc) & atc %in% eu_codes
on_any <- function(atc) on_eu(atc) | on_who(atc)
## EML code a register code matched (for joining the EML medicine name)
eml_match_code <- function(atc) {
  if_else(atc %in% who_codes, atc,
          if_else(str_sub(atc, 1, 5) %in% who_codes_4, str_sub(atc, 1, 5), NA_character_))
}

cat("EU list codes also on the WHO EML:", sum(on_who(eu_codes)), "\n")

## =================================================================
## 2. EMA (EPAR): EU-list file + top-up from the full export
## =================================================================
ema_curated_raw <- read_csv(in_data(EMA_CURATED_FILE), show_col_types = FALSE) |>
  clean_names() |>
  filter(!is.na(atc_code), atc_code != "")

ema_topup_raw <- if (have(EMA_FULL_FILE)) {
  read_delim(in_data(EMA_FULL_FILE), delim = ";", show_col_types = FALSE) |>
    clean_names() |>
    filter(!is.na(atc_code), atc_code != "") |>
    separate_rows(atc_code, sep = "\\s*[;,]\\s*") |>
    mutate(atc_code = str_squish(atc_code)) |>
    filter(on_who(atc_code), !atc_code %in% ema_curated_raw$atc_code)
} else ema_curated_raw[0, ]

ema_std <- bind_rows(
  ema_curated_raw |> mutate(data_origin = "curated"),
  ema_topup_raw   |> mutate(data_origin = "topup")
) |>
  transmute(
    atc_code          = atc_code,
    mfr_company       = str_squish(manufacturer_name),
    mfr_country       = country,
    manufacturer_step = as.integer(manufacturer_step),
    source            = "ema",
    data_origin
  )

cat("EMA rows:", nrow(ema_std), "| top-up codes:", n_distinct(ema_topup_raw$atc_code), "\n")

## =================================================================
## 3. Germany (BfArM): EU-list download + WHO EML download
## =================================================================
germany_files <- GERMANY_FILES[c(TRUE, have(GERMANY_FILES[["topup"]]))]

germany_raw <- imap(germany_files, \(f, origin)
                    read_csv(in_data(f), col_types = cols(.default = "c")) |>
                      clean_names() |>
                      mutate(data_origin = origin)) |>
  bind_rows() |>
  ## the same product/role/company can come back from both searches
  distinct(atc_code, eingangsnummer, role, name, pu_nummer, plz_ort, .keep_all = TRUE)
cat("BfArM rows by file:\n"); print(table(germany_raw$data_origin))

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
  distinct(land) |> pull(land)
if (length(missing_de_countries) > 0) {
  cat("German country names not in de_to_en_country, will show as NA:\n")
  print(missing_de_countries)
}

germany_std <- germany_raw |>
  filter(!is.na(atc_code), atc_code != "", role != "Zulassungsinhaber") |>
  transmute(
    atc_code          = atc_code,
    mfr_company       = name,
    mfr_country       = unname(de_to_en_country[land]),
    manufacturer_step = case_when(role == "Wirkstoffherstellung"   ~ 1L,
                                  role == "Hersteller/Endfreigabe" ~ 2L,
                                  TRUE                             ~ NA_integer_),
    source            = "germany",
    data_origin
  )
cat("Germany rows:", nrow(germany_std), "\n")

## WHO codes with no BfArM result in either download -> list for a follow-up query
germany_to_query <- eml_codes |>
  filter(nchar(atc_code) == 7, !atc_code %in% c(germany_raw$search_atc, germany_raw$atc_code))
write_csv(germany_to_query, in_data("germany_eml_codes_without_bfarm_result.csv"))
cat("WHO EML codes without any BfArM result (written to file):", nrow(germany_to_query), "\n")

## =================================================================
## 4. Ireland (HPRA): EU-list review file + top-up from the full export
## =================================================================
split_trim <- function(x) {
  if (is.na(x)) return(NA_character_)
  str_squish(str_split(x, "\\|")[[1]])
}

## one row per product: atc_code (comma-separated) and "|"-separated
## mfr_company / mfr_country lists, paired up where the lengths match
standardise_ireland <- function(df, origin) {
  pairs <- df |>
    filter(!is.na(atc_code), atc_code != "") |>
    separate_rows(atc_code, sep = ",\\s*") |>
    mutate(atc_code      = str_trim(atc_code),
           row_id        = row_number(),
           company_list  = map(mfr_company, split_trim),
           country_list  = map(mfr_country, split_trim),
           lengths_match = map2_lgl(company_list, country_list, ~ length(.x) == length(.y)))

  matched <- pairs |>
    filter(lengths_match) |>
    select(row_id, atc_code, company_list, country_list) |>
    unnest(cols = c(company_list, country_list)) |>
    rename(mfr_company = company_list, mfr_country = country_list)

  ## lists that don't line up stay one row; "Belgium | Belgium" -> "Belgium"
  unmatched <- pairs |>
    filter(!lengths_match) |>
    transmute(row_id, atc_code,
              mfr_company = str_squish(mfr_company),
              mfr_country = map_chr(country_list, function(v) {
                u <- unique(v[!is.na(v) & v != ""])
                if (length(u) == 0) NA_character_ else paste(u, collapse = " | ")
              }))
  if (nrow(unmatched) > 0)
    cat("Ireland (", origin, ") rows kept unsplit (company/country list length mismatch):",
        nrow(unmatched), "\n")

  bind_rows(matched, unmatched) |>
    transmute(atc_code, mfr_company, mfr_country,
              manufacturer_step = NA_integer_, source = "ireland", data_origin = origin)
}

ireland_curated <- read_csv(in_data(IRELAND_CURATED_FILE), show_col_types = FALSE) |>
  clean_names() |>
  mutate(atc_code = matched_critical_atc) |>
  standardise_ireland("curated")

ireland_topup <- if (!is.null(hpra_all)) {
  hpra_all |>
    mutate(atc_code = extract_atc_codes(atc_code)) |>
    standardise_ireland("topup") |>
    filter(on_who(atc_code), !atc_code %in% ireland_curated$atc_code)
} else ireland_curated[0, ]

ireland_std <- bind_rows(ireland_curated, ireland_topup)
cat("Ireland rows:", nrow(ireland_std), "| top-up codes:", n_distinct(ireland_topup$atc_code), "\n")

## =================================================================
## 5. CEP (EDQM): full register, valid certificates
## =================================================================
cep_atc <- read_csv(in_data(CEP_FILE), show_col_types = FALSE) |>
  clean_names() |>
  filter(status_cep == "Valid", !is.na(atc_code), atc_code != "") |>
  separate_rows(atc_code, sep = ",\\s*") |>
  mutate(atc_code       = str_trim(atc_code),
         holder_country = str_extract(certificate_cep_holder, "[A-Z]{2}$")) |>
  filter(on_any(atc_code))

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

missing_codes <- setdiff(unique(na.omit(cep_atc$holder_country)), names(iso2_to_name))
if (length(missing_codes) > 0) {
  cat("CEP country codes not in iso2_to_name, will show as NA:\n"); print(missing_codes)
}

cep_std <- cep_atc |>
  transmute(atc_code,
            mfr_company       = certificate_cep_holder,
            mfr_country       = unname(iso2_to_name[holder_country]),
            manufacturer_step = NA_integer_,
            source            = "cep",
            data_origin       = "full_register")
cat("CEP rows:", nrow(cep_std), "\n")

## =================================================================
## 6. GMDP (EudraGMDP API sites): EU-list file + WHO EML file
##    Long files (one row per site x certificate x activity) are collapsed
##    to one row per ATC code x site. The is_critical column in the files
##    is not used; list membership comes from the flags below.
## =================================================================
gmdp_files <- GMDP_FILES[c(TRUE, have(GMDP_FILES[["topup"]]))]

gmdp_raw <- imap(gmdp_files, \(f, origin)
                 read_csv(in_data(f), show_col_types = FALSE,
                          col_types = cols(activity_code = col_character())) |>
                   clean_names() |>
                   mutate(data_origin = origin)) |>
  bind_rows()

gmdp_sites <- gmdp_raw |>
  filter(!is.na(atc), atc != "", is.na(withdrawn) | !withdrawn, cert_type == "API") |>
  separate_rows(atc, sep = "\\s*[;,]\\s*") |>               # "V03AH01;C02DA01" -> 2 rows
  mutate(atc       = str_trim(atc),
         site_name = str_squish(site_name),
         is_mfg    = str_detect(activity_code, gmdp_production_codes)) |>
  filter(on_any(atc)) |>
  group_by(data_origin, atc, site_name, country) |>
  summarise(any_mfg = any(is_mfg, na.rm = TRUE), inn = first(inn), .groups = "drop")

if (GMDP_MANUFACTURING_ONLY) gmdp_sites <- filter(gmdp_sites, any_mfg)

gmdp_std <- gmdp_sites |>
  ## pooled files: keep each code x site once, preferring the curated row
  arrange(atc, site_name, country, data_origin != "curated") |>
  distinct(atc, site_name, country, .keep_all = TRUE) |>
  transmute(atc_code          = atc,
            mfr_company       = site_name,
            mfr_country       = country,
            manufacturer_step = 1L,      # API (active substance) certificates
            source            = "gmdp",
            data_origin)
cat("GMDP code x site rows:", nrow(gmdp_std), "| by file:\n"); print(table(gmdp_std$data_origin))

## =================================================================
## 7. One substance name per ATC code (to label plots), in this order:
##    GMDP INN, EMA active substance, CEP substance, WHO EML medicine,
##    BfArM atc_text (German; full ATC paths reduced to the last segment)
## =================================================================
germany_name <- germany_raw |>
  distinct(atc_code, name = str_squish(atc_text)) |>
  mutate(name = if_else(str_detect(name, "^[A-Z][0-9]{2}[A-Z]{2}[0-9]{2}:"),
                        str_trim(str_extract(name, "[^-]+$")), name))

substance_lookup <- bind_rows(
  gmdp_sites |> distinct(atc_code = atc, name = str_to_sentence(str_squish(inn))) |> mutate(priority = 1L),
  bind_rows(ema_curated_raw, ema_topup_raw) |>
    distinct(atc_code, name = str_squish(active_substance))                       |> mutate(priority = 2L),
  cep_atc  |> distinct(atc_code, name = str_squish(substance))                    |> mutate(priority = 3L),
  eml_codes |> filter(nchar(atc_code) == 7) |>
    distinct(atc_code, name = str_to_sentence(eml_medicine))                      |> mutate(priority = 4L),
  germany_name |> mutate(priority = 5L)
) |>
  filter(!is.na(atc_code), !is.na(name), name != "") |>
  arrange(atc_code, priority, name) |>
  distinct(atc_code, .keep_all = TRUE) |>
  select(atc_code, substance = name)

## =================================================================
## 8. Combine and flag
## =================================================================
country_spelling <- c(
  "SPAIN" = "Spain", "UK" = "United Kingdom", "USA" = "United States",
  "The Netherlands" = "Netherlands", "Republic of Korea" = "South Korea",
  "Korea, Republic of" = "South Korea", "Czechia" = "Czech Republic",
  "United Kingdom (Northern Ireland)" = "United Kingdom"
)

## ---- manufacturer region (used by the shortage models) -------------------
## EU/EEA is the reference group in the models. Groups follow the UN M49
## regions, except: Puerto Rico (US territory) counts as North America;
## Africa and Latin America (incl. Mexico) are too small on their own
## (5 and ~50 entries) and are pooled as "Rest of world".
eu_eea_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark",
  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy",
  "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal",
  "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Iceland", "Norway", "Liechtenstein")
region_of_country <- c(
  setNames(rep("EU/EEA", length(eu_eea_countries)), eu_eea_countries),
  "China" = "China",
  "India" = "India",
  ## other Asia (incl. Western Asia) and Oceania
  "Japan" = "Other Asia & Oceania", "South Korea" = "Other Asia & Oceania",
  "Taiwan" = "Other Asia & Oceania", "Singapore" = "Other Asia & Oceania",
  "Malaysia" = "Other Asia & Oceania", "Thailand" = "Other Asia & Oceania",
  "Indonesia" = "Other Asia & Oceania", "Hong Kong" = "Other Asia & Oceania",
  "Macao" = "Other Asia & Oceania", "Pakistan" = "Other Asia & Oceania",
  "Israel" = "Other Asia & Oceania", "Turkey" = "Other Asia & Oceania",
  "Jordan" = "Other Asia & Oceania", "Saudi Arabia" = "Other Asia & Oceania",
  "United Arab Emirates" = "Other Asia & Oceania", "Oman" = "Other Asia & Oceania",
  "Australia" = "Other Asia & Oceania", "New Zealand" = "Other Asia & Oceania",
  "Marshall Islands" = "Other Asia & Oceania",
  ## North America
  "United States" = "North America", "Canada" = "North America",
  "Puerto Rico" = "North America",
  ## Europe outside the EU/EEA
  "United Kingdom" = "Non-EU Europe", "Switzerland" = "Non-EU Europe",
  "Ukraine" = "Non-EU Europe", "Serbia" = "Non-EU Europe", "Monaco" = "Non-EU Europe",
  "Russia" = "Non-EU Europe",
  ## everything else outside Europe: Latin America and Africa
  "Mexico" = "Rest of world", "Argentina" = "Rest of world", "Brazil" = "Rest of world",
  "Chile" = "Rest of world", "Colombia" = "Rest of world",
  "South Africa" = "Rest of world", "Morocco" = "Rest of world"
)

manufacturer_registers <- bind_rows(ema_std, germany_std, ireland_std, cep_std, gmdp_std) |>
  filter(on_any(atc_code)) |>
  ## EU-list codes use the EU files only (see TOPUP_EU_CODES)
  filter(TOPUP_EU_CODES | !(data_origin == "topup" & on_eu(atc_code))) |>
  mutate(
    on_eu_list  = on_eu(atc_code),
    on_who_eml  = on_who(atc_code),
    mfr_country = str_remove(mfr_country, "\\.$"),
    mfr_country = coalesce(unname(country_spelling[mfr_country]), mfr_country),
    mfr_region  = unname(region_of_country[mfr_country]),
    level1_code = str_extract(atc_code, "^[A-Z]"),
    eml_code    = eml_match_code(atc_code)
  ) |>
  left_join(atc_level1, by = "level1_code") |>
  left_join(select(eml_codes, eml_code = atc_code, eml_medicine), by = "eml_code") |>
  select(-eml_code) |>
  left_join(substance_lookup, by = "atc_code")

cat("\nCombined rows:", nrow(manufacturer_registers), "\n")
cat("Codes on EU list:", n_distinct(manufacturer_registers$atc_code[manufacturer_registers$on_eu_list]),
    "| on WHO EML:", n_distinct(manufacturer_registers$atc_code[manufacturer_registers$on_who_eml]),
    "| on both:", n_distinct(manufacturer_registers$atc_code[manufacturer_registers$on_eu_list &
                                                             manufacturer_registers$on_who_eml]), "\n")
cat("Rows by source and origin:\n")
print(table(manufacturer_registers$source, manufacturer_registers$data_origin, useNA = "ifany"))

## countries without a region (add them to region_of_country); Ireland's
## unsplit "A | B" country lists also have no region, as before
no_region <- manufacturer_registers |>
  filter(!is.na(mfr_country), is.na(mfr_region), !str_detect(mfr_country, "\\|")) |>
  distinct(mfr_country) |> pull(mfr_country)
if (length(no_region) > 0) {
  cat("Countries without a region (add to region_of_country):\n"); print(no_region)
}
cat("Rows by region (Germany, EMA, CEP, GMDP):\n")
print(table(manufacturer_registers$mfr_region[manufacturer_registers$source != "ireland"],
            useNA = "ifany"))

write_csv(manufacturer_registers, in_data("manufacturer_registers_combined.csv"))

## =================================================================
## 9. Extra outputs
## =================================================================
## every code on either list, with flags (used by the treemaps); WHO level-4
## codes stay as given in the EML
## in_eml = code given in the EML itself (540 codes, incl. 5 level-4 codes);
## on_who_eml also counts EU codes that sit under an EML level-4 code
list_codes <- tibble(atc_code = sort(unique(c(eu_codes, who_codes)))) |>
  mutate(on_eu_list = on_eu(atc_code),
         in_eml     = atc_code %in% who_codes,
         on_who_eml = in_eml | (on_eu_list & on_who(atc_code))) |>
  left_join(select(eml_codes, atc_code, eml_medicine), by = "atc_code")
write_csv(list_codes, in_data("list_codes.csv"))
cat("list_codes.csv: EU", sum(list_codes$on_eu_list), "| WHO", sum(list_codes$atc_code %in% who_codes),
    "| on both", sum(list_codes$on_eu_list & list_codes$on_who_eml), "\n")

## rows per WHO EML code and source
eml_coverage <- eml_codes |>
  left_join(
    manufacturer_registers |>
      mutate(eml_code = eml_match_code(atc_code)) |>
      filter(!is.na(eml_code)) |>
      count(eml_code, source) |>
      pivot_wider(names_from = source, values_from = n, values_fill = 0, names_prefix = "n_"),
    by = c("atc_code" = "eml_code")) |>
  mutate(across(starts_with("n_"), ~ coalesce(.x, 0L)),
         on_eu_list = on_eu(atc_code))
write_csv(eml_coverage, in_data("who_eml_coverage_by_source.csv"))
cat("WHO EML codes with data from at least one source:",
    sum(rowSums(select(eml_coverage, starts_with("n_"))) > 0), "of", nrow(eml_coverage), "\n")

## ---- repeat any missing WHO file at the very end, where it is seen ----
if (length(missing_topup_files) > 0) {
  cat("\n", strrep("!", 70), "\n",
      "WARNING: these WHO files are missing from ", DATA_DIR, "/:\n   ",
      paste(missing_topup_files, collapse = "\n   "), "\n",
      "Medicines that are only on the WHO list get no data from them, so the\n",
      "WHO results are incomplete (close to the overlap list). Add the files\n",
      "and rerun this script.\n", strrep("!", 70), "\n", sep = "")
}
