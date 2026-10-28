## =================================================================
## Does manufacturer concentration predict whether a critical medicine
## is currently on Germany's shortage list (BfArM "Lieferengpass"
## register)?
##
## Inputs (all in the Data folder):
##   - manufacturer_registers_combined.csv   (built by 01_combine_sources.R:
##                                             EPAR/Germany/Ireland/CEP/GMDP,
##                                             critical ATC codes only).
##                                             The only manufacturer input:
##                                             V1, V2 and V3 all come from it.
##   - LEMeldungen_2026-09-09-13-30-43.csv   (BfArM shortage reports,
##                                             semicolon-delimited,
##                                             Latin-1 encoded)
##
## What this script does:
##   1. Builds a shortage flag per critical ATC code from the LE file.
##   2. Builds manufacturer-concentration metrics per ATC code, at
##      four levels: EPAR-only (source == "ema" -- this is the basis
##      for the paper's ">25 single-source" claim, "according to EU
##      authorizations"), V1 = Germany-only (source == "germany"),
##      V2 = Germany + EPAR combined/deduplicated, and
##      V3 = all sources: Germany + EPAR + CEP + EudraGMDP API sites
##      (name-harmonised so the same firm listed in several registers
##      counts once).
##   3. Cross-tabs single-source status against shortage status
##      (reproduces your "1 of 25" observation and tests whether it's
##      more than chance).
##   4. Runs univariate tests (Wilcoxon, point-biserial correlation)
##      and a multivariate logistic regression of shortage status on
##      number of manufacturers, country-level HHI, and share of
##      manufacturers in China/India ("main" model).
##   5. Fits a "regional" model with the China+India share split into
##      China, India, other Asia & Oceania, North America, non-EU Europe
##      and rest of world (EU/EEA = reference), for V1, V2 and V3.
##   6. Makes a few diagnostic plots.
##
## Base register: Germany (BfArM), always, when you run this script.
## Ireland_Shortage.R runs it with the Irish register (HPRA) instead, in a
## separate environment so the Ireland setting never carries over to the
## next run; its output files contain _Ireland_<LIST>.
##
## Shortage outcome: Germany (BfArM), always, when you run this script.
## GermanyEMA_Shortage.R runs it with OUTCOME = "GermanyEMA": a code counts
## as in shortage if it is on the German BfArM register OR on the EMA
## shortage list (Medicines_output_shortages_en.json, matched to ATC codes
## through Data/ema_shortages_atc_crosswalk.csv). Same separate-environment
## approach as Ireland; output files contain _GermanyEMA_<LIST> and go to
## Results/GermanyEMAShortage.
## OUTCOME = "GermanyEMAall" is the same, but counts EVERY code on the EMA
## list, whatever its status (ongoing, resolved, discontinued) and dates;
## the German part stays "active on the reference date". Outputs contain
## _GermanyEMAall_<LIST> and go to Results/GermanyEMAallShortage.
##
## Outputs (Results/<LIST> folder, file names end in _<LIST>, e.g. _WHO):
##   shortage_models_V1_V2_V3.csv   all model results, main + regional, each
##                                  also with ATC chapter dummies (main_atc,
##                                  regional_atc)
##   shortage_region_counts.csv     codes with a manufacturer per region
##   atc_shortage_concentration_analysis.csv, plot_v*.png
## GermanyEMA stream only: ema_shortage_atc_matches.csv (EMA record -> ATC
##   code, status, active on the reference date, in the critical list)
## =================================================================


library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(broom)
library(ggplot2)
library(scales)
library(car)    # vif()
library(pROC)   # roc()/auc()

## Minimal stand-in for janitor::clean_names() (avoids the extra
## dependency): lowercases, replaces non-alphanumeric runs with "_",
## trims leading/trailing "_".
clean_names_simple <- function(df) {
  nm <- names(df)
  nm <- str_to_lower(nm)
  nm <- str_replace_all(nm, "[^a-z0-9]+", "_")
  nm <- str_replace_all(nm, "^_|_$", "")
  names(df) <- nm
  df
}

## -----------------------------------------------------------------
## 0. Paths (relative to the project folder that contains Data/)
## -----------------------------------------------------------------
## ---- which list -------------------------------------------------------
## "EU" = EU Union list, "WHO" = WHO EML, "overlap" = on both lists.
## run_all_lists.R sets LIST and runs this script once per list.
if (!exists("LIST")) LIST <- "EU"
DATA_DIR <- "Data"
OUT_DIR  <- file.path("Results", LIST)          # outputs go to Results/EU, /WHO, /overlap

## ---- base register ------------------------------------------------------
## The national register V1-V3 are built on: "Germany" (BfArM, default) or
## "Ireland" (HPRA; Ireland_Shortage.R sets this). V1 = base register,
## V2 = base + EPAR, V3 = base + EPAR + CEP + GMDP. The shortage outcome is the
## German BfArM register in both cases.
## Germany unless Ireland_Shortage.R set BASE in this script's own
## environment: a BASE left over in the R session (from an older
## Ireland_Shortage.R or set by hand) must not turn a Germany run into Ireland
if (identical(environment(), globalenv()) || !exists("BASE", inherits = FALSE))
  BASE <- "Germany"
base_source <- c(Germany = "germany", Ireland = "ireland")[BASE]
if (is.na(base_source)) stop('BASE must be "Germany" or "Ireland"')
lbl <- function(x) gsub("Germany", BASE, x, fixed = TRUE)   # labels follow the base

## ---- shortage outcome ---------------------------------------------------
## "Germany"    = on the German BfArM register (default)
## "GermanyEMA" = on the German BfArM register OR on the EMA shortage list
##                (active on the reference date; GermanyEMA_Shortage.R sets this)
## "GermanyEMAall" = German BfArM register OR ever on the EMA shortage list
##                (all EMA records, status and dates ignored)
## Same guard as BASE: a leftover OUTCOME in the R session must not turn a
## plain Germany run into a GermanyEMA run
if (identical(environment(), globalenv()) || !exists("OUTCOME", inherits = FALSE))
  OUTCOME <- "Germany"
if (!OUTCOME %in% c("Germany", "GermanyEMA", "GermanyEMAall"))
  stop('OUTCOME must be "Germany", "GermanyEMA" or "GermanyEMAall"')
uses_ema <- OUTCOME %in% c("GermanyEMA", "GermanyEMAall")
outcome_label <- c(Germany = "German BfArM register",
                   GermanyEMA = "German BfArM register or EMA shortage list",
                   GermanyEMAall = "German BfArM register or ever on EMA shortage list (all statuses)")[[OUTCOME]]

## EMA settings (used only by the GermanyEMA outcome; GermanyEMAall counts all)
## Count EMA records with status "Medicine discontinued" as a shortage?
## Default FALSE: a discontinuation is a permanent withdrawal of one product,
## not a supply shortage of the medicine
if (!exists("EMA_COUNT_DISCONTINUED", inherits = FALSE)) EMA_COUNT_DISCONTINUED <- FALSE

## output file name with base (if not Germany), outcome (if not Germany)
## and list added:
##   out_file("plot.png") -> Results/WHO/plot_WHO.png, plot_Ireland_WHO.png,
##                           or plot_GermanyEMA_WHO.png
file_tag <- paste0(if (BASE != "Germany") paste0("_", BASE),
                   if (OUTCOME != "Germany") paste0("_", OUTCOME), "_", LIST)
## Ireland: all lists in one separate folder, Results/IrelandShortage;
## GermanyEMA outcome: Results/GermanyEMAShortage (both: IrelandShortage_GermanyEMA)
if (BASE == "Ireland") OUT_DIR <- file.path("Results", "IrelandShortage")
if (uses_ema)
  OUT_DIR <- if (BASE == "Ireland") file.path("Results", paste0("IrelandShortage_", OUTCOME)) else
    file.path("Results", paste0(OUTCOME, "Shortage"))
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
out_file <- function(name) file.path(OUT_DIR, sub("(\\.[A-Za-z]+)$", paste0(file_tag, "\\1"), name))

keep_list <- function(df) {
  switch(LIST,
         EU      = dplyr::filter(df, on_eu_list),
         WHO     = dplyr::filter(df, on_who_eml),
         overlap = dplyr::filter(df, on_eu_list, on_who_eml),
         stop('LIST must be "EU", "WHO" or "overlap"'))
}
cat("List:", LIST, "-> outputs in", OUT_DIR, "
")
cat("Shortage outcome:", outcome_label, "\n")
path_mfr       <- file.path(DATA_DIR, "manufacturer_registers_combined.csv")
path_shortages <- file.path(DATA_DIR, "LEMeldungen_2026-09-09-13-30-43.csv")
path_ema       <- file.path(DATA_DIR, "Medicines_output_shortages_en.json")
path_ema_atc   <- file.path(DATA_DIR, "ema_shortages_atc_crosswalk.csv")

## -----------------------------------------------------------------
## 1. Load the manufacturer register (already critical-ATC-filtered)
## -----------------------------------------------------------------
cat("Running", "Shortage.R", "-- base register:", BASE, "-- version 2026-10-20: main + regional models (China, India,",
    "other Asia & Oceania, North America, non-EU Europe, rest of world)\n")

if (!"mfr_region" %in% names(readr::read_csv(path_mfr, n_max = 1, show_col_types = FALSE)))
  stop("manufacturer_registers_combined.csv has no mfr_region column. ",
       "Rerun the current 01_combine_sources.R first (or run_all_lists.R with ",
       "REBUILD_COMBINED_DATA <- TRUE).", call. = FALSE)

mfr <- read_csv(path_mfr, show_col_types = FALSE) |>
  keep_list() |>
  mutate(
    mfr_company = str_squish(mfr_company),
    mfr_country = str_squish(mfr_country)
  )

cat("Manufacturer register rows:", nrow(mfr), "\n")
cat("Unique critical ATC codes covered by ANY source:", n_distinct(mfr$atc_code), "\n")
print(table(mfr$source))

## country -> region, as assigned in 01_combine_sources.R (mfr_region):
## China, India, Other Asia & Oceania, North America, Non-EU Europe,
## Rest of world (Latin America + Africa), EU/EEA (reference)
if (BASE == "Ireland") {
  n_mixed <- sum(mfr$source == "ireland" & str_detect(coalesce(mfr$mfr_country, ""), "\\|"))
  cat("Ireland rows with a mixed 'A | B' country list (no single country, left out):", n_mixed, "\n")
  mfr <- filter(mfr, !(source == "ireland" & str_detect(coalesce(mfr_country, ""), "\\|")))
}

region_of_country <- mfr |>
  filter(!is.na(mfr_country), !is.na(mfr_region)) |>
  distinct(mfr_country, mfr_region) |>
  { \(d) setNames(d$mfr_region, d$mfr_country) }()

## -----------------------------------------------------------------
## 1b. EudraGMDP API sites (for V3) -- taken from the combined file
##    (source == "gmdp"). 01_combine_sources.R has already:
##    - collapsed the long GMDP file to one row per ATC code x site
##    - kept only sites that MAKE or PROCESS the substance (EU GMP
##      API-certificate sections 3.1-3.4 or 3.5.1; packaging-only
##      3.5.2-3.5.4 and testing-only 3.6.x sites dropped). Set
##      GMDP_MANUFACTURING_ONLY <- FALSE there to keep every site.
##    - dropped withdrawn certificates and non-critical codes
##    - tagged every site manufacturer_step = 1 (active substance)
##    - harmonised "Czechia" / "Korea, Republic of" to the Germany/CEP
##      spellings, so the China/India share and HHI line up
## -----------------------------------------------------------------
gmp_api <- mfr |>
  filter(source == "gmdp") |>
  distinct(atc_code, mfr_company, mfr_country, manufacturer_step, source)

cat("\nEudraGMDP API sites in the combined file (ATC x site rows) for V3:", nrow(gmp_api), "\n")
cat("Critical ATC codes with >=1 GMP API site:", n_distinct(gmp_api$atc_code), "\n")

## -----------------------------------------------------------------
## 2. Load the BfArM shortage register
##    - semicolon-delimited, Latin-1 encoded
##    - "Atc Code" can hold multiple comma-separated codes
##    - a few rows carry a level-4-only code (5 chars, e.g. "A02AH")
##      or a placeholder "TERM_ID_NA_..." with no ATC at all -- both
##      are dropped from the exact-match shortage flag below, since
##      they can't be pinned to one critical ATC5 code.
## -----------------------------------------------------------------
shortages_raw <- read_delim(
  path_shortages,
  delim = ";",
  locale = locale(encoding = "ISO-8859-1", decimal_mark = ","),
  show_col_types = FALSE
) |>
  clean_names_simple()

cat("\nShortage report rows:", nrow(shortages_raw), "\n")

## Parse dates (dd.mm.yyyy; "N/A" -> NA = open-ended shortage)
parse_de_date <- function(x) dmy(na_if(x, "N/A"))

shortages <- shortages_raw |>
  mutate(
    beginn = parse_de_date(beginn),
    ende   = parse_de_date(ende)
  )

## One row per individual ATC code (split multi-code cells)
shortage_long <- shortages |>
  filter(!is.na(atc_code), atc_code != "") |>
  separate_rows(atc_code, sep = ",\\s*") |>
  mutate(atc_code = str_trim(atc_code))

## Keep only proper 7-character ATC5 codes for the exact-match flag
n_level4  <- sum(nchar(shortage_long$atc_code) == 5, na.rm = TRUE)
n_no_atc  <- sum(str_detect(shortage_long$atc_code, "^TERM_ID_NA"), na.rm = TRUE)
cat("Dropping", n_level4, "level-4-only rows and", n_no_atc,
    "rows with no assigned ATC code from the exact-match flag.\n")

shortage_long <- shortage_long |>
  filter(nchar(atc_code) == 7)

## "Currently active" = start on/before today, and either open-ended
## or end on/after today. Change reference_date if you want the flag
## as of a different point in time (e.g. the file's own pull date).
reference_date <- as_date("2026-09-09")

shortage_status <- shortage_long |>
  group_by(atc_code) |>
  summarise(
    ever_reported   = TRUE,
    currently_active = any(beginn <= reference_date &
                             (is.na(ende) | ende >= reference_date)),
    n_reports       = n(),
    .groups = "drop"
  )

cat("Unique ATC5 codes with >=1 shortage report:", nrow(shortage_status), "\n")

## -----------------------------------------------------------------
## 2b. EMA shortage list (GermanyEMA / GermanyEMAall outcomes only)
##    - Medicines_output_shortages_en.json (EMA download): one record per
##      shortage, with names but NO ATC code
##    - ema_shortages_atc_crosswalk.csv: record (ema_slug = last part of
##      shortage_url) -> ATC code(s), one row per code. Curated by hand
##      because name matching alone gets several wrong (Zirabev ->
##      ophthalmic bevacizumab, Bronchitol -> IV mannitol, Cyanokit ->
##      vitamin B12) and misses brand-only records (Moventig, Trulicity).
##      A record can map to several codes (e.g. Insuman Rapid/Basal/Comb).
##    - Active on the reference date (same date as the German flag) =
##      first published on/before it AND
##        status "Shortage ongoing", or
##        status "Shortage resolved" with last update on/after it
##        (resolved after the reference date, so still open on it), or
##        status "Medicine discontinued" only if EMA_COUNT_DISCONTINUED
##      The EMA list gives no end date, so the last update of a resolved
##      record is the closest available date of resolution.
##    - GermanyEMAall: every record counts (status and dates ignored)
## -----------------------------------------------------------------
if (uses_ema) {
  ema_raw <- jsonlite::fromJSON(path_ema)$data |>
    as_tibble() |>
    filter(category == "Human") |>
    mutate(
      ema_slug        = basename(shortage_url),
      status          = supply_shortage_status,
      first_published = dmy(na_if(first_published_date, "")),
      last_updated    = dmy(na_if(last_updated_date, "")),
      ema_active      = first_published <= reference_date & (
        status == "Shortage ongoing" |
          (status == "Shortage resolved" & coalesce(last_updated >= reference_date, FALSE)) |
          (status == "Medicine discontinued" & EMA_COUNT_DISCONTINUED)),
      ema_active      = if (OUTCOME == "GermanyEMAall") TRUE else ema_active
    )
  cat("\nEMA shortage records:", nrow(ema_raw), "\n")
  print(table(ema_raw$status))
  
  ema_atc <- read_csv(path_ema_atc, show_col_types = FALSE, col_types = cols(.default = "c"))
  
  ## every EMA record must be in the crosswalk (a new EMA download can add
  ## records: map them in the crosswalk, or give them an empty atc_code)
  missing_xw <- setdiff(ema_raw$ema_slug, ema_atc$ema_slug)
  if (length(missing_xw))
    stop("EMA records missing from ", path_ema_atc, ":\n  ",
         paste(missing_xw, collapse = "\n  "),
         "\nAdd a row for each (atc_code left empty if there is no ATC code).", call. = FALSE)
  
  ema_matches <- ema_raw |>
    select(ema_slug, medicine_affected, status, first_published, last_updated, ema_active) |>
    left_join(select(ema_atc, ema_slug, atc_code, match, note), by = "ema_slug",
              relationship = "one-to-many") |>
    mutate(in_critical_list = atc_code %in% mfr$atc_code)
  
  ema_status <- ema_matches |>
    filter(!is.na(atc_code)) |>
    group_by(atc_code) |>
    summarise(
      ema_ever_listed    = TRUE,
      ema_active         = any(ema_active),
      n_ema_records      = n_distinct(ema_slug),
      ema_medicines      = paste(unique(medicine_affected), collapse = "; "),
      .groups = "drop"
    )
  
  write_csv(mutate(ema_matches, list = LIST, .before = 1), out_file("ema_shortage_atc_matches.csv"))
  cat(sprintf(paste0("EMA records counted as shortage (", OUTCOME, ", reference date %s): %d of %d; they map to %d ATC codes,",
                     " %d of them in this list's critical codes\n"),
              reference_date, sum(ema_raw$ema_active), nrow(ema_raw),
              sum(ema_status$ema_active),
              sum(ema_status$ema_active & ema_status$atc_code %in% mfr$atc_code)))
  cat("Unmapped EMA records (no ATC code):",
      paste(unique(ema_matches$medicine_affected[is.na(ema_matches$atc_code)]), collapse = "; "), "\n")
}

## -----------------------------------------------------------------
## 3. Manufacturer-concentration metrics per ATC code
##    Computed at three levels:
##      - EPAR-only  (source == "ema")            -> used only for the
##                                                    "25 single-source"
##                                                    reproduction check
##                                                    in section 4
##      - V1: Germany-only (source == "germany")  -> main analysis input
##      - V2: Germany + EPAR combined/deduplicated -> main analysis input
##      - V3: Germany + EPAR + CEP + EudraGMDP API  -> main analysis input
##    Ireland is not used anywhere; CEP and GMDP are used only in V3.
## -----------------------------------------------------------------
## Step-1 (active-substance) priority filter, exactly as in the paper's
## Methods: for each ATC code, keep step-1 rows if any exist for that
## code, otherwise fall back to all disclosed rows (step-2 / NA-step).
## This matters for EPAR and Germany, which distinguish step 1/2;
## Ireland and CEP have no step field, so every row passes through
## unchanged for them.
apply_step1_priority <- function(df) {
  df |>
    group_by(atc_code) |>
    mutate(has_step1 = any(manufacturer_step == 1, na.rm = TRUE)) |>
    filter(!has_step1 | (has_step1 & manufacturer_step == 1)) |>
    ungroup() |>
    select(-has_step1)
}

## Company-name harmonisation, used for V3 only. The V3 registers
## spell the same firm differently, e.g.
##   Germany: "Zhejiang Supor Pharmaceuticals Chemical Co., Ltd. (BS 1)"
##   CEP:     "ZHEJIANG SUPOR PHARMACEUTICAL CO., LTD. Shangyu CN"
##   GMP:     "Zhejiang Supor Pharmaceuticals Co. Ltd."
## so an exact-string n_distinct() would count one firm up to three
## times. The key is: drop "(BS n)" site suffixes and the CEP trailing
## 2-letter country code, transliterate to ASCII, lowercase, strip
## punctuation and legal-form words, then keep the first two remaining
## words ("zhejiang supor"), and pair it with the country. Deliberately
## coarse: it can merge two distinct same-country firms that share their
## first two words (e.g. "Chemische Fabrik Lehrte" / "Chemische Fabrik
## Berg"), so treat V3 company counts as a slight lower bound.
legal_forms <- c("ltd", "limited", "pvt", "private", "plc", "llc", "inc", "corp",
                 "corporation", "co", "company", "gmbh", "mbh", "ag", "kg", "ohg",
                 "se", "sa", "sas", "sarl", "spa", "srl", "sl", "slu", "bv", "nv",
                 "as", "aps", "ab", "oy", "oyj", "kft", "zrt", "sro", "doo", "dd",
                 "sp", "zoo", "the", "s", "a", "p", "r", "l", "o", "z", "b", "v")

normalize_company <- function(x) {
  key <- x |>
    str_remove_all("\\(BS\\s*\\d+\\)") |>
    str_remove("\\s+[A-Z]{2}$") |>
    iconv(to = "ASCII//TRANSLIT", sub = "") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", " ") |>
    str_squish() |>
    str_split(" ") |>
    vapply(\(w) paste(head(setdiff(w, legal_forms), 2), collapse = " "), "")
  if_else(key == "", str_to_lower(str_squish(x)), key)   # all-legal-form name: keep as is
}

compute_metrics <- function(df, china_india = c("China", "India"), step1_priority = FALSE,
                            harmonise_names = FALSE) {
  if (step1_priority) df <- apply_step1_priority(df)
  ## harmonised key = name key + country, so "Fresenius Kabi" in Austria
  ## and in Germany stay two manufacturers (as in V1/V2), while the same
  ## firm listed by Germany, CEP and GMP in one country collapses to one
  if (harmonise_names) df <- mutate(df, mfr_company = paste(normalize_company(mfr_company),
                                                            mfr_country, sep = " @ "))
  df |>
    distinct(atc_code, mfr_company, mfr_country) |>
    mutate(region = unname(region_of_country[mfr_country])) |>
    group_by(atc_code) |>
    summarise(
      n_manufacturers = n_distinct(mfr_company, na.rm = TRUE),
      n_countries     = n_distinct(mfr_country, na.rm = TRUE),
      n_china_india   = sum(mfr_country %in% china_india, na.rm = TRUE),
      pct_china_india = n_china_india / n(),
      ## share of manufacturers per region (same denominator as above);
      ## EU/EEA is the reference group in the regional model
      pct_china         = sum(region %in% "China")                / n(),
      pct_india         = sum(region %in% "India")                / n(),
      pct_other_asia    = sum(region %in% "Other Asia & Oceania") / n(),
      pct_north_america = sum(region %in% "North America")        / n(),
      pct_non_eu_europe = sum(region %in% "Non-EU Europe")        / n(),
      pct_rest_of_world = sum(region %in% "Rest of world")        / n(),
      pct_eu_eea        = sum(region %in% "EU/EEA")               / n(),
      hhi             = {
        shares <- table(mfr_country[!is.na(mfr_country)]) / sum(!is.na(mfr_country))
        sum(shares^2) * 10000
      },
      .groups = "drop"
    ) |>
    mutate(
      single_source  = n_manufacturers == 1,   # exactly one manufacturer/company
      single_country = hhi >= 9999              # all disclosed sites in one country
      # (this is the paper's own "single
      # source" definition -- likely what
      # your "25" figure used, not
      # single_source above)
    )
}

metrics_epar <- compute_metrics(filter(mfr, source == "ema"), step1_priority = TRUE) |>
  rename_with(~paste0(.x, "_epar"), -atc_code)

## V1: Germany only
metrics_v1 <- compute_metrics(filter(mfr, source == base_source), step1_priority = TRUE) |>
  rename_with(~paste0(.x, "_v1"), -atc_code)

## V2: Germany + EPAR combined (step-1 priority applied within this
## Germany+EPAR subset, so a code with a step-1 EPAR or Germany
## manufacturer uses only step-1 rows; falls back to step-2/NA
## otherwise)
metrics_v2 <- compute_metrics(filter(mfr, source %in% c(base_source, "ema")), step1_priority = TRUE) |>
  rename_with(~paste0(.x, "_v2"), -atc_code)

## V3: all sources -- Germany + EPAR + CEP + EudraGMDP API sites.
## CEP and GMP API rows are active-substance manufacturers by
## definition, but CEP has no step field (NA). Left as NA, the step-1
## filter would DROP every CEP row for any code where Germany or EPAR
## lists a step-1 site -- so CEP is tagged step 1 here (GMP already is).
## Net effect: per code, V3 = Germany and EPAR step-1 sites + all CEP
## holders + all GMP API sites (or Germany/EPAR step-2/NA rows + CEP +
## GMP if no source has a step-1 site for that code). Company names are
## harmonised across the four registers (see normalize_company()).
v3_input <- bind_rows(
  mfr |>
    filter(source %in% c(base_source, "ema", "cep")) |>
    mutate(manufacturer_step = if_else(source == "cep", 1, manufacturer_step)),
  gmp_api                                   # = combined rows with source == "gmdp"
)

metrics_v3 <- compute_metrics(v3_input, step1_priority = TRUE, harmonise_names = TRUE) |>
  rename_with(~paste0(.x, "_v3"), -atc_code)

## How much the name harmonisation matters (raw strings vs. harmonised key)
v3_dedup_check <- compute_metrics(v3_input, step1_priority = TRUE, harmonise_names = FALSE) |>
  select(atc_code, n_raw = n_manufacturers) |>
  inner_join(select(metrics_v3, atc_code, n_harm = n_manufacturers_v3), by = "atc_code")
cat(sprintf(paste0("\nV3 name harmonisation: median manufacturers per code %.0f (raw strings)",
                   " -> %.0f (harmonised); %d of %d codes change.\n"),
            median(v3_dedup_check$n_raw), median(v3_dedup_check$n_harm),
            sum(v3_dedup_check$n_raw != v3_dedup_check$n_harm), nrow(v3_dedup_check)))
cat("V3 covers", nrow(metrics_v3), "critical ATC codes (V1:", nrow(metrics_v1),
    "| V2:", nrow(metrics_v2), ")\n")

## Full ATC universe = every critical code with at least one manufacturer
## record anywhere in the combined register (GMDP included), so codes with
## zero disclosed EPAR/DE manufacturers still show up (with NA metrics for
## that source) rather than being silently dropped.
all_codes <- tibble(atc_code = unique(mfr$atc_code))

analysis <- all_codes |>
  left_join(metrics_epar,     by = "atc_code") |>
  left_join(metrics_v1,       by = "atc_code") |>
  left_join(metrics_v2,       by = "atc_code") |>
  left_join(metrics_v3,       by = "atc_code") |>
  left_join(shortage_status,  by = "atc_code") |>
  mutate(
    ever_reported    = replace_na(ever_reported, FALSE),
    currently_active = replace_na(currently_active, FALSE),
    n_reports        = replace_na(n_reports, 0L)
  )

## GermanyEMA outcome: currently_active = German OR EMA shortage, so every
## test and model below runs on the combined outcome unchanged. The German
## and EMA flags are kept separately (currently_active_de, ema_active).
if (uses_ema) {
  analysis <- analysis |>
    left_join(ema_status, by = "atc_code") |>
    mutate(
      ema_ever_listed     = replace_na(ema_ever_listed, FALSE),
      ema_active          = replace_na(ema_active, FALSE),
      n_ema_records       = replace_na(n_ema_records, 0L),
      currently_active_de = currently_active,
      ever_reported_de    = ever_reported,
      currently_active    = currently_active_de | ema_active,
      ever_reported       = ever_reported_de | ema_ever_listed
    )
  cat("\n--- Shortage outcome: German register vs. EMA list (critical codes, currently active) ---\n")
  print(with(analysis, table(german = currently_active_de, ema = ema_active)))
  cat("Codes added by EMA (not in German shortage):",
      paste(analysis$atc_code[analysis$ema_active & !analysis$currently_active_de], collapse = ", "), "\n")
}

cat("\nAnalysis table:", nrow(analysis), "critical ATC codes;",
    sum(analysis$currently_active), "currently in shortage (", outcome_label, ").\n")

## -----------------------------------------------------------------
## 4. Reproduce your "1 of 25" check
##    single_country (HHI = 10,000, i.e. all disclosed EPAR sites in
##    one country) is almost certainly the definition behind the
##    paper's "25 single-source" claim -- single_source (exactly one
##    manufacturer company) is a stricter, different thing and only
##    matches 12 EPAR codes, not ~25. Both are reported below so you
##    can see which one matches what you counted by hand.
## -----------------------------------------------------------------
cat("\n--- Single-COUNTRY (EPAR/EMA basis, HHI=10000) vs. currently-active shortage ---\n")
tab_single_country <- analysis |>
  filter(!is.na(single_country_epar)) |>
  count(single_country_epar, currently_active) |>
  pivot_wider(names_from = currently_active, values_from = n, values_fill = 0)
print(tab_single_country)

ft_country <- analysis |>
  filter(!is.na(single_country_epar)) |>
  { \(d) fisher.test(table(d$single_country_epar, d$currently_active)) }()
print(ft_country)

cat("\n--- Single-source COMPANY (EPAR/EMA basis, exactly 1 manufacturer) vs. currently-active shortage ---\n")
tab_single_source <- analysis |>
  filter(!is.na(single_source_epar)) |>
  count(single_source_epar, currently_active) |>
  pivot_wider(names_from = currently_active, values_from = n, values_fill = 0)
print(tab_single_source)

ft <- analysis |>
  filter(!is.na(single_source_epar)) |>
  { \(d) fisher.test(table(d$single_source_epar, d$currently_active)) }()
print(ft)

## Same check using V1 (Germany-only) manufacturer count, since the
## shortage register itself is German and Germany's register has far
## denser coverage (251 of 299 critical codes) than EPAR (51 codes).
cat(lbl("\n--- Single-source (V1: Germany only) vs. currently-active shortage ---\n"))
tab_single_source_v1 <- analysis |>
  filter(!is.na(single_source_v1)) |>
  count(single_source_v1, currently_active) |>
  pivot_wider(names_from = currently_active, values_from = n, values_fill = 0)
print(tab_single_source_v1)

ft_v1 <- analysis |>
  filter(!is.na(single_source_v1)) |>
  { \(d) fisher.test(table(d$single_source_v1, d$currently_active)) }()
print(ft_v1)

## Same check on V3 (Germany + EPAR + CEP + GMP API sites)
cat(lbl("\n--- Single-source (V3: Germany + EPAR + CEP + GMP API) vs. currently-active shortage ---\n"))
tab_single_source_v3 <- analysis |>
  filter(!is.na(single_source_v3)) |>
  count(single_source_v3, currently_active) |>
  pivot_wider(names_from = currently_active, values_from = n, values_fill = 0)
print(tab_single_source_v3)

ft_v3 <- analysis |>
  filter(!is.na(single_source_v3)) |>
  { \(d) fisher.test(table(d$single_source_v3, d$currently_active)) }()
print(ft_v3)

## -----------------------------------------------------------------
## 5. Univariate tests: does each metric differ between shortage and
##    non-shortage codes? Run on both V1 (Germany only) and V2
##    (Germany + EPAR combined).
## -----------------------------------------------------------------
univariate_test <- function(var, label) {
  d <- analysis |> filter(!is.na(.data[[var]]))
  wt <- wilcox.test(d[[var]] ~ d$currently_active)
  ct <- cor.test(d[[var]], as.numeric(d$currently_active))
  tibble(
    metric      = label,
    n           = nrow(d),
    median_no   = median(d[[var]][!d$currently_active], na.rm = TRUE),
    median_yes  = median(d[[var]][d$currently_active], na.rm = TRUE),
    wilcox_p    = wt$p.value,
    point_biserial_r = ct$estimate,
    cor_p       = ct$p.value
  )
}

univariate_results <- bind_rows(
  univariate_test("n_manufacturers_v1", lbl("N manufacturers (V1: Germany)")),
  univariate_test("n_countries_v1",     lbl("N countries (V1: Germany)")),
  univariate_test("pct_china_india_v1", lbl("% China+India (V1: Germany)")),
  univariate_test("hhi_v1",             lbl("Country HHI (V1: Germany)")),
  univariate_test("n_manufacturers_v2", lbl("N manufacturers (V2: Germany+EPAR)")),
  univariate_test("n_countries_v2",     lbl("N countries (V2: Germany+EPAR)")),
  univariate_test("pct_china_india_v2", lbl("% China+India (V2: Germany+EPAR)")),
  univariate_test("hhi_v2",             lbl("Country HHI (V2: Germany+EPAR)")),
  univariate_test("n_manufacturers_v3", lbl("N manufacturers (V3: Germany+EPAR+CEP+GMP)")),
  univariate_test("n_countries_v3",     lbl("N countries (V3: Germany+EPAR+CEP+GMP)")),
  univariate_test("pct_china_india_v3", lbl("% China+India (V3: Germany+EPAR+CEP+GMP)")),
  univariate_test("hhi_v3",             lbl("Country HHI (V3: Germany+EPAR+CEP+GMP)"))
)

cat("\n--- Univariate association with currently-active shortage ---\n")
print(univariate_results, n = Inf)

## -----------------------------------------------------------------
## 6. Multivariate logistic regression
##    shortage ~ manufacturer count + concentration + China/India share
##    Log-transform manufacturer count (right-skewed); scale HHI to
##    0-1 for a readable coefficient. Fit separately on V1 and V2.
## -----------------------------------------------------------------
## predictors that are constant in the data cannot be estimated (e.g. the
## Irish register lists no manufacturer in China or India, so the China +
## India share is 0 for every code): they are left out of that model, with
## a message, and listed as "not estimable" in the saved table
usable_terms <- function(d, terms, label) {
  ok <- vapply(terms, \(t) length(unique(na.omit(d[[t]]))) > 1, logical(1))
  if (any(!ok))
    cat("\n", label, ": no variation in", paste(terms[!ok], collapse = ", "),
        "-> left out of the model (not estimable)\n")
  terms[ok]
}
main_terms <- c("log_n_manufacturers", "hhi_scaled", "pct_china_india")

model_data_v1 <- analysis |>
  filter(!is.na(n_manufacturers_v1)) |>
  mutate(
    log_n_manufacturers = log1p(n_manufacturers_v1),
    hhi_scaled          = hhi_v1 / 10000,
    pct_china_india      = pct_china_india_v1
  )

formula_v1 <- reformulate(usable_terms(model_data_v1, main_terms, lbl("V1: Germany only")),
                          response = "currently_active")
fit_v1 <- glm(
  formula_v1,
  data   = model_data_v1,
  family = binomial()
)

cat(lbl("\n--- Logistic regression: shortage ~ manufacturer concentration (V1: Germany only) ---\n"))
print(summary(fit_v1))

cat("\nOdds ratios:\n")
print(tidy(fit_v1, exponentiate = TRUE, conf.int = TRUE))

model_data_v2 <- analysis |>
  filter(!is.na(n_manufacturers_v2)) |>
  mutate(
    log_n_manufacturers = log1p(n_manufacturers_v2),
    hhi_scaled          = hhi_v2 / 10000,
    pct_china_india      = pct_china_india_v2
  )

formula_v2 <- reformulate(usable_terms(model_data_v2, main_terms, lbl("V2: Germany + EPAR")),
                          response = "currently_active")
fit_v2 <- glm(
  formula_v2,
  data   = model_data_v2,
  family = binomial()
)

cat(lbl("\n--- Logistic regression: shortage ~ manufacturer concentration (V2: Germany + EPAR) ---\n"))
print(summary(fit_v2))
cat("\nOdds ratios:\n")
print(tidy(fit_v2, exponentiate = TRUE, conf.int = TRUE))

model_data_v3 <- analysis |>
  filter(!is.na(n_manufacturers_v3)) |>
  mutate(
    log_n_manufacturers = log1p(n_manufacturers_v3),
    hhi_scaled          = hhi_v3 / 10000,
    pct_china_india      = pct_china_india_v3
  )

formula_v3 <- reformulate(usable_terms(model_data_v3, main_terms, lbl("V3: Germany + EPAR + CEP + GMP API")),
                          response = "currently_active")
fit_v3 <- glm(
  formula_v3,
  data   = model_data_v3,
  family = binomial()
)

cat(lbl("\n--- Logistic regression: shortage ~ manufacturer concentration (V3: Germany + EPAR + CEP + GMP API) ---\n"))
print(summary(fit_v3))
cat("\nOdds ratios:\n")
print(tidy(fit_v3, exponentiate = TRUE, conf.int = TRUE))

## -----------------------------------------------------------------
## 6b. More detailed model diagnostics for each fit
##    - profile-likelihood CIs on the odds ratios (more reliable in
##      small samples than the Wald CIs tidy() gives above)
##    - a likelihood-ratio test per term (drop-in-deviance test --
##      generally preferred over the Wald z-test in summary() for
##      small/medium n)
##    - McFadden's pseudo-R^2 (no single "R^2" exists for logistic
##      models; this is the most commonly reported analogue)
##    - VIFs, to check the three predictors aren't confounding each
##      other (values > ~5 would be a concern; not expected here since
##      they capture different aspects of concentration)
##    - in-sample AUC and a confusion matrix at a 0.5 cutoff, i.e. how
##      well the fitted model actually discriminates shortage vs. not
## -----------------------------------------------------------------
model_diagnostics <- function(fit, outcome, label) {
  cat("\n===", label, "===\n")
  
  cat("\nProfile-likelihood CIs (odds ratios):\n")
  print(exp(cbind(OR = coef(fit), confint(fit))))
  
  cat("\nLikelihood-ratio test per term (Type II, drop-in-deviance):\n")
  print(car::Anova(fit, type = "II", test.statistic = "LR"))
  
  mcfadden_r2 <- 1 - (fit$deviance / fit$null.deviance)
  cat(sprintf("\nMcFadden's pseudo-R^2: %.3f\n", mcfadden_r2))
  cat(sprintf("AIC: %.1f   BIC: %.1f   log-likelihood: %.1f\n",
              AIC(fit), BIC(fit), as.numeric(logLik(fit))))
  
  cat("\nVariance inflation factors (multicollinearity check):\n")
  if (length(coef(fit)) > 2) print(tryCatch(car::vif(fit), error = function(e) conditionMessage(e))) else
    cat("(only one predictor -- not applicable)\n")
  
  pred_prob <- predict(fit, type = "response")
  roc_obj   <- pROC::roc(outcome, pred_prob, quiet = TRUE)
  cat(sprintf("\nIn-sample AUC: %.3f\n", as.numeric(pROC::auc(roc_obj))))
  
  cat("\nConfusion matrix at 0.5 cutoff:\n")
  pred_class <- factor(pred_prob > 0.5, levels = c(FALSE, TRUE))
  print(table(predicted = pred_class, actual = outcome))
  cat("(With shortages this rare (~", round(100 * mean(outcome), 1),
      "% of codes), the model may never cross 0.5 -- that's expected class\n",
      "imbalance, not a broken model. A cutoff at the base rate is more informative:\n", sep = "")
  
  base_rate <- mean(outcome)
  pred_class_br <- factor(pred_prob > base_rate, levels = c(FALSE, TRUE))
  cat(sprintf("Confusion matrix at %.3f (base-rate) cutoff:\n", base_rate))
  print(table(predicted = pred_class_br, actual = outcome))
  
  invisible(list(mcfadden_r2 = mcfadden_r2, auc = as.numeric(pROC::auc(roc_obj))))
}

model_diagnostics(fit_v1, model_data_v1$currently_active, lbl("V1: Germany only"))
model_diagnostics(fit_v2, model_data_v2$currently_active, lbl("V2: Germany + EPAR"))
model_diagnostics(fit_v3, model_data_v3$currently_active, lbl("V3: Germany + EPAR + CEP + GMP API"))

## -----------------------------------------------------------------
## 6c. Regional model (V1, V2, V3)
##    Same as section 6, but the China + India share is split by region:
##      China, India, other Asia & Oceania, North America, non-EU Europe,
##      rest of world (Latin America + Africa)
##    EU/EEA is the reference group, so each odds ratio compares a code
##    whose manufacturers are all in that region with one whose
##    manufacturers are all in the EU/EEA. Regions with no manufacturer
##    for any code in the data are left out of that model (message printed).
##    With 8 predictors and few shortages, check events_per_variable in
##    the saved table: below ~10 the estimates are unstable.
## -----------------------------------------------------------------
version_labels <- c(v1 = lbl("V1: Germany only"), v2 = lbl("V2: Germany + EPAR"),
                    v3 = lbl("V3: Germany + EPAR + CEP + GMP API"))
base_terms     <- c("log_n_manufacturers", "hhi_scaled")
regional_terms <- c("pct_china", "pct_india", "pct_other_asia", "pct_north_america",
                    "pct_non_eu_europe", "pct_rest_of_world")

make_model_data <- function(v) {
  analysis |>
    filter(!is.na(.data[[paste0("n_manufacturers_", v)]])) |>
    select(atc_code, currently_active, ends_with(paste0("_", v))) |>
    rename_with(~ sub(paste0("_", v, "$"), "", .x)) |>
    mutate(log_n_manufacturers = log1p(n_manufacturers),
           hhi_scaled          = hhi / 10000)
}

fit_regional <- function(v) {
  d     <- make_model_data(v)
  terms <- usable_terms(d, c(base_terms, regional_terms),
                        paste("Regional model,", version_labels[[v]]))
  glm(reformulate(terms, response = "currently_active"), data = d, family = binomial())
}

fits_regional <- lapply(c(v1 = "v1", v2 = "v2", v3 = "v3"), fit_regional)

for (v in names(fits_regional)) {
  cat("\n--- Regional logistic regression:", version_labels[[v]], "---\n")
  print(summary(fits_regional[[v]]))
  model_diagnostics(fits_regional[[v]], fits_regional[[v]]$data$currently_active,
                    paste("Regional model,", version_labels[[v]]))
}

## -----------------------------------------------------------------
## 6d. Save all model results (main + regional, V1-V3) to one CSV:
##     Results/<LIST>/shortage_models_V1_V2_V3.csv
##     odds ratio, profile-likelihood 95% CI (Wald CI if profiling fails),
##     likelihood-ratio p per term, N codes, N shortages, events per
##     variable, AUC, McFadden R^2, AIC
## -----------------------------------------------------------------
model_table <- function(fit, model, version, intended) {
  terms <- setdiff(names(coef(fit)), "(Intercept)")
  ci <- tryCatch(suppressMessages(confint(fit)),
                 error = function(e) { message("profile CI failed (", version, ", ", model,
                                               "), using Wald CI"); confint.default(fit) })
  lrt <- drop1(fit, test = "LRT")
  y   <- fit$y
  tibble(
    list                = LIST,
    base_register       = BASE,
    outcome             = OUTCOME,
    model               = model,
    version             = version,
    version_label       = version_labels[[version]],
    term                = terms,
    odds_ratio          = exp(coef(fit)[terms]),
    ci_low              = exp(ci[terms, 1]),
    ci_high             = exp(ci[terms, 2]),
    p_lrt               = lrt[terms, "Pr(>Chi)"],
    n_codes             = length(y),
    n_shortage          = sum(y),
    events_per_variable = sum(y) / length(terms),
    auc                 = as.numeric(pROC::auc(pROC::roc(y, fitted(fit), quiet = TRUE))),
    mcfadden_r2         = 1 - fit$deviance / fit$null.deviance,
    aic                 = AIC(fit)
  ) |>
    mutate(note = case_when(
      ## (quasi-)separation: e.g. no code with a manufacturer in that region
      ## is in shortage -> odds ratio runs off towards 0 or infinity
      is.na(ci_low) | is.na(ci_high) | odds_ratio < 1e-3 | odds_ratio > 1e3 ~
        "unstable: (quasi-)separation, do not interpret",
      events_per_variable < 10 ~ "few events per variable (<10): interpret with care",
      TRUE ~ ""
    )) |>
    ## terms left out of this model (no variation in the data)
    bind_rows(tibble(list = LIST, base_register = BASE, outcome = OUTCOME, model = model, version = version,
                     version_label = version_labels[[version]],
                     term = setdiff(intended, terms),
                     note = "not estimable: no variation in the data (left out of the model)")) |>
    mutate(term = factor(term, levels = intended)) |>
    arrange(term) |>
    mutate(term = as.character(term)) |>
    tidyr::fill(n_codes, n_shortage, events_per_variable, auc, mcfadden_r2, aic, .direction = "downup")
}

## -----------------------------------------------------------------
## 6e. Main and regional model + ATC chapter dummies (first letter of
##     the ATC code: J = anti-infectives, L = antineoplastic and
##     immunomodulating agents, ...). Every chapter with at least
##     ATC_MIN_CODES codes in the model data gets its own dummy (atc_J,
##     atc_L, ...); the smaller chapters are pooled as the reference
##     group, since a chapter with a handful of codes and 0-2 shortages
##     gives an unstable estimate. A joint likelihood-ratio test of all
##     ATC dummies against the same model without them is saved as term
##     "atc_joint" (p_lrt; dAIC in the note).
## -----------------------------------------------------------------
if (!exists("ATC_MIN_CODES", inherits = FALSE)) ATC_MIN_CODES <- 15

add_atc_dummies <- function(d) {
  chapter <- substr(d$atc_code, 1, 1)
  big     <- sort(names(which(table(chapter) >= ATC_MIN_CODES)))
  for (ch in big) d[[paste0("atc_", ch)]] <- as.integer(chapter == ch)
  attr(d, "atc_terms")     <- paste0("atc_", big)
  attr(d, "atc_reference") <- paste(sort(setdiff(unique(chapter), big)), collapse = ", ")
  d
}

fit_with_atc <- function(v, model) {
  d     <- add_atc_dummies(make_model_data(v))
  base  <- if (model == "main") main_terms else c(base_terms, regional_terms)
  label <- paste(if (model == "main") "Main" else "Regional", "model + ATC,", version_labels[[v]])
  base_used <- usable_terms(d, base, label)
  atc_used  <- usable_terms(d, attr(d, "atc_terms"), label)
  f0 <- glm(reformulate(base_used, response = "currently_active"), data = d, family = binomial())
  f1 <- glm(reformulate(c(base_used, atc_used), response = "currently_active"), data = d, family = binomial())
  joint_p <- anova(f0, f1, test = "LRT")$`Pr(>Chi)`[2]
  list(fit = f1, intended = c(base, attr(d, "atc_terms")), joint_p = joint_p,
       d_aic = AIC(f1) - AIC(f0), n_dummies = length(atc_used), reference = attr(d, "atc_reference"))
}

atc_table <- function(v, model) {
  r <- fit_with_atc(v, model)
  tab <- model_table(r$fit, paste0(model, "_atc"), v, r$intended)
  bind_rows(tab, tibble(
    list = LIST, base_register = BASE, outcome = OUTCOME, model = paste0(model, "_atc"),
    version = v, version_label = version_labels[[v]], term = "atc_joint",
    p_lrt = r$joint_p, n_codes = tab$n_codes[1], n_shortage = tab$n_shortage[1],
    events_per_variable = tab$events_per_variable[1], auc = tab$auc[1],
    mcfadden_r2 = tab$mcfadden_r2[1], aic = tab$aic[1],
    note = sprintf("joint LR test of %d ATC dummies vs. same model without; dAIC %+.1f; reference (pooled): %s",
                   r$n_dummies, r$d_aic, r$reference)))
}

atc_results <- bind_rows(lapply(c("v1", "v2", "v3"), function(v)
  bind_rows(atc_table(v, "main"), atc_table(v, "regional"))))

cat("\n--- ATC chapter dummies: joint test (does therapeutic area add information?) ---\n")
print(atc_results |> filter(term == "atc_joint") |> select(model, version, p_lrt, auc, note), width = 200)

model_results <- bind_rows(
  model_table(fit_v1, "main", "v1", main_terms),
  model_table(fit_v2, "main", "v2", main_terms),
  model_table(fit_v3, "main", "v3", main_terms),
  model_table(fits_regional$v1, "regional", "v1", c(base_terms, regional_terms)),
  model_table(fits_regional$v2, "regional", "v2", c(base_terms, regional_terms)),
  model_table(fits_regional$v3, "regional", "v3", c(base_terms, regional_terms)),
  atc_results
)

write_csv(model_results, out_file("shortage_models_V1_V2_V3.csv"))
cat("\nModel results saved to", out_file("shortage_models_V1_V2_V3.csv"), "\n")

## how much data is behind each regional term: codes with >= 1 manufacturer
## in the region, and how many of those are currently in shortage
region_counts <- bind_rows(lapply(names(version_labels), function(v) {
  d <- make_model_data(v)
  tibble(
    list       = LIST,
    base_register = BASE,
    outcome    = OUTCOME,
    version    = v,
    region     = c("China", "India", "Other Asia & Oceania", "North America",
                   "Non-EU Europe", "Rest of world", "EU/EEA"),
    term       = c(regional_terms, "pct_eu_eea"),
    n_codes_with_region = vapply(term, \(t) sum(d[[t]] > 0, na.rm = TRUE), numeric(1)),
    n_in_shortage       = vapply(term, \(t) sum(d[[t]] > 0 & d$currently_active, na.rm = TRUE), numeric(1)),
    n_codes_in_model    = nrow(d)
  )
}))
write_csv(region_counts, out_file("shortage_region_counts.csv"))
cat("\nCodes with >= 1 manufacturer per region (and how many are in shortage):\n")
print(region_counts |> select(version, region, n_codes_with_region, n_in_shortage), n = Inf)
print(model_results |>
        select(model, version, term, odds_ratio, ci_low, ci_high, p_lrt, events_per_variable) |>
        mutate(across(where(is.numeric), ~ signif(.x, 3))),
      n = Inf)

## -----------------------------------------------------------------
## 7. Plots (V1: Germany only, as the primary/default view)
## -----------------------------------------------------------------
theme_set(theme_minimal(base_size = 12))
shortage_axis <- c(Germany = "Currently on shortage list",
                   GermanyEMA = "Currently in shortage (German register or EMA list)",
                   GermanyEMAall = "In shortage (German register, or ever on EMA list)")[[OUTCOME]]

p1 <- analysis |>
  filter(!is.na(n_manufacturers_v1)) |>
  ggplot(aes(x = currently_active, y = n_manufacturers_v1)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_log10() +
  labs(x = shortage_axis, y = lbl("Distinct manufacturers (V1: Germany, log scale)"),
       title = "Manufacturer count vs. shortage status")

p2 <- analysis |>
  filter(!is.na(hhi_v1)) |>
  ggplot(aes(x = currently_active, y = hhi_v1)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  labs(x = shortage_axis, y = lbl("Country-level HHI (V1: Germany)"),
       title = "Supply concentration vs. shortage status")

p3 <- analysis |>
  filter(!is.na(pct_china_india_v1)) |>
  ggplot(aes(x = currently_active, y = pct_china_india_v1)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_continuous(labels = percent) +
  labs(x = shortage_axis, y = lbl("Share of manufacturers in China/India (V1: Germany)"),
       title = "China+India dependence vs. shortage status")

p4 <- model_data_v1 |>
  ggplot(aes(x = n_manufacturers_v1, y = as.numeric(currently_active))) +
  geom_jitter(height = 0.03, alpha = 0.3) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "firebrick") +
  scale_x_log10() +
  labs(x = lbl("Distinct manufacturers (V1: Germany, log scale)"), y = "P(currently on shortage list)",
       title = "Fitted shortage probability vs. manufacturer count")

p5 <- analysis |>
  filter(!is.na(n_manufacturers_v2)) |>
  ggplot(aes(x = currently_active, y = n_manufacturers_v2)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_log10() +
  labs(x = shortage_axis, y = lbl("Distinct manufacturers (V2: Germany+EPAR, log scale)"),
       title = "Manufacturer count vs. shortage status (V2)")

p6 <- analysis |>
  filter(!is.na(pct_china_india_v2)) |>
  ggplot(aes(x = currently_active, y = pct_china_india_v2)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_continuous(labels = percent) +
  labs(x = shortage_axis, y = lbl("Share of manufacturers in China/India (V2: Germany+EPAR)"),
       title = "China+India dependence vs. shortage status (V2)")

p7 <- analysis |>
  filter(!is.na(n_manufacturers_v3)) |>
  ggplot(aes(x = currently_active, y = n_manufacturers_v3)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_log10() +
  labs(x = shortage_axis, y = sub("DE", c(Germany = "DE", Ireland = "IE")[[BASE]], "Distinct manufacturers (V3: DE+EPAR+CEP+GMP, log scale)", fixed = TRUE),
       title = "Manufacturer count vs. shortage status (V3)")

p8 <- analysis |>
  filter(!is.na(pct_china_india_v3)) |>
  ggplot(aes(x = currently_active, y = pct_china_india_v3)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_continuous(labels = percent) +
  labs(x = shortage_axis, y = sub("DE", c(Germany = "DE", Ireland = "IE")[[BASE]], "Share of manufacturers in China/India (V3: DE+EPAR+CEP+GMP)", fixed = TRUE),
       title = "China+India dependence vs. shortage status (V3)")

p9 <- analysis |>
  filter(!is.na(hhi_v3)) |>
  ggplot(aes(x = currently_active, y = hhi_v3)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  labs(x = shortage_axis, y = sub("DE", c(Germany = "DE", Ireland = "IE")[[BASE]], "Country-level HHI (V3: DE+EPAR+CEP+GMP)", fixed = TRUE),
       title = "Supply concentration vs. shortage status (V3)")

ggsave(out_file("plot_v1_manufacturers_vs_shortage.png"), p1, width = 6, height = 4.5, dpi = 150)
ggsave(out_file("plot_v1_hhi_vs_shortage.png"),           p2, width = 6, height = 4.5, dpi = 150)
ggsave(out_file("plot_v1_china_india_vs_shortage.png"),   p3, width = 6, height = 4.5, dpi = 150)
ggsave(out_file("plot_v1_shortage_probability_curve.png"),p4, width = 6, height = 4.5, dpi = 150)
ggsave(out_file("plot_v2_manufacturers_vs_shortage.png"), p5, width = 6, height = 4.5, dpi = 150)
ggsave(out_file("plot_v2_china_india_vs_shortage.png"),   p6, width = 6, height = 4.5, dpi = 150)
ggsave(out_file("plot_v3_manufacturers_vs_shortage.png"), p7, width = 6, height = 4.5, dpi = 150)
ggsave(out_file("plot_v3_china_india_vs_shortage.png"),   p8, width = 6, height = 4.5, dpi = 150)
ggsave(out_file("plot_v3_hhi_vs_shortage.png"),           p9, width = 6, height = 4.5, dpi = 150)

## -----------------------------------------------------------------
## 8. Save the merged analysis table for further digging
## -----------------------------------------------------------------
write_csv(mutate(analysis, list = LIST, base_register = BASE, outcome = OUTCOME, .before = 1), out_file("atc_shortage_concentration_analysis.csv"))

cat("\nDone. Wrote", basename(out_file("atc_shortage_concentration_analysis.csv")), "and 9 diagnostic plots to", OUT_DIR, "\n")