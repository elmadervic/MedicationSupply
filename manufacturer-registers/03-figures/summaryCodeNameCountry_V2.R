library(ggplot2)
## -----------------------------------------------------------------
## Paths. Run this script from the repository root.
##   DATA_DIR - the four source registers + critical.csv (read-only)
##   OUT_DIR  - everything this script writes (tables and figures)
## -----------------------------------------------------------------
DATA_DIR <- "data/manufacturer-registers/raw"
OUT_DIR  <- "data/manufacturer-registers/out"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

library(dplyr)
library(tidyr)
library(patchwork)
library(stringr)
library(readr)
library(purrr)

# ---------------------------------------------------------------
# FIXES APPLIED vs. the previous version of this script (found by
# comparing its totals against plots_by_source.R, which reads a
# separately-built manufacturer_registers_combined.csv):
#
#  1. Germany/BfArM's 'land' column (German country names, e.g.
#     "Vereinigtes Königreich" / "Vereinigtes Königreich (Nordirland)")
#     was never translated to English or normalized. Both German UK
#     variants above counted as 2 distinct countries instead of
#     merging into "United Kingdom" -- this alone accounted for
#     Germany showing 50 distinct countries here vs. 49 in
#     plots_by_source.R. Fixed with country_map, applied right after
#     reading the file.
#  2. EPAR's 'country' column was used completely unnormalized (no
#     country_map applied at all), while Germany/Ireland/CEP did get
#     some normalization further down. EPAR's raw data has 4
#     duplicate-spelling pairs -- UK/United Kingdom, USA/United States,
#     The Netherlands/Netherlands, Republic of Korea/South Korea --
#     that were being counted as 8 distinct countries instead of 4.
#     This exactly matched the 26-vs-30 EPAR country gap vs.
#     plots_by_source.R. Fixed by applying country_map to EPAR$country
#     right after reading the file, same as the other sources.
#  3. Ireland's mfr_company / mfr_country are "|"-separated parallel
#     lists (multiple manufacturers per product row, e.g.
#     "Salutas Pharma GmbH | Rowa Pharmaceuticals Ltd. | Novartis
#     Pharmaceuticals S.R.L." paired with "Germany | Ireland |
#     Romania"). The previous version only ran
#     separate_rows(mfr_country, ...) -- mfr_company was NEVER split,
#     so every exploded row still carried the full 3-company glued
#     string as a single "manufacturer" value, mismatched against its
#     one now-single country. Fixed by splitting BOTH columns together
#     (separate_rows(mfr_company, mfr_country, ...)) so each
#     manufacturer is correctly paired with its own country. Rows
#     where the two lists don't have matching lengths are handled by
#     tidyr's recycling with a warning rather than silently
#     mispairing -- inspect the warning if it appears and adjust the
#     handful of affected rows manually if needed.
#  4. Ireland's atc_code was re-derived from the raw combined text
#     field via a fragile regex (splitting on commas, extracting
#     ALL-CAPS 5+ character tokens, splitting again on semicolons).
#     This can pick up ATC-chapter-level parent codes (e.g. "J02AC",
#     5 characters) alongside true level-5 codes (e.g. "J02AC01") from
#     the same cell, and is generally harder to audit than just using
#     the file's own pre-cleaned single-code column. Simplified to use
#     matched_critical_atc directly, dropping the custom regex
#     extraction entirely.
# ---------------------------------------------------------------

# German -> English country names, plus normalization of variant
# spellings that otherwise appear in EPAR's own data (UK vs United
# Kingdom, USA vs United States, etc.) so every source's country field
# is comparable and duplicate-spelling entries don't inflate counts.
country_map <- c(
  "Argentinien" = "Argentina", "Australien" = "Australia", "Belgien" = "Belgium",
  "Brasilien" = "Brazil", "Bulgarien" = "Bulgaria", "Kroatien" = "Croatia",
  "Deutschland" = "Germany", "Dänemark" = "Denmark", "Finnland" = "Finland",
  "Frankreich" = "France", "Griechenland" = "Greece", "Indien" = "India",
  "Irland" = "Ireland", "Island" = "Iceland", "Italien" = "Italy", "Kanada" = "Canada",
  "Korea, Republik" = "South Korea", "Republic of Korea" = "South Korea",
  "Südkorea" = "South Korea", "Lettland" = "Latvia", "Litauen" = "Lithuania",
  "Mexiko" = "Mexico", "Niederlande" = "Netherlands", "The Netherlands" = "Netherlands",
  "Norwegen" = "Norway", "Polen" = "Poland", "Rumänien" = "Romania",
  "Schweden" = "Sweden", "Schweiz" = "Switzerland", "Singapur" = "Singapore",
  "Slowakei" = "Slovakia", "Slowenien" = "Slovenia", "Südafrika" = "South Africa",
  "Tschechische Republik" = "Czech Republic", "Türkei" = "Turkey",
  "UK" = "United Kingdom", "USA" = "United States", "Ungarn" = "Hungary",
  "Vereinigte Staaten" = "United States", "Vereinigtes Königreich" = "United Kingdom",
  "Vereinigtes Königreich (Nordirland)" = "United Kingdom", "Zypern" = "Cyprus",
  "Österreich" = "Austria"
)

germany <- read.csv(file.path(DATA_DIR, "bfarm_api_origin_critical_rest_LONG.csv"))
str(germany)
germany <- germany %>%
  filter(role != "Zulassungsinhaber") %>%
  mutate(land = recode(str_trim(land), !!!country_map))   # FIX #1: translate/normalize

write.csv(germany, file.path(OUT_DIR, "germany_critical.csv"), row.names = F)

EPAR <- read.csv(file.path(DATA_DIR, "EMA_data_critical.csv")) %>%
  mutate(country = recode(str_trim(country), !!!country_map))   # FIX #2: normalize

ireland <- read.csv(file.path(DATA_DIR, "ireland_critical_atc_review.csv"))
str(ireland)

# FIX #4: use the file's own pre-cleaned matched_critical_atc column
# directly, instead of re-deriving atc_code from the raw combined text
# field with a regex that could also match chapter-level parent codes.
# One row in ireland_critical_atc_review.csv has an un-split combined
# value ("L01BA01, L04AX03") -- split on comma too, same as CEP's
# atc_code handling, so that row's codes are still counted rather than
# silently failing the critical.csv match as one glued string.
ireland <- ireland %>%
  filter(!is.na(matched_critical_atc), matched_critical_atc != "") %>%
  mutate(atc_split = str_split(matched_critical_atc, ",\\s*")) %>%
  unnest(atc_split) %>%
  mutate(atc_code = str_trim(atc_split)) %>%
  select(-atc_split)

## -----------------------------------------------------------------
## Determine the critical ATC code universe.
##
## FALLBACK (added): this script read Data/critical.csv unconditionally,
## but that file isn't part of this data set, so a fresh session stopped
## here. Handled the same way as ComebineAll4SourcesV2.R: use
## Data/critical.csv when it exists, and otherwise fall back to the
## derived union of EMA/Germany/Ireland's own ATC codes. Those three
## files are already pre-filtered to critical substances, so their union
## approximates the reference list; CEP is not pre-filtered, and is what
## the filter below actually constrains. The derived union is only an
## approximation -- when the real reference file is present, this script
## and ComebineAll4SourcesV2.R use exactly the same universe.
## -----------------------------------------------------------------
if (file.exists(file.path(DATA_DIR, "critical.csv"))) {
  critical <- read.csv(file.path(DATA_DIR, "critical.csv"))
  str(critical)
  critical_codes <- unique(critical$ATC.level.5)
  critical_codes <- critical_codes[!is.na(critical_codes) & critical_codes != ""]
  cat("Critical ATC codes loaded from critical.csv:", length(critical_codes), "\n")
} else {
  critical_codes <- unique(c(EPAR$atc_code, germany$atc_code, ireland$atc_code))
  critical_codes <- critical_codes[!is.na(critical_codes) & critical_codes != ""]
  cat("critical.csv not found in", DATA_DIR, "-- falling back to the derived union of",
      "EMA/Germany/Ireland's own ATC codes:", length(critical_codes), "\n")
}

# FIX #3 (revised): split mfr_company AND mfr_country TOGETHER so each
# manufacturer stays paired with its own country, instead of only
# splitting mfr_country and leaving mfr_company as one glued string.
#
# FIX #5 (this pass): a plain separate_rows(mfr_company, mfr_country, ...)
# requires both columns to split into the SAME number of pieces per row.
# For ~21 Ireland rows they don't (e.g. 3 companies vs. 4 countries), and
# separate_rows silently recycles the shorter list to match the longer
# one -- mispairing manufacturers with the wrong countries and inflating
# the distinct-manufacturer count (530, vs. ~523 verified against the
# raw file) rather than erroring or leaving the row alone. This is the
# same bug already fixed in plots_by_source.R and
# combine_manufacturer_registers.R, applied here too: split only where
# the two "|"-lists have equal length; leave mismatched rows as a single
# unsplit fallback row instead of guessing a pairing.
split_trim <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  # FIX: was str_trim(), which only strips leading/trailing whitespace.
  # 81 Ireland manufacturer names have an embedded newline in the
  # MIDDLE of the string (e.g. "Fannin Limited\nFannin House,\n") --
  # str_trim() leaves that internal newline in place, so two rows that
  # are really the same company end up as different strings and get
  # double-counted (531, vs. ~523-524 verified against the raw file
  # with squishing). str_squish() collapses ALL whitespace (including
  # internal newlines) to single spaces, matching the same fix already
  # applied in plots_by_source.R and combine_manufacturer_registers.R.
  str_squish(str_split(x, "\\|")[[1]])
}

ireland <- ireland %>%
  mutate(
    .company_list  = map(mfr_company, split_trim),
    .country_list  = map(mfr_country, split_trim),
    .lengths_match = map2_lgl(.company_list, .country_list,
                              ~ length(.x) == length(.y) && length(.x) > 0)
  )

ireland_matched <- ireland %>%
  filter(.lengths_match) %>%
  select(-mfr_company, -mfr_country, -.lengths_match) %>%
  unnest(cols = c(.company_list, .country_list)) %>%
  rename(mfr_company = .company_list, mfr_country = .country_list)

ireland_fallback <- ireland %>%
  filter(!.lengths_match) %>%
  select(-.company_list, -.country_list, -.lengths_match)

n_fallback <- nrow(ireland_fallback)
if (n_fallback > 0) {
  cat(n_fallback, "Ireland rows kept unsplit (mfr_company/mfr_country '|'-list length mismatch) -- inspect manually if needed.\n")
}

ireland <- bind_rows(ireland_matched, ireland_fallback)

ireland <- ireland %>% filter(atc_code %in% critical_codes)
str(germany)


# ---- CEP (EDQM Certificates of Suitability), 4th source ----
# Loads cep_atc if it isn't already in the session; falls back to
# reading the raw export. `exists("cep_atc")` may pick up an
# already-processed object (from an earlier CEP+ATC pipeline run) that
# already has clean snake_case names and a holder_country column -- in
# that case we use it as-is. A fresh read of the raw CSV instead comes
# in with spaced column names ("Substance", "Certificate (CEP) Holder")
# and no holder_country, so we clean names and derive it ourselves.
if (!exists("cep_atc")) {
  cep_atc <- read_csv(file.path(DATA_DIR, "EXPORT_WEB_CEP_with_ATC_drugbank.csv"), show_col_types = FALSE)
}

# Always clean names, whether cep_atc was just read or reused from an
# earlier session -- this is idempotent (no-op if already clean), so it
# safely covers both cases without depending on how cep_atc got here.
cep_atc <- janitor::clean_names(cep_atc)

# The raw export bundles company + city + a trailing 2-letter ISO
# country code into one field, e.g. "Glaxo Wellcome London GB". Only
# derive holder_country if it isn't already present (i.e. not reused
# from an already-processed object).
if (!"holder_country" %in% names(cep_atc)) {
  cep_atc <- cep_atc %>%
    mutate(
      holder_country          = str_extract(certificate_cep_holder, "(?<=\\s)[A-Z]{2}$"),
      certificate_cep_holder  = str_trim(str_remove(certificate_cep_holder, "\\s[A-Z]{2}$"))
    )
}

# CEP's holder_country is a 2-letter ISO code (EDQM convention), unlike
# the other three sources which already carry full country names --
# translate before use so it's directly comparable.
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
  # extend if you spot NAs printed by the check below
)

cep_full <- cep_atc %>%
  filter(!is.na(atc_code), atc_code != "") %>%
  separate_rows(atc_code, sep = ",\\s*") %>%       # split multi-code cells first
  filter(atc_code %in% critical_codes) %>%
  transmute(
    atc_code,
    medicine    = substance,
    manufacturer = str_trim(certificate_cep_holder),
    country      = unname(iso2_to_name[holder_country])
  )

n_missing_country <- sum(is.na(cep_full$country) & !is.na(cep_full$manufacturer))
if (n_missing_country > 0) {
  cat(n_missing_country, "CEP rows have an unmapped holder_country code -- add missing codes to iso2_to_name.\n")
}


# ---- keep full copies BEFORE reducing, so Panel A can still use the original columns ----
germany_full <- germany
EPAR_full    <- EPAR
ireland_full <- ireland

# ---- build the reduced, unified `data` (for Panel B / combined analysis) ----
EPAR    <- EPAR[,c("atc_code", "medicine_name", "manufacturer_name", "country")]
ireland <- ireland[,c("atc_code", "product_name", "mfr_company", "mfr_country")]
germany <- germany[,c("atc_code", "arzneimittel", "name", "land")]
cep     <- cep_full[,c("atc_code", "medicine", "manufacturer", "country")]

names(EPAR)    <- c("atc_code", "medicine", "mfr_company", "mfr_country")
names(ireland) <- c("atc_code", "medicine", "mfr_company", "mfr_country")
names(germany) <- c("atc_code", "medicine", "mfr_company", "mfr_country")
names(cep)     <- c("atc_code", "medicine", "mfr_company", "mfr_country")

EPAR$source    <- "EPAR"
ireland$source <- "Ireland"
germany$source <- "Germany"
cep$source     <- "CEP"

data <- rbind(EPAR, ireland, germany, cep)

pal <- c(Germany = "#B9861A", EPAR = "#1D6F5C", Ireland = "#C1461D", CEP = "#1A4D7A")

# ---------------- Panel A: totals per source (unique counts, not summed) ----------------
totals <- bind_rows(
  germany_full %>% transmute(source = "Germany", atc_code,
                             manufacturer = str_trim(name), country = str_trim(land)),
  EPAR_full %>% transmute(source = "EPAR", atc_code,
                          manufacturer = str_trim(manufacturer_name), country = str_trim(country)),
  ireland_full %>% transmute(source = "Ireland", atc_code,
                             manufacturer = str_trim(mfr_company), country = str_trim(mfr_country)),
  cep_full %>% transmute(source = "CEP", atc_code, manufacturer, country)
) %>%
  group_by(source) %>%
  summarise(
    `ATC codes`   = n_distinct(atc_code, na.rm = TRUE),
    # FIX: this used to count any non-missing manufacturer name,
    # regardless of whether its country was known. EMA_data_critical.csv
    # has 4 rows where manufacturer_name is actually leftover document
    # text ("B. CONDITIONS", "RESTRICTION REGARDING SUPPLY AND USE",
    # "responsible for the release of the concerned batch.") rather than
    # a real company -- all 4 have no associated country, which is what
    # gives them away as parsing artifacts, not manufacturers. Requiring
    # a non-missing country too (same rule plots_by_source.R already
    # used) drops these 4 and brings EPAR's count from 144 to the
    # correct 140, matching plots_by_source.R.
    Manufacturers = n_distinct(manufacturer[!is.na(manufacturer) & manufacturer != "" &
                                              !is.na(country) & country != ""]),
    Countries     = n_distinct(country[!is.na(country) & country != ""])
  ) %>%
  pivot_longer(-source, names_to = "metric", values_to = "value") %>%
  mutate(source = factor(source, levels = c("EPAR","Germany","Ireland","CEP")),
         metric = factor(metric, levels = c("ATC codes","Manufacturers","Countries")))

panelA <- ggplot(totals, aes(x = metric, y = value, fill = source)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = value), position = position_dodge(width = 0.75),
            vjust = -0.3, size = 3.2) +
  scale_y_log10() +
  scale_fill_manual(values = pal, name = NULL) +
  labs(x = NULL, y = "Count") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

# ---------------- Panel B: concentration plot (manufacturers vs countries) ----------------
per_code <- bind_rows(
  germany_full %>% transmute(source = "Germany", atc_code, mfr_company = str_trim(name), mfr_country = str_trim(land)),
  EPAR_full %>% transmute(source = "EPAR", atc_code, mfr_company = str_trim(manufacturer_name), mfr_country = str_trim(country)),
  ireland_full %>% transmute(source = "Ireland", atc_code, mfr_company = str_trim(mfr_company), mfr_country = str_trim(mfr_country)),
  cep_full %>% transmute(source = "CEP", atc_code, mfr_company = manufacturer, mfr_country = country)
) %>%
  distinct(source, atc_code, mfr_company, mfr_country) %>%
  group_by(source, atc_code) %>%
  summarise(
    # FIX: same rule as Panel A -- require a non-missing country too,
    # so the handful of EMA rows with document-parsing artifacts as
    # "manufacturer" names (no associated country) aren't counted here
    # either, keeping this panel consistent with the totals in Panel A.
    n_manufacturers = n_distinct(mfr_company[!is.na(mfr_company) & mfr_company != "" &
                                               !is.na(mfr_country) & mfr_country != ""]),
    n_countries     = n_distinct(mfr_country[!is.na(mfr_country) & mfr_country != ""]),
    .groups = "drop"
  ) %>%
  mutate(source = factor(source, levels = c("EPAR","Germany","Ireland","CEP")))

panelB <- ggplot(per_code, aes(x = n_manufacturers, y = n_countries)) +
  geom_hex(bins = 15) +
  scale_fill_gradient(low = "#F4E1D6", high = "#7A2C10", name = "ATC codes") +
  facet_wrap(~ source, scales = "free_x", nrow = 1) +
  labs(x = "Distinct manufacturers", y = "Distinct countries") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

# ---------------- combine ----------------
report_plot <- panelA / panelB + plot_layout(heights = c(1, 1.6))

ggsave(file.path(OUT_DIR, "atc_summary_report_plot.png"), report_plot, width = 15, height = 9, dpi = 300)
print(report_plot)



# ---------------- V2: Panel B only, white background ----------------
ggsave(file.path(OUT_DIR, "atc_summary_report_plot_v2.png"), panelB, width = 16, height = 5, dpi = 300,
       bg = "white")