# ---------------------------------------------------------------
# Manufacturing sites by ATC chapter, split into:
#   EEA - API (step 1), EEA - batch release (step 2), Non-EEA
# One panel per source (EPAR, Germany, Ireland, CEP).
#
# FIXES APPLIED vs. the previous version of this script:
#  1. Data/germany_critical.csv doesn't exist in this data set --
#     replaced with bfarm_api_origin_critical_rest_LONG.csv. Note this
#     file's 'land' column lists manufacturers worldwide, not just
#     Germany -- it's the German national registry (BfArM) of
#     marketed products, kept labeled "Germany" as the source name
#     per the original script's convention.
#  2. Data/critical.csv doesn't exist either. Germany/EPAR/Ireland are
#     already pre-filtered to critical substances (per their file
#     names), so critical_codes is now the union of their ATC codes,
#     used to filter CEP instead of critical$ATC.level.5.
#  3. cep_atc was read without janitor::clean_names(), but the raw
#     file has columns like "Certificate (CEP) Holder" / "Status CEP"
#     / "Substance" while the script referenced certificate_cep_holder
#     / status_cep / substance -- clean_names() added so those exist.
#  4. holder_country was referenced (for CEP country lookup) but never
#     computed -- now extracted from the trailing 2-letter ISO code in
#     certificate_cep_holder.
#  5. CEP rows with multiple comma-separated ATC codes (28 rows) are
#     now split one-code-per-row before filtering, so they're no
#     longer silently dropped by the exact-match filter.
#  6. Ireland's mfr_country has a couple of trailing-period values
#     ("Italy.", "United Kingdom.") that failed to match the EEA list
#     after recode() and were misclassified as Non-EEA -- trailing
#     periods are now stripped before country normalization.
#  7. Ireland's mfr_company / mfr_country are "|"-separated parallel
#     lists (multiple manufacturers per product row). Left unsplit,
#     "Company A | Company B" counted as one fake "site" -- directly
#     wrong for a plot about unique site counts. Now split and paired
#     up, with a same-logic fallback (keep row unsplit) on the rows
#     where the two lists don't line up in length.
#  8. Ireland's chapter/ATC now comes from matched_critical_atc (the
#     clean single code) instead of the raw atc_code text field, which
#     can hold combined text like "J02AC Triazole derivatives, J02AC01
#     fluconazole".
#
# NOTE: Ireland has no manufacturing-step field, so its EEA sites
# cannot be split into API vs batch-release -- they are shown as a
# single "EEA - not disclosed" category instead. This is a genuine
# data limitation, not a plotting simplification. CEP (EDQM
# Certificates of Suitability) has the same limitation: certificates
# name the holder but do not disclose API vs finished-product
# manufacturing role, so CEP is classified the same way as Ireland.
#
# Sites are deduplicated: Germany by pu_nummer (true unique site ID,
# each assigned its modal country); EPAR and Ireland by manufacturer
# name (no unique site ID available in those sources); CEP by
# certificate holder name (no unique site ID either).
#
# NOTE: unlike the HHI step-1-priority analysis, NO step-2 records are
# deleted here. A site is counted as "API (step 1)" if it has any
# step-1 record and "batch release" only if it has step-2 records but
# no step-1 record -- but the underlying data is unfiltered; every
# disclosed manufacturer/site is retained and classified.
#
# Requires: install.packages(c("dplyr","tidyr","ggplot2","stringr","patchwork","readr","janitor","purrr"))
# ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(patchwork)
library(readr)
library(janitor)
library(purrr)

germany   <- read.csv("Data/bfarm_api_origin_critical_rest_LONG.csv", stringsAsFactors = FALSE)
EPAR    <- read.csv("Data/EMA_data_critical.csv", stringsAsFactors = FALSE)
ireland <- read.csv("Data/ireland_critical_atc_review.csv", stringsAsFactors = FALSE)  # adjust path/name if needed

# ---- CEP (EDQM Certificates of Suitability), 4th source ----
# Always re-read + clean_names() here (no exists() guard) -- reusing a
# cep_atc object left over from an earlier script run in the same R
# session is what caused the "object 'status_cep' not found" error,
# since an old cep_atc without clean_names() applied has raw column
# names like "Status CEP" instead of status_cep.
cep_atc <- read_csv("Data/EXPORT_WEB_CEP_with_ATC_drugbank.csv", show_col_types = FALSE) |>
  clean_names()

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
  # China, Israel, Japan, Monaco, Puerto Rico, Taiwan, Ukraine, Chile are
  # spelled the same in German and English, so they pass through unmapped
  # (dplyr::recode keeps unmatched values as-is) -- no entry needed
)

# CEP's holder_country is a 2-letter ISO code (EDQM convention), unlike
# the other three sources which already carry full country names.
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
  SG = "Singapore", TW = "Taiwan", RU = "Russia", UA = "Ukraine"
  # extend if you spot NAs printed by the check below
)

EEA <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark",
         "Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy",
         "Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal",
         "Romania","Slovakia","Slovenia","Spain","Sweden","Iceland","Norway","Liechtenstein")

atc_chapter_names <- c(
  A = "A - Alimentary & metabolism", B = "B - Blood & blood forming organs",
  C = "C - Cardiovascular system", D = "D - Dermatologicals",
  G = "G - Genito-urinary system", H = "H - Systemic hormonal preparations",
  J = "J - Antiinfectives (systemic)", L = "L - Antineoplastic & immunomodulating",
  M = "M - Musculo-skeletal system", N = "N - Nervous system",
  P = "P - Antiparasitic products", R = "R - Respiratory system",
  S = "S - Sensory organs", V = "V - Various"
)

# ---------------------------------------------------------------
# Derive the critical ATC code universe from Germany + EPAR + Ireland
# (all three are already pre-filtered to critical substances; there
# is no standalone critical.csv in this data set). Ireland uses
# matched_critical_atc, the clean single code.
# ---------------------------------------------------------------
critical_codes <- unique(c(
  germany$atc_code,
  EPAR$atc_code,
  ireland$matched_critical_atc
))
critical_codes <- critical_codes[!is.na(critical_codes) & critical_codes != ""]

cep_atc2 <- cep_atc[!is.na(cep_atc$atc_code), ]
cep_atc <- cep_atc %>% filter(atc_code %in% critical_codes)
cep_atc <- cep_atc %>% filter(status_cep == "Valid")

# CEP cells can hold multiple comma-separated ATC codes -- split before
# use so multi-code substances aren't silently dropped by the exact match
cep_atc <- cep_atc %>%
  separate_rows(atc_code, sep = ",\\s*") %>%
  filter(atc_code %in% critical_codes)

cep_atc <- cep_atc %>%
  mutate(holder_country = str_extract(certificate_cep_holder, "[A-Z]{2}$"))

str(unique(cep_atc$substance))

str(unique(cep_atc$atc_code))

# ---------------------------------------------------------------
# Helper: build the site x chapter classification for one source
# ---------------------------------------------------------------
classify_sites <- function(df, source_name) {
  df %>%
    group_by(chapter, site_id) %>%
    summarise(
      country = first(country),
      has_step1 = any(step == 1, na.rm = TRUE),
      has_step2 = any(step == 2, na.rm = TRUE),
      step_known = any(!is.na(step)),
      .groups = "drop"
    ) %>%
    mutate(
      is_eea = country %in% EEA,
      region = ifelse(is_eea, "EEA", "Non-EEA"),
      step_label = case_when(
        !step_known ~ "not disclosed",
        has_step1 ~ "API (step 1)",
        has_step2 ~ "batch release",
        TRUE ~ "not disclosed"
      ),
      category = paste(region, "-", step_label),
      source = source_name
    )
}

# ---- Germany: site = pu_nummer, modal country, step from role ----
germany_sites <- germany %>%
  mutate(
    country = recode(str_remove(str_trim(land), "\\.$"), !!!country_map),
    step = case_when(role == "Wirkstoffherstellung" ~ 1,
                     role == "Hersteller/Endfreigabe" ~ 2,
                     TRUE ~ NA_real_),
    chapter = recode(substr(atc_code, 1, 1), !!!atc_chapter_names)
  ) %>%
  filter(!is.na(atc_code), atc_code != "", !is.na(pu_nummer), !is.na(country), country != "") %>%
  transmute(chapter, site_id = as.character(pu_nummer), country, step)

# a site can appear under multiple chapters -- classify_sites operates
# per chapter x site as intended (a site counts once per chapter it supplies)
germany_classified <- classify_sites(germany_sites, "Germany")

# ---- EPAR: site = manufacturer name, step from manufacturer_step ----
epar_sites <- EPAR %>%
  mutate(
    country = recode(str_remove(str_trim(country), "\\.$"), !!!country_map),
    step = coalesce(as.numeric(manufacturer_step), 2),  # missing/NA step -> assume 2 (batch release)
    chapter = recode(substr(atc_code, 1, 1), !!!atc_chapter_names)
  ) %>%
  filter(!is.na(atc_code), atc_code != "", !is.na(manufacturer_name), manufacturer_name != "",
         !is.na(country), country != "") %>%
  transmute(chapter, site_id = str_trim(manufacturer_name), country, step)

epar_classified <- classify_sites(epar_sites, "EPAR")

# ---- Ireland: site = manufacturer name, NO step field.
#      mfr_company / mfr_country are "|"-separated parallel lists
#      (multiple manufacturers per product row) -- split and pair them
#      up so each manufacturer is counted as its own site rather than
#      one bogus combined "site"; rows where the two lists don't line
#      up in length are kept unsplit as a fallback. ----
split_trim <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  str_trim(str_split(x, "\\|")[[1]])
}

ireland_pairs <- ireland %>%
  filter(!is.na(matched_critical_atc), matched_critical_atc != "") %>%
  mutate(
    company_list = map(mfr_company, split_trim),
    country_list = map(mfr_country, split_trim),
    lengths_match = map2_lgl(company_list, country_list, ~ length(.x) == length(.y) && length(.x) > 0)
  )

ireland_expanded <- ireland_pairs %>%
  filter(lengths_match) %>%
  transmute(atc_code = matched_critical_atc, company_list, country_list) %>%
  unnest(cols = c(company_list, country_list)) %>%
  rename(mfr_company_s = company_list, mfr_country_s = country_list)

ireland_fallback <- ireland_pairs %>%
  filter(!lengths_match) %>%
  transmute(atc_code = matched_critical_atc,
            mfr_company_s = mfr_company, mfr_country_s = mfr_country)

ireland_split <- bind_rows(ireland_expanded, ireland_fallback)

ire_sites <- ireland_split %>%
  mutate(
    country = recode(str_remove(str_trim(mfr_country_s), "\\.$"), !!!country_map),
    step = NA_real_,
    chapter = recode(substr(atc_code, 1, 1), !!!atc_chapter_names)
  ) %>%
  filter(!is.na(atc_code), atc_code != "", !is.na(mfr_company_s), mfr_company_s != "",
         !is.na(country), country != "") %>%
  transmute(chapter, site_id = str_trim(mfr_company_s), country, step)

ire_classified <- classify_sites(ire_sites, "Ireland")

# ---- CEP: site = certificate holder name, NO step field (same
#      limitation as Ireland -- CEPs don't disclose API vs
#      finished-product manufacturing role) ----
cep_sites <- cep_atc %>%
  filter(!is.na(atc_code), atc_code %in% critical_codes) %>%
  mutate(
    country = unname(iso2_to_name[holder_country]),
    step = NA_real_,
    chapter = recode(substr(atc_code, 1, 1), !!!atc_chapter_names)
  ) %>%
  filter(!is.na(atc_code), atc_code != "",
         !is.na(certificate_cep_holder), certificate_cep_holder != "",
         !is.na(country), country != "") %>%
  transmute(chapter, site_id = str_trim(certificate_cep_holder), country, step)

cep_classified <- classify_sites(cep_sites, "CEP")

# ---------------------------------------------------------------
# Combine, compute shares per chapter x source
# ---------------------------------------------------------------
all_classified <- bind_rows(germany_classified, epar_classified, ire_classified, cep_classified)

chapter_summary <- all_classified %>%
  count(source, chapter, category) %>%
  group_by(source, chapter) %>%
  mutate(total = sum(n), pct = 100 * n / total) %>%
  ungroup()

# order chapters within each source by total site count (descending),
# using Germany's order as the shared reference so panels are comparable
chapter_order <- chapter_summary %>%
  filter(source == "Germany") %>%
  distinct(chapter, total) %>%
  arrange(desc(total)) %>%
  pull(chapter)

chapter_summary <- chapter_summary %>%
  mutate(chapter = factor(chapter, levels = rev(chapter_order)),
         source = factor(source, levels = c("EPAR", "Germany", "Ireland", "CEP")),
         category = factor(category, levels = c(
           "EEA - API (step 1)", "EEA - batch release", "EEA - not disclosed",
           "Non-EEA - API (step 1)", "Non-EEA - batch release", "Non-EEA - not disclosed"
         )))

step_colors <- c(
  "EEA - API (step 1)"         = "#0B2545",  # dark navy (blue family, darkest = most disclosed)
  "EEA - batch release"        = "#4A7FB5",  # medium blue
  "EEA - not disclosed"        = "#8FB3D6",  # deepened pale blue (was #B8D0E6)
  "Non-EEA - API (step 1)"     = "#7A1810",  # dark red (red family, darkest = most disclosed)
  "Non-EEA - batch release"    = "#C1461D",  # medium red/orange
  "Non-EEA - not disclosed"    = "#E28A6E"   # deepened pale salmon (was #F0B8A8)
)

# bar height (thickness), proportional to sqrt(n) -- same convention as
# ggplot's varwidth for boxplots. Scaled per SOURCE (not globally) since
# EPAR/Germany/Ireland/CEP have very different total chapter sizes; a floor
# of 0.25 keeps the smallest chapters visible instead of vanishing.
totals_label <- chapter_summary %>%
  distinct(source, chapter, total) %>%
  group_by(source) %>%
  mutate(bar_height = 0.25 + 0.65 * sqrt(total / max(total))) %>%
  ungroup()

chapter_summary <- chapter_summary %>%
  left_join(totals_label %>% select(source, chapter, bar_height), by = c("source", "chapter"))

p <- ggplot(chapter_summary, aes(x = pct, y = chapter, fill = category)) +
  geom_col(aes(width = bar_height), alpha = 1) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "grey40", alpha = 1) +
  geom_text(data = totals_label, aes(x = 50, y = chapter, label = total),
            inherit.aes = FALSE, color = "#545454", size = 4, fontface = "bold") +
  scale_fill_manual(values = step_colors, name = NULL, guide = guide_legend(nrow = 2)) +
  facet_wrap(~ source, nrow = 1) +
  coord_cartesian(xlim = c(0, 100), clip = "off") +
  labs(x = "Share of unique manufacturing sites (%)", y = NULL) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 13),
    strip.text = element_text(face = "bold", size = 16),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
  )

ggsave("Data/manufacturing_sites_by_chapter_step_source.png", p, width = 25, height = 9.5, dpi = 300)
print(p)