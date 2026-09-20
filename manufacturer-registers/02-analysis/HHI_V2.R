# ---------------------------------------------------------------
# ONE-SHOT SCRIPT (now with CEP as a fourth source):
#   1. For Germany and EPAR: per ATC code, delete step-2 producer
#      rows for any code that ALSO has step-1 (API) data -- i.e.
#      when both steps are present, keep only step-1. Codes with
#      ONLY step-2 keep their step-2 rows (no step-1 alternative
#      exists to prioritise). Ireland AND CEP have no step field,
#      so both are used unchanged (all manufacturing records).
#   2. Compute country-level HHI per ATC code, per source.
#   3. Count how often each source reports the HIGHEST HHI for a
#      given ATC code (i.e. which source drives the worst-case
#      concentration view), both as a raw count and as a % of that
#      source's own coverage.
#
# FIXES APPLIED vs. the previous version of this script:
#  1. germany was read from Data/germany_critical.csv, an
#     intermediate cache file WRITTEN BY A DIFFERENT SCRIPT
#     (atc_summary_report_plot.R). That makes this script's output
#     depend on run order and on that other script's exact version --
#     if germany_critical.csv is stale (e.g. written by an older copy
#     with the Germany UK-merge bug, or not regenerated at all), this
#     script would silently inherit the problem with no way to tell.
#     Now reads directly from bfarm_api_origin_critical_rest_LONG.csv
#     and does its own filtering, making it self-contained.
#  2. ireland was read from data/manufacturer-registers/out/ireland.csv, which doesn't exist --
#     the real file is ireland_critical_atc_review.csv. Also used the
#     raw atc_code column directly, which holds messy combined text
#     (e.g. "J02AC Triazole derivatives, J02AC01 fluconazole") rather
#     than a clean level-5 code -- switched to matched_critical_atc.
#     mfr_company/mfr_country were used as-is without splitting the
#     "|"-separated multi-manufacturer lists (353/1166 rows affected)
#     -- for an HHI calculation, which is fundamentally about counting
#      distinct sites, leaving these as one glued composite "site"
#     string materially understates diversity and overstates
#     concentration. Added the same length-checked split + fallback
#     logic (with str_squish) used in the other report scripts.
#  3. cep was read without the "Data/" path prefix used everywhere
#     else in this pipeline -- fixed for consistency.
#  4. `if (!exists("critical"))` skipped reloading Data/critical.csv if
#     an object of that name was already in the R session -- removed;
#     always reads fresh.
#  5. `cep %>% filter(atc_code %in% critical$`ATC level 5`)` -- this is
#     the exact same bug found earlier in plots_by_source.R: base R's
#     read.csv() mangles the column name "ATC level 5" into
#     "ATC.level.5" (dots replace spaces), so critical$`ATC level 5`
#     resolved to NULL and `atc_code %in% NULL` was FALSE for every
#     row -- silently dropping ALL of CEP's data before any other CEP
#     processing ran. Fixed to critical$ATC.level.5. This filter was
#     ALSO applied before splitting CEP's multi-code cells (comma-
#     separated ATC codes in one field), so even with the column name
#     fixed, a combined cell like "G03CA03, G03HB01" would never
#     exactly match a single code in the critical list -- moved the
#     filter to after separate_rows(), same fix already applied in
#     combine_manufacturer_registers.R for the same underlying issue.
#  6. EPAR/Germany's site_id (manufacturer_name / name) wasn't
#     str_squish()'d -- some EMA manufacturer names have an embedded
#     newline (a data-quality issue in the raw file) that under-splits
#     what should be treated as one site into what looks like two
#     different site_ids. Added str_squish() for consistency with the
#     other report scripts.
#
# Requires: install.packages(c("dplyr","tidyr","ggplot2","stringr","purrr","janitor"))
# ---------------------------------------------------------------
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
library(ggplot2)
library(stringr)
library(purrr)

germany_raw <- read.csv(file.path(DATA_DIR, "bfarm_api_origin_critical_rest_LONG.csv"), stringsAsFactors = FALSE) %>%
  filter(role != "Zulassungsinhaber")   # keep manufacturers only, drop marketing-authorization holders

EPAR    <- read.csv(file.path(DATA_DIR, "EMA_data_critical.csv"), stringsAsFactors = FALSE)
ireland <- read.csv(file.path(DATA_DIR, "ireland_critical_atc_review.csv"), stringsAsFactors = FALSE)
cep     <- read.csv(file.path(DATA_DIR, "EXPORT_WEB_CEP_with_ATC_drugbank.csv"), stringsAsFactors = FALSE)
names(cep) <- janitor::make_clean_names(names(cep))
## -> columns now e.g. substance, certificate_cep_holder, status_cep, atc_code, ...

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

## CEP holder strings end in a trailing 2-letter ISO country code
## (e.g. "Sigma-Aldrich Corporation Saint Louis US" -> "US"), not a
## German location name like the BfArM (Germany) data uses -- so CEP
## needs its own ISO2 -> full-name map, separate from country_map above.
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
  # extend this list if you spot unmatched codes in the check below
)

# Splits an Ireland "|"-separated list and squishes each piece (see
# fix #2 above). Returns character(0) for NA/blank input.
split_squish <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  str_squish(str_split(x, "\\|")[[1]])
}

compute_hhi <- function(df, source_name) {
  df %>%
    distinct(atc_code, site_id, country) %>%
    count(atc_code, country, name = "n_sites") %>%
    group_by(atc_code) %>%
    mutate(share = n_sites / sum(n_sites)) %>%
    summarise(
      n_countries         = n_distinct(country),
      n_sites_total       = sum(n_sites),
      hhi                 = sum(share^2) * 10000,
      effective_countries = 10000 / hhi,
      .groups = "drop"
    ) %>%
    mutate(source = source_name)
}

# ---------------------------------------------------------------
# 1a. Germany: delete step-2 producers for codes that also have step-1
# ---------------------------------------------------------------
germany_clean <- germany_raw %>%
  mutate(land = recode(str_trim(land), !!!country_map)) %>%
  filter(!is.na(atc_code), atc_code != "", !is.na(pu_nummer), !is.na(land), land != "")

atc_has_step1_ger <- germany_clean %>%
  filter(role == "Wirkstoffherstellung") %>%
  distinct(atc_code) %>% pull(atc_code)

germany_priority <- germany_clean %>%
  filter(
    (atc_code %in% atc_has_step1_ger  & role == "Wirkstoffherstellung") |
      (!atc_code %in% atc_has_step1_ger & role == "Hersteller/Endfreigabe")
  ) %>%
  transmute(atc_code, site_id = as.character(pu_nummer), country = land)

cat("Germany: codes with both steps -> step-2 producers deleted, step-1 kept:", length(atc_has_step1_ger), "\n")
cat("Germany: codes with ONLY step-2 -> step-2 kept as fallback:",
    n_distinct(germany_clean$atc_code) - length(atc_has_step1_ger), "\n")

hhi_germany <- compute_hhi(germany_priority, "Germany")

# ---------------------------------------------------------------
# 1b. EPAR: same logic
# ---------------------------------------------------------------
epar_clean <- EPAR %>%
  mutate(country = recode(str_trim(country), !!!country_map)) %>%
  filter(!is.na(atc_code), atc_code != "",
         !is.na(manufacturer_name), manufacturer_name != "",
         !is.na(country), country != "")

atc_has_step1_epar <- epar_clean %>%
  filter(manufacturer_step == 1) %>%
  distinct(atc_code) %>% pull(atc_code)

epar_priority <- epar_clean %>%
  filter(
    (atc_code %in% atc_has_step1_epar  & manufacturer_step == 1) |
      (!atc_code %in% atc_has_step1_epar & manufacturer_step == 2)
  ) %>%
  transmute(atc_code, site_id = str_squish(manufacturer_name), country)

cat("EPAR: codes with both steps -> step-2 producers deleted, step-1 kept:", length(atc_has_step1_epar), "\n")
cat("EPAR: codes with ONLY step-2 -> step-2 kept as fallback:",
    n_distinct(epar_clean$atc_code) - length(atc_has_step1_epar), "\n\n")

hhi_epar <- compute_hhi(epar_priority, "EPAR")

# ---------------------------------------------------------------
# 1c. Ireland: unchanged (no step field to prioritise), but uses
# matched_critical_atc (clean single code) instead of the messy raw
# atc_code text field, and splits mfr_company/mfr_country "|"-lists
# with a length check so each manufacturer pairs with its own
# country instead of counting a glued multi-company string as one
# site (see fix #2 above).
# ---------------------------------------------------------------
ireland_pairs <- ireland %>%
  filter(!is.na(matched_critical_atc), matched_critical_atc != "") %>%
  mutate(
    company_list = map(mfr_company, split_squish),
    country_list = map(mfr_country, split_squish),
    lengths_match = map2_lgl(company_list, country_list,
                             ~ length(.x) == length(.y) && length(.x) > 0)
  )

ireland_matched <- ireland_pairs %>%
  filter(lengths_match) %>%
  select(atc_code = matched_critical_atc, company_list, country_list) %>%
  unnest(cols = c(company_list, country_list)) %>%
  rename(mfr_company = company_list, mfr_country = country_list)

ireland_fallback <- ireland_pairs %>%
  filter(!lengths_match) %>%
  transmute(
    atc_code    = matched_critical_atc,
    mfr_company = str_squish(mfr_company),
    mfr_country = str_squish(mfr_country)
  )

n_fallback <- nrow(ireland_fallback)
if (n_fallback > 0) {
  cat("Ireland rows kept unsplit (company/country list length mismatch):", n_fallback, "\n")
}

ireland_std <- bind_rows(ireland_matched, ireland_fallback) %>%
  mutate(mfr_country = recode(mfr_country, !!!country_map)) %>%
  filter(!is.na(atc_code), atc_code != "",
         !is.na(mfr_company), mfr_company != "",
         !is.na(mfr_country), mfr_country != "") %>%
  transmute(atc_code, site_id = mfr_company, country = mfr_country)

hhi_ireland <- compute_hhi(ireland_std, "Ireland")

# ---------------------------------------------------------------
# 1d. CEP: unchanged (no step field to prioritise, like Ireland).
# atc_code can hold MULTIPLE comma-separated codes per row -- split
# those into one row per code BEFORE filtering to critical (a combined
# cell like "G03CA03, G03HB01" would never exactly match a single
# code in critical.csv, silently dropping the whole row otherwise).
# ---------------------------------------------------------------
## -----------------------------------------------------------------
## Determine the critical ATC code universe.
##
## FALLBACK (added): this script read Data/critical.csv unconditionally
## near the top, but that file isn't part of this data set, so a fresh
## session stopped there. Handled the same way as ComebineAll4SourcesV2.R:
## use Data/critical.csv when it exists, and otherwise fall back to the
## derived union of EMA/Germany/Ireland's own ATC codes. The read moved
## down to here, where the three cleaned frames the fallback needs exist,
## and where its only use -- the CEP filter below -- is. Germany, EPAR
## and Ireland are already pre-filtered to critical substances; CEP is
## not, which is why only CEP is filtered against this universe.
## -----------------------------------------------------------------
if (file.exists(file.path(DATA_DIR, "critical.csv"))) {
  critical <- read.csv(file.path(DATA_DIR, "critical.csv"), stringsAsFactors = FALSE)
  critical_codes <- unique(critical$ATC.level.5)
  critical_codes <- critical_codes[!is.na(critical_codes) & critical_codes != ""]
  cat("Critical ATC codes loaded from critical.csv:", length(critical_codes), "\n")
} else {
  critical_codes <- unique(c(epar_clean$atc_code, germany_clean$atc_code, ireland_std$atc_code))
  critical_codes <- critical_codes[!is.na(critical_codes) & critical_codes != ""]
  cat("critical.csv not found in", DATA_DIR, "-- falling back to the derived union of",
      "EMA/Germany/Ireland's own ATC codes:", length(critical_codes), "\n")
}

cep_clean <- cep %>%
  filter(!is.na(atc_code), atc_code != "") %>%
  separate_rows(atc_code, sep = ",\\s*") %>%
  filter(atc_code %in% critical_codes) %>%
  mutate(
    holder_country_iso = str_extract(certificate_cep_holder, "[A-Z]{2}$"),
    country = unname(iso2_to_name[holder_country_iso])
  ) %>%
  filter(!is.na(certificate_cep_holder), certificate_cep_holder != "",
         !is.na(country), country != "")

missing_cep_codes <- cep_clean %>%
  distinct(holder_country_iso) %>%
  filter(!holder_country_iso %in% names(iso2_to_name)) %>%
  pull(holder_country_iso)
if (length(missing_cep_codes) > 0) {
  cat("CEP: country codes not in iso2_to_name, dropped as NA:\n")
  print(missing_cep_codes)
}

cep_std <- cep_clean %>%
  transmute(atc_code, site_id = str_squish(certificate_cep_holder), country)

hhi_cep <- compute_hhi(cep_std, "CEP")

# ---------------------------------------------------------------
# 2. Combine
# ---------------------------------------------------------------
hhi_all <- bind_rows(hhi_epar, hhi_germany, hhi_ireland, hhi_cep) %>%
  mutate(source = factor(source, levels = c("EPAR", "Germany", "Ireland", "CEP")))

write.csv(hhi_all, file.path(OUT_DIR, "hhi_step1_priority_final.csv"), row.names = FALSE)

# ---------------------------------------------------------------
# 3. How often does each source hold the HIGHEST HHI per ATC code?
# ---------------------------------------------------------------
winner_summary <- hhi_all %>%
  group_by(atc_code) %>%
  filter(hhi == max(hhi)) %>%   # ties counted for every tied source
  ungroup() %>%
  count(source, name = "n_times_highest")

coverage <- hhi_all %>% count(source, name = "n_codes_covered")

winner_pct <- winner_summary %>%
  left_join(coverage, by = "source") %>%
  mutate(pct_of_own_coverage = round(100 * n_times_highest / n_codes_covered, 1))

n_ties <- hhi_all %>%
  group_by(atc_code) %>%
  filter(hhi == max(hhi)) %>%
  summarise(n_tied = n(), .groups = "drop") %>%
  filter(n_tied > 1) %>%
  nrow()

cat("=========================================================\n")
cat("HOW OFTEN EACH SOURCE HOLDS THE HIGHEST HHI\n")
cat("=========================================================\n")
print(winner_pct)
cat("\nTotal distinct ATC codes with data from at least one source:", n_distinct(hhi_all$atc_code), "\n")
cat("ATC codes with an exact tie for highest HHI between 2+ sources:", n_ties, "\n")

write.csv(winner_pct, file.path(OUT_DIR, "hhi_highest_by_source_final.csv"), row.names = FALSE)

library(xtable)
xtable(winner_pct)

# ---------------------------------------------------------------
# HHI distribution by ATC Level 1 chapter, faceted by source
# (EPAR / Germany / Ireland / CEP), using the step-1-priority
# filtered HHI data (Data/hhi_step1_priority_final.csv from
# hhi_full_pipeline.R).
#
# Dashed reference lines at HHI = 1500 and 2500 mark the standard
# antitrust thresholds (US DOJ/FTC Horizontal Merger Guidelines):
#   < 1500 = unconcentrated, 1500-2500 = moderately concentrated,
#   > 2500 = highly concentrated. Used here only as an intuition
#   anchor, not a formal application of antitrust doctrine to a
#   non-market setting.
#
# Requires: install.packages(c("dplyr","ggplot2","stringr","forcats","RColorBrewer","ggh4x"))
# ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)

hhi_all <- read.csv(file.path(OUT_DIR, "hhi_step1_priority_final.csv"), stringsAsFactors = FALSE)

# ---- ATC Level 1 chapter names ----
chapter_names <- c(
  A = "A - Alimentary & metabolism", B = "B - Blood & blood forming organs",
  C = "C - Cardiovascular system", D = "D - Dermatologicals",
  G = "G - Genito-urinary system", H = "H - Systemic hormonal preparations",
  J = "J - Antiinfectives (systemic)", L = "L - Antineoplastic & immunomodulating",
  M = "M - Musculo-skeletal system", N = "N - Nervous system",
  P = "P - Antiparasitic products", R = "R - Respiratory system",
  S = "S - Sensory organs", V = "V - Various"
)

hhi_all <- hhi_all %>%
  mutate(
    chapter_code = substr(atc_code, 1, 1),
    chapter = recode(chapter_code, !!!chapter_names),
    source  = factor(source, levels = c("EPAR", "Germany", "Ireland", "CEP"))
  )

# ---- fix chapter order once, using the MEDIAN HHI across all
#      sources combined, so all panels share the same row order
#      and stay easy to compare side by side ----
chapter_order <- hhi_all %>%
  group_by(chapter) %>%
  summarise(med = median(hhi)) %>%
  arrange(desc(med)) %>%
  pull(chapter)

hhi_all$chapter <- factor(hhi_all$chapter, levels = rev(chapter_order))

# ---- qualitative palette, one color per chapter (consistent across panels) ----
chapter_pal <- setNames(
  colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(length(chapter_order)),
  chapter_order
)

# y-axis label colors, in the SAME order as the factor levels (rev(chapter_order))
# so each label's text color matches its own box fill color
axis_label_colors <- chapter_pal[rev(chapter_order)]

# established source palette, reused from all earlier plots in this analysis
source_pal <- c(EPAR = "#1D6F5C", Germany = "#B9861A", Ireland = "#C1461D", CEP = "#4B5FAD")

p <- ggplot(hhi_all, aes(x = hhi, y = chapter, fill = chapter)) +
  geom_vline(xintercept = c(1500, 2500), linetype = "dashed", color = "grey40") +
  geom_boxplot(outlier.size = 1, show.legend = FALSE, varwidth = TRUE) +
  scale_fill_manual(values = chapter_pal) +
  ggh4x::facet_wrap2(~ source, nrow = 1, strip = ggh4x::strip_themed(
    text_x = ggh4x::elem_list_text(
      colour = unname(source_pal[levels(hhi_all$source)]),
      face   = rep("bold", nlevels(hhi_all$source)),
      size   = rep(13, nlevels(hhi_all$source))
    )
  )) +
  scale_x_continuous(breaks = c(0, 2500, 5000, 7500, 10000)) +
  labs(x = "HHI", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(face = "bold", size = 9, colour = axis_label_colors),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(file.path(OUT_DIR, "hhi_by_chapter_by_source.png"), p, width = 17, height = 8, dpi = 300)
print(p)

# ---- also save each source individually ----
for (src in levels(hhi_all$source)) {
  p_single <- hhi_all %>%
    filter(source == src) %>%
    ggplot(aes(x = hhi, y = chapter, fill = chapter)) +
    geom_vline(xintercept = c(1500, 2500), linetype = "dashed", color = "grey40") +
    geom_boxplot(outlier.size = 1, show.legend = FALSE, varwidth = TRUE) +
    scale_fill_manual(values = chapter_pal) +
    scale_x_continuous(breaks = c(0, 2500, 5000, 7500, 10000)) +
    labs(title = src, x = "HHI", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, colour = source_pal[[src]]),
      axis.text.y = element_text(face = "bold", size = 9, colour = axis_label_colors),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
  ggsave(file.path(OUT_DIR, paste0("hhi_by_chapter_", src, ".png")), p_single, width = 8, height = 8, dpi = 300)
}



# ---------------------------------------------------------------
# Single plot, 4 subplots (one per source): distribution of
# country-level HHI, using ONLY the step-1-priority filtered data
# (Germany/EPAR: step-2 producers deleted for any ATC code that
# also has step-1 data; Ireland/CEP: unchanged, no step field exists).
#
# Requires: install.packages(c("dplyr","ggplot2"))
# Reads: Data/hhi_step1_priority_final.csv (from hhi_full_pipeline.R)
# ---------------------------------------------------------------
library(dplyr)
library(ggplot2)

hhi_all <- read.csv(file.path(OUT_DIR, "hhi_step1_priority_final.csv"), stringsAsFactors = FALSE) %>%
  mutate(source = factor(source, levels = c("EPAR", "Germany", "Ireland", "CEP")))

source_pal <- c(EPAR = "#1D6F5C", Germany = "#B9861A", Ireland = "#C1461D", CEP = "#4B5FAD")

p <- ggplot(hhi_all, aes(x = hhi, fill = source)) +
  geom_vline(xintercept = c(1500, 2500), linetype = "dashed", color = "grey50") +
  geom_histogram(binwidth = 500, boundary = 0, color = "white") +
  facet_wrap(~ source, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = source_pal, guide = "none") +
  scale_x_continuous(breaks = c(0, 2500, 5000, 7500, 10000)) +
  labs(
    title = "Country-level HHI per critical ATC code (step-1-priority filtered)",
    x = "HHI (0 = diversified, 10000 = single-country)",
    y = "Number of ATC codes"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(OUT_DIR, "hhi_step1_priority_by_source.png"), p, width = 12, height = 5, dpi = 300)
print(p)