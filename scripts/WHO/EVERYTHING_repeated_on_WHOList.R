source("paths.R")

recode <- dplyr::recode

setwd("/Users/elmadervic/Documents/Github/MedicationSupply2026Final/WHO")

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(tidytext)
library(scales)
library(purrr)

## WHO EML critical codes + per-source WHO files, all written by 01_combine_sources_WHO.R
critical_codes_who <- read.csv(file.path(PROC_DIR, "who_eml_critical_codes.csv"),
                               stringsAsFactors = FALSE)$atc_code
cat("WHO EML critical codes:", length(critical_codes_who), "\n")

integer_breaks <- function(n = 5) {
  function(x) {
    rng <- range(x, na.rm = TRUE)
    if (!is.finite(rng[1]) || !is.finite(rng[2])) return(numeric(0))
    if (diff(rng) == 0) return(round(rng[1]))
    step <- max(1, round(diff(rng) / n))
    seq(floor(rng[1]), ceiling(rng[2]), by = step)
  }
}

data <- read.csv(file.path(PROC_DIR, "manufacturer_registers_combined.csv"), stringsAsFactors = FALSE)

cat("rows read from manufacturer_registers_combined.csv:", nrow(data), "\n")

x <- data[data$source == "cep", ]
str(x)
str(unique(x$atc_code))

split_trim <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  str_trim(str_split(x, "\\|")[[1]])
}

data <- data %>%
  mutate(
    .company_list  = map(mfr_company, split_trim),
    .country_list  = map(mfr_country, split_trim),
    .lengths_match = map2_lgl(.company_list, .country_list,
                              ~ length(.x) == length(.y) && length(.x) > 0)
  )

data_matched <- data %>%
  filter(.lengths_match) %>%
  select(-mfr_company, -mfr_country, -.lengths_match) %>%
  unnest(cols = c(.company_list, .country_list)) %>%
  rename(mfr_company = .company_list, mfr_country = .country_list)

data_fallback <- data %>%
  filter(!.lengths_match) %>%
  select(-.company_list, -.country_list, -.lengths_match)

n_fallback <- nrow(data_fallback)
if (n_fallback > 0) {
  cat(n_fallback, "rows kept unsplit (mfr_company/mfr_country '|'-list length mismatch) -- inspect manually if needed.\n")
}

data <- bind_rows(data_matched, data_fallback)

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

data <- data %>%
  mutate(
    mfr_company = str_trim(mfr_company),
    mfr_country = recode(str_trim(mfr_country), !!!country_map),
    source = str_to_title(source),
    source = recode(source, "Ema" = "EPAR", "Cep" = "CEP")
  ) %>%
  filter(!is.na(atc_code), atc_code != "",
         !is.na(mfr_company), mfr_company != "",
         !is.na(mfr_country), mfr_country != "")

cat("rows after cleaning/splitting:", nrow(data), "\n")

source_pal <- c(EPAR = "#1D6F5C", Germany = "#B9861A", Ireland = "#C1461D", CEP = "#1A4D7A")
source_levels <- c("EPAR", "Germany", "Ireland", "CEP")
data$source <- factor(data$source, levels = source_levels)

cat("=========================================================\n")
cat("SUMMARY BY SOURCE\n")
cat("=========================================================\n")
data %>%
  group_by(source) %>%
  summarise(
    n_rows = n(),
    n_atc_codes = n_distinct(atc_code),
    n_manufacturers = n_distinct(mfr_company),
    n_countries = n_distinct(mfr_country)
  ) %>%
  print()

atc_summary <- data %>%
  distinct(atc_code, mfr_company, mfr_country, source) %>%
  group_by(source, atc_code) %>%
  summarise(
    n_manufacturers = n_distinct(mfr_company),
    n_countries = n_distinct(mfr_country),
    .groups = "drop"
  )

totals <- data %>%
  group_by(source) %>%
  summarise(
    `ATC codes` = n_distinct(atc_code),
    Manufacturers = n_distinct(mfr_company),
    Countries = n_distinct(mfr_country)
  ) %>%
  pivot_longer(-source, names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, levels = c("ATC codes", "Manufacturers", "Countries")))

p_totals <- ggplot(totals, aes(x = metric, y = value, fill = source)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = value), position = position_dodge(width = 0.75), vjust = -0.3, size = 3) +
  scale_y_log10(labels = label_number(accuracy = 1, big.mark = ",")) +
  scale_fill_manual(values = source_pal, name = NULL) +
  labs(x = NULL, y = "Count (log scale)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

p_manu <- ggplot(atc_summary, aes(x = n_manufacturers, fill = source)) +
  geom_histogram(binwidth = 2, boundary = 0, color = "white") +
  facet_wrap(~ source, scales = "free", nrow = 1, drop = FALSE) +
  scale_fill_manual(values = source_pal, guide = "none") +
  scale_x_continuous(breaks = integer_breaks(), labels = label_number(accuracy = 1)) +
  scale_y_continuous(breaks = integer_breaks(), labels = label_number(accuracy = 1)) +
  labs(x = "Distinct manufacturers per ATC code", y = "Number of ATC codes") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

p_ctry <- ggplot(atc_summary, aes(x = n_countries, fill = source)) +
  geom_histogram(binwidth = 1, color = "white") +
  facet_wrap(~ source, scales = "free", nrow = 1, drop = FALSE) +
  scale_fill_manual(values = source_pal, guide = "none") +
  scale_x_continuous(breaks = integer_breaks(), labels = label_number(accuracy = 1)) +
  scale_y_continuous(breaks = integer_breaks(), labels = label_number(accuracy = 1)) +
  labs(x = "Distinct countries per ATC code", y = "Number of ATC codes") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

top_countries <- data %>%
  distinct(atc_code, mfr_company, mfr_country, source) %>%
  count(source, mfr_country, name = "n_records") %>%
  group_by(source) %>%
  slice_max(n_records, n = 10) %>%
  ungroup()

p_top_countries <- ggplot(top_countries, aes(x = reorder_within(mfr_country, n_records, source),
                                             y = n_records, fill = source)) +
  geom_col() +
  coord_flip() +
  tidytext::scale_x_reordered() +
  facet_wrap(~ source, scales = "free", nrow = 1, drop = FALSE) +
  scale_fill_manual(values = source_pal, guide = "none") +
  scale_y_continuous(breaks = integer_breaks(), labels = label_number(accuracy = 1)) +
  labs(x = NULL, y = "Manufacturing records") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

report_plot <- p_totals / p_manu / p_ctry / p_top_countries +
  plot_layout(heights = c(1, 1, 1, 1.2))

ggsave(file.path(FIG_DIR, "plots_by_source.png"), report_plot, width = 15, height = 20, dpi = 300)
print(report_plot)


library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)

germany_raw <- read.csv(file.path(PROC_DIR, "germany_who_LONG.csv"), stringsAsFactors = FALSE) %>%
  filter(role != "Zulassungsinhaber")

EPAR    <- read.csv(file.path(PROC_DIR, "ema_who.csv"), stringsAsFactors = FALSE)
ireland <- read.csv(file.path(PROC_DIR, "ireland_who.csv"), stringsAsFactors = FALSE)
cep     <- read.csv(file.path(RAW_DIR, "EXPORT_WEB_CEP_with_ATC_drugbank.csv"), stringsAsFactors = FALSE)
names(cep) <- janitor::make_clean_names(names(cep))

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

critical_codes <- critical_codes_who

cep_clean <- cep %>%
  filter(status_cep == "Valid", !is.na(atc_code), atc_code != "") %>%
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

hhi_all <- bind_rows(hhi_epar, hhi_germany, hhi_ireland, hhi_cep) %>%
  mutate(source = factor(source, levels = c("EPAR", "Germany", "Ireland", "CEP")))

write.csv(hhi_all, file.path(TAB_DIR, "hhi_step1_priority.csv"), row.names = FALSE)

winner_summary <- hhi_all %>%
  group_by(atc_code) %>%
  filter(hhi == max(hhi)) %>%
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

write.csv(winner_pct, file.path(TAB_DIR, "hhi_highest_by_source.csv"), row.names = FALSE)

library(xtable)
print(xtable(winner_pct, caption = "Number of critical ATC codes for which each source reports the highest country-level HHI, out of the codes that source covers.", label = "tab:hhi_highest_by_sourceT"), file = file.path(TAB_DIR, "hhi_highest_by_source.tex"))

library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)

hhi_all <- read.csv(file.path(TAB_DIR, "hhi_step1_priority.csv"), stringsAsFactors = FALSE)

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

chapter_order <- hhi_all %>%
  group_by(chapter) %>%
  summarise(med = median(hhi)) %>%
  arrange(desc(med)) %>%
  pull(chapter)

hhi_all$chapter <- factor(hhi_all$chapter, levels = rev(chapter_order))

chapter_pal <- setNames(
  colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(length(chapter_order)),
  chapter_order
)

axis_label_colors <- chapter_pal[rev(chapter_order)]

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

ggsave(file.path(FIG_DIR, "hhi_by_chapter_by_source.png"), p, width = 17, height = 8, dpi = 300)
print(p)

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
  ggsave(file.path(ADD_DIR, paste0("hhi_by_chapter_", src, ".png")), p_single, width = 8, height = 8, dpi = 300)
}

library(dplyr)
library(ggplot2)

hhi_all <- read.csv(file.path(TAB_DIR, "hhi_step1_priority.csv"), stringsAsFactors = FALSE) %>%
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

ggsave(file.path(FIG_DIR, "hhi_step1_priority_by_source.png"), p, width = 12, height = 5, dpi = 300)
print(p)




library(dplyr)
library(tidyr)
library(patchwork)
library(stringr)
library(readr)
library(purrr)

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

germany <- read.csv(file.path(PROC_DIR, "germany_who_LONG.csv"))
str(germany)
germany <- germany %>%
  filter(role != "Zulassungsinhaber") %>%
  mutate(land = recode(str_trim(land), !!!country_map))

write.csv(germany, file.path(PROC_DIR, "germany_critical.csv"), row.names = F)

EPAR <- read.csv(file.path(PROC_DIR, "ema_who.csv")) %>%
  mutate(country = recode(str_trim(country), !!!country_map))

ireland <- read.csv(file.path(PROC_DIR, "ireland_who.csv"))
str(ireland)

ireland <- ireland %>%
  filter(!is.na(matched_critical_atc), matched_critical_atc != "") %>%
  mutate(atc_split = str_split(matched_critical_atc, ",\\s*")) %>%
  unnest(atc_split) %>%
  mutate(atc_code = str_trim(atc_split)) %>%
  select(-atc_split)

critical_codes <- critical_codes_who

split_trim <- function(x) {
  if (is.na(x) || x == "") return(character(0))
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

if (!exists("cep_atc")) {
  cep_atc <- read_csv(file.path(RAW_DIR, "EXPORT_WEB_CEP_with_ATC_drugbank.csv"), show_col_types = FALSE)
}

cep_atc <- janitor::clean_names(cep_atc)

if (!"holder_country" %in% names(cep_atc)) {
  cep_atc <- cep_atc %>%
    mutate(
      holder_country          = str_extract(certificate_cep_holder, "(?<=\\s)[A-Z]{2}$"),
      certificate_cep_holder  = str_trim(str_remove(certificate_cep_holder, "\\s[A-Z]{2}$"))
    )
}

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

cep_full <- cep_atc %>%
  filter(status_cep == "Valid", !is.na(atc_code), atc_code != "") %>%
  separate_rows(atc_code, sep = ",\\s*") %>%
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

germany_full <- germany
EPAR_full    <- EPAR
ireland_full <- ireland

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

per_code <- bind_rows(
  germany_full %>% transmute(source = "Germany", atc_code, mfr_company = str_trim(name), mfr_country = str_trim(land)),
  EPAR_full %>% transmute(source = "EPAR", atc_code, mfr_company = str_trim(manufacturer_name), mfr_country = str_trim(country)),
  ireland_full %>% transmute(source = "Ireland", atc_code, mfr_company = str_trim(mfr_company), mfr_country = str_trim(mfr_country)),
  cep_full %>% transmute(source = "CEP", atc_code, mfr_company = manufacturer, mfr_country = country)
) %>%
  distinct(source, atc_code, mfr_company, mfr_country) %>%
  group_by(source, atc_code) %>%
  summarise(
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

report_plot <- panelA / panelB + plot_layout(heights = c(1, 1.6))

ggsave(file.path(ADD_DIR, "atc_summary_report_plot.png"), report_plot, width = 15, height = 9, dpi = 300)
print(report_plot)

ggsave(file.path(FIG_DIR, "atc_summary_report_plot_v2.png"), panelB, width = 16, height = 5, dpi = 300,
       bg = "white")




library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(patchwork)
library(scales)

data <- read.csv(file.path(PROC_DIR, "manufacturer_registers_combined.csv"), stringsAsFactors = FALSE)
nrow(data)

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

## split "A | B" lists only where company and country lists have the same length
## (separate_rows() on both columns errors on the Irish rows where they differ)
n_pipe <- function(x) ifelse(is.na(x) | x == "", 0L, str_count(x, "\\|") + 1L)
same_len <- n_pipe(data$mfr_company) == n_pipe(data$mfr_country)
data <- bind_rows(
  data[same_len, ] %>% separate_rows(mfr_company, mfr_country, sep = "\\s*\\|\\s*"),
  data[!same_len, ]
) %>%
  mutate(
    mfr_company = str_trim(mfr_company),
    mfr_country = recode(str_trim(mfr_country), !!!country_map),
    source = str_to_title(str_trim(source)),
    source = recode(source, "Ema" = "EPAR", "Cep" = "CEP")
  ) %>%
  filter(!is.na(atc_code), atc_code != "",
         !is.na(mfr_company), mfr_company != "",
         !is.na(mfr_country), mfr_country != "")

atc_group_names <- c(
  A = "Alimentary tract\n& metabolism", B = "Blood & blood\nforming organs",
  C = "Cardiovascular\nsystem", D = "Dermatologicals",
  G = "Genito urinary\n& sex hormones", H = "Systemic hormonal\npreparations",
  J = "Antiinfectives", L = "Antineoplastic &\nimmunomodulating",
  M = "Musculo-skeletal\nsystem", N = "Nervous system",
  P = "Antiparasitic\nproducts", R = "Respiratory\nsystem",
  S = "Sensory organs", V = "Various"
)

data <- data %>%
  mutate(atc_group = substr(atc_code, 1, 1),
         atc_group_label = recode(atc_group, !!!atc_group_names))

EU_EEA <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark",
            "Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy",
            "Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal",
            "Romania","Slovakia","Slovenia","Spain","Sweden","Iceland","Norway","Liechtenstein")

matrix_data <- data %>%
  distinct(source, atc_code, mfr_company, mfr_country, atc_group_label) %>%
  group_by(source, mfr_country, atc_group_label) %>%
  summarise(
    unique_sites = n_distinct(mfr_company),
    unique_atc   = n_distinct(atc_code),
    .groups = "drop"
  )

country_order <- matrix_data %>%
  group_by(mfr_country) %>%
  summarise(total_sites = sum(unique_sites)) %>%
  arrange(desc(total_sites)) %>%
  pull(mfr_country)

matrix_data <- matrix_data %>%
  mutate(
    mfr_country_label = factor(mfr_country, levels = rev(country_order)),
    source = factor(source, levels = c("EPAR", "Germany", "Ireland", "CEP"))
  )

group_order <- atc_group_names[c("A","B","C","D","G","H","J","L","M","N","P","R","S","V")]
matrix_data$atc_group_label <- factor(matrix_data$atc_group_label, levels = group_order)

country_labels <- function(x) {
  parse(text = ifelse(x %in% EU_EEA, sprintf("'%s'", x), sprintf("bold('%s')", x)))
}

source_pal <- c(EPAR = "#1D6F5C", Germany = "#B9861A", Ireland = "#C1461D", CEP = "#1A4D7A")

make_bubble_plot <- function(df, source_name, base_color, show_y_labels = TRUE) {
  
  sub <- df %>% filter(source == source_name)
  
  p <- ggplot(sub, aes(x = atc_group_label, y = mfr_country_label)) +
    geom_point(aes(size = unique_sites, fill = unique_atc),
               shape = 21, color = base_color, stroke = 0.8, alpha = 1) +
    scale_size_continuous(name = "Unique sites", range = c(1, 10),
                          breaks = function(x) unique(round(scales::extended_breaks()(x))),
                          labels = scales::label_number(accuracy = 1)) +
    scale_fill_gradient(low = "#F0EDE3", high = base_color, name = "Unique ATC\ncodes",
                        breaks = function(x) unique(round(scales::extended_breaks()(x))),
                        labels = scales::label_number(accuracy = 1)) +
    scale_y_discrete(drop = FALSE, labels = country_labels,
                     expand = expansion(add = 1.2)) +
    guides(size = guide_legend(override.aes = list(fill = "grey50", color = "grey50"))) +
    labs(x = NULL, y = NULL, title = source_name) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 40, hjust = 1, size = 11),
      axis.text.y = if (show_y_labels) element_text(size = 12) else element_blank(),
      panel.grid.major = element_line(color = "grey92"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      legend.position = "top",
      legend.box = "horizontal",
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      legend.key.size = unit(0.7, "cm"),
      legend.margin = margin(b = -8, t = 0, l = 0, r = 0),
      legend.box.margin = margin(b = -8),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
    )
  
  p
}

p_ema     <- make_bubble_plot(matrix_data, "EPAR",    source_pal["EPAR"],    show_y_labels = TRUE)
p_germany <- make_bubble_plot(matrix_data, "Germany", source_pal["Germany"], show_y_labels = TRUE)
p_ireland <- make_bubble_plot(matrix_data, "Ireland", source_pal["Ireland"], show_y_labels = TRUE)
p_cep     <- make_bubble_plot(matrix_data, "CEP",     source_pal["CEP"],     show_y_labels = TRUE)

ggsave(file.path(ADD_DIR, "bubble_matrix_EPAR.png"),     p_ema,     width = 8,  height = 11, dpi = 300)
ggsave(file.path(ADD_DIR, "bubble_matrix_Germany.png"), p_germany, width = 8,  height = 11, dpi = 300)
ggsave(file.path(ADD_DIR, "bubble_matrix_Ireland.png"), p_ireland, width = 8,  height = 11, dpi = 300)
ggsave(file.path(ADD_DIR, "bubble_matrix_CEP.png"),     p_cep,     width = 8,  height = 11, dpi = 300)

p <- (p_ema | p_germany) / (p_ireland | p_cep)

ggsave(file.path(FIG_DIR, "bubble_matrix_by_source.png"), p, width = 18, height = 20, dpi = 300)
print(p)



library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(patchwork)
library(readr)
library(janitor)
library(purrr)

germany   <- read.csv(file.path(PROC_DIR, "germany_who_LONG.csv"), stringsAsFactors = FALSE)
EPAR    <- read.csv(file.path(PROC_DIR, "ema_who.csv"), stringsAsFactors = FALSE)
ireland <- read.csv(file.path(PROC_DIR, "ireland_who.csv"), stringsAsFactors = FALSE)

cep_atc <- read_csv(file.path(RAW_DIR, "EXPORT_WEB_CEP_with_ATC_drugbank.csv"), show_col_types = FALSE) |>
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
)

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

critical_codes <- critical_codes_who

cep_atc2 <- cep_atc[!is.na(cep_atc$atc_code), ]
cep_atc <- cep_atc %>% filter(status_cep == "Valid")   # split multi-code cells before filtering

cep_atc <- cep_atc %>%
  separate_rows(atc_code, sep = ",\\s*") %>%
  filter(atc_code %in% critical_codes)

cep_atc <- cep_atc %>%
  mutate(holder_country = str_extract(certificate_cep_holder, "[A-Z]{2}$"))

str(unique(cep_atc$substance))

str(unique(cep_atc$atc_code))

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

germany_classified <- classify_sites(germany_sites, "Germany")

epar_sites <- EPAR %>%
  mutate(
    country = recode(str_remove(str_trim(country), "\\.$"), !!!country_map),
    step = coalesce(as.numeric(manufacturer_step), 2),
    chapter = recode(substr(atc_code, 1, 1), !!!atc_chapter_names)
  ) %>%
  filter(!is.na(atc_code), atc_code != "", !is.na(manufacturer_name), manufacturer_name != "",
         !is.na(country), country != "") %>%
  transmute(chapter, site_id = str_trim(manufacturer_name), country, step)

epar_classified <- classify_sites(epar_sites, "EPAR")

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

all_classified <- bind_rows(germany_classified, epar_classified, ire_classified, cep_classified)

chapter_summary <- all_classified %>%
  count(source, chapter, category) %>%
  group_by(source, chapter) %>%
  mutate(total = sum(n), pct = 100 * n / total) %>%
  ungroup()

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
  "EEA - API (step 1)"         = "#0B2545",
  "EEA - batch release"        = "#4A7FB5",
  "EEA - not disclosed"        = "#8FB3D6",
  "Non-EEA - API (step 1)"     = "#7A1810",
  "Non-EEA - batch release"    = "#C1461D",
  "Non-EEA - not disclosed"    = "#E28A6E"
)

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

ggsave(file.path(FIG_DIR, "manufacturing_sites_by_chapter_step_source.png"), p, width = 25, height = 9.5, dpi = 300)
print(p)




library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(patchwork)
library(readr)
library(janitor)
library(purrr)

germany   <- read.csv(file.path(PROC_DIR, "germany_who_LONG.csv"), stringsAsFactors = FALSE)
EPAR    <- read.csv(file.path(PROC_DIR, "ema_who.csv"), stringsAsFactors = FALSE)
ireland <- read.csv(file.path(PROC_DIR, "ireland_who.csv"), stringsAsFactors = FALSE)

cep_atc <- read_csv(file.path(RAW_DIR, "EXPORT_WEB_CEP_with_ATC_drugbank.csv"), show_col_types = FALSE) |>
  clean_names()

atc_chapter_names <- c(
  A = "Alimentary tract & metabolism",
  B = "Blood & blood forming organs",
  C = "Cardiovascular system",
  D = "Dermatologicals",
  G = "Genito urinary system & sex hormones",
  H = "Systemic hormonal preparations",
  J = "Antiinfectives (systemic)",
  L = "Antineoplastic & immunomodulating",
  M = "Musculo-skeletal system",
  N = "Nervous system",
  P = "Antiparasitic products",
  R = "Respiratory system",
  S = "Sensory organs",
  V = "Various"
)

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

country_map <- c(
  "The Netherlands"    = "Netherlands",
  "UK"                 = "United Kingdom",
  "USA"                = "United States",
  "Republic of Korea"  = "South Korea",
  "Korea, Republic of" = "South Korea"
)

EEA <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
  "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
  "Spain", "Sweden", "Iceland", "Liechtenstein", "Norway"
)

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
)

critical_codes <- critical_codes_who

cep_atc2 <- cep_atc[!is.na(cep_atc$atc_code), ]
cep_atc <- cep_atc %>% filter(status_cep == "Valid")   # split multi-code cells before filtering

cep_atc <- cep_atc %>%
  separate_rows(atc_code, sep = ",\\s*") %>%
  filter(atc_code %in% critical_codes)

cep_atc <- cep_atc %>%
  mutate(holder_country = str_extract(certificate_cep_holder, "[A-Z]{2}$"))

str(unique(cep_atc$substance))

str(unique(cep_atc$atc_code))

classify_sites <- function(df, source_name) {
  df %>%
    group_by(chapter, site_id) %>%
    summarise(
      country = first(country),
      has_step1 = any(step == 1, na.rm = TRUE),
      has_step2 = any(step == 2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      country_group = case_when(
        country == "China" ~ "China",
        country == "India" ~ "India",
        country %in% EEA   ~ "EU/EEA",
        TRUE ~ "Other"
      ),
      step_label = ifelse(has_step1, "API (step 1)", "batch release / not disclosed"),
      category = paste(country_group, "-", step_label),
      source = source_name
    )
}

germany_sites <- germany %>%
  mutate(
    country = recode(str_remove(str_trim(land), "\\.$"), !!!de_to_en_country) %>%
      recode(!!!country_map),
    step = case_when(role == "Wirkstoffherstellung" ~ 1,
                     role == "Hersteller/Endfreigabe" ~ 2,
                     TRUE ~ NA_real_),
    chapter = recode(substr(atc_code, 1, 1), !!!atc_chapter_names)
  ) %>%
  filter(!is.na(atc_code), atc_code != "", !is.na(pu_nummer), !is.na(country), country != "") %>%
  transmute(chapter, site_id = as.character(pu_nummer), country, step)

germany_classified <- classify_sites(germany_sites, "Germany")

epar_sites <- EPAR %>%
  mutate(
    country = recode(str_remove(str_trim(country), "\\.$"), !!!country_map),
    step = coalesce(as.numeric(manufacturer_step), 2),
    chapter = recode(substr(atc_code, 1, 1), !!!atc_chapter_names)
  ) %>%
  filter(!is.na(atc_code), atc_code != "", !is.na(manufacturer_name), manufacturer_name != "",
         !is.na(country), country != "") %>%
  transmute(chapter, site_id = str_trim(manufacturer_name), country, step)

epar_classified <- classify_sites(epar_sites, "EPAR")

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

all_classified <- bind_rows(germany_classified, epar_classified, ire_classified, cep_classified)

chapter_summary <- all_classified %>%
  count(source, chapter, category) %>%
  group_by(source, chapter) %>%
  mutate(total = sum(n), pct = 100 * n / total) %>%
  ungroup()

germany_all_roles_order <- germany %>%
  mutate(chapter = recode(substr(atc_code, 1, 1), !!!atc_chapter_names)) %>%
  filter(!is.na(atc_code), atc_code != "", !is.na(pu_nummer)) %>%
  distinct(chapter, pu_nummer) %>%
  count(chapter, name = "total") %>%
  arrange(desc(total)) %>%
  pull(chapter)

chapter_order <- germany_all_roles_order

chapter_summary <- chapter_summary %>%
  mutate(chapter = factor(chapter, levels = rev(chapter_order)),
         source = factor(source, levels = c("EPAR", "Germany", "Ireland", "CEP")),
         category = factor(category, levels = c(
           "China - API (step 1)",  "China - batch release / not disclosed",
           "India - API (step 1)",  "India - batch release / not disclosed",
           "EU/EEA - API (step 1)", "EU/EEA - batch release / not disclosed",
           "Other - API (step 1)",  "Other - batch release / not disclosed"
         )))

group_colors <- c(
  "China - API (step 1)"                   = "#ED7014",
  "China - batch release / not disclosed"  = "#E8951A",
  "India - API (step 1)"                   = "#B36A1E",
  "India - batch release / not disclosed"  = "#CC9350",
  "EU/EEA - API (step 1)"                  = "#0B2545",
  "EU/EEA - batch release / not disclosed" = "#4A7FB5",
  "Other - API (step 1)"                   = "#7A1810",
  "Other - batch release / not disclosed"  = "#C1461D"
)

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
            inherit.aes = FALSE, color = "black", size = 4, fontface = "bold") +
  scale_fill_manual(values = group_colors, name = NULL, guide = guide_legend(nrow = 2)) +
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

ggsave(file.path(FIG_DIR, "manufacturing_sites_by_chapter_country_group_source.png"), p, width = 25, height = 9.5, dpi = 300)
print(p)



library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(ggrepel)
library(purrr)

germany <- read.csv(file.path(PROC_DIR, "germany_who_LONG.csv"), stringsAsFactors = FALSE) %>%
  filter(role != "Zulassungsinhaber")
EPAR    <- read.csv(file.path(PROC_DIR, "ema_who.csv"), stringsAsFactors = FALSE)
ireland <- read.csv(file.path(PROC_DIR, "ireland_who.csv"), stringsAsFactors = FALSE)

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

split_squish <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  str_squish(str_split(x, "\\|")[[1]])
}

make_diversification_plot <- function(df, source_name, highlight_threshold = 65,
                                      min_sites_to_label = 1, base_color = "#7FA8C9") {
  
  summary_df <- df %>%
    distinct(substance, site_id, country) %>%
    group_by(substance) %>%
    summarise(
      n_sites = n_distinct(site_id),
      pct_china_india = 100 * mean(country %in% c("China", "India")),
      .groups = "drop"
    ) %>%
    filter(n_sites >= min_sites_to_label)
  
  summary_df <- summary_df %>%
    mutate(
      highlight = pct_china_india > highlight_threshold & n_sites <= 30,
      substance_label = str_trunc(substance, 25, ellipsis = "...")
    )
  
  ggplot(summary_df, aes(x = n_sites, y = pct_china_india)) +
    geom_hline(yintercept = highlight_threshold, linetype = "dotted", color = "grey60") +
    geom_point(aes(color = highlight), size = 2.2, alpha = 0.8) +
    geom_text_repel(
      data = filter(summary_df, highlight),
      aes(label = substance_label),
      size = 4.5, max.overlaps = 30, segment.size = 0.3, segment.color = "grey50",
      min.segment.length = 0, seed = 1
    ) +
    scale_color_manual(values = c(`TRUE` = "#B33018", `FALSE` = base_color), guide = "none") +
    labs(
      x = "Number of distinct API-producer sites  (\u2192 more diversified)",
      y = "Share of sites in China + India (%)  (\u2191 more dependent)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank()
    )
}

germany_clean <- germany %>%
  mutate(country = recode(str_trim(land), !!!country_map)) %>%
  filter(!is.na(atc_code), atc_code != "", !is.na(pu_nummer), !is.na(country), country != "")

atc_has_step1_ger <- germany_clean %>%
  filter(role == "Wirkstoffherstellung") %>%
  distinct(atc_code) %>% pull(atc_code)

germany_priority <- germany_clean %>%
  filter(
    (atc_code %in% atc_has_step1_ger  & role == "Wirkstoffherstellung") |
      (!atc_code %in% atc_has_step1_ger & role == "Hersteller/Endfreigabe")
  ) %>%
  mutate(
    substance = str_trim(atc_text),
    substance = ifelse(
      str_detect(substance, "^[A-Z][0-9]{2}[A-Z]{2}[0-9]{2}:"),
      sapply(str_split(substance, "\\s*-\\s*"), function(x) tail(x, 1)),
      substance
    )
  ) %>%
  transmute(substance, site_id = str_squish(as.character(pu_nummer)), country)

p_germany <- make_diversification_plot(germany_priority, "Germany")

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
  mutate(substance = ifelse(is.na(active_substance) | active_substance == "",
                            medicine_name, str_trim(active_substance))) %>%
  transmute(substance, site_id = str_squish(manufacturer_name), country)

p_epar <- make_diversification_plot(epar_priority, "EPAR")

ireland_pairs <- ireland %>%
  filter(!is.na(atc_code), atc_code != "") %>%
  mutate(
    company_list = map(mfr_company, split_squish),
    country_list = map(mfr_country, split_squish),
    lengths_match = map2_lgl(company_list, country_list,
                             ~ length(.x) == length(.y) && length(.x) > 0)
  )

ireland_matched <- ireland_pairs %>%
  filter(lengths_match) %>%
  select(product_name, company_list, country_list) %>%
  unnest(cols = c(company_list, country_list)) %>%
  rename(mfr_company = company_list, mfr_country = country_list)

ireland_fallback <- ireland_pairs %>%
  filter(!lengths_match) %>%
  transmute(
    product_name,
    mfr_company = str_squish(mfr_company),
    mfr_country = str_squish(mfr_country)
  )

n_fallback <- nrow(ireland_fallback)
if (n_fallback > 0) {
  cat("Ireland rows kept unsplit (company/country list length mismatch):", n_fallback, "\n")
}

ireland_std <- bind_rows(ireland_matched, ireland_fallback) %>%
  mutate(country = recode(mfr_country, !!!country_map)) %>%
  filter(!is.na(mfr_company), mfr_company != "",
         !is.na(country), country != "") %>%
  transmute(substance = str_squish(product_name), site_id = mfr_company, country)

p_ireland <- make_diversification_plot(ireland_std, "Ireland")

ggsave(file.path(FIG_DIR, "api_diversification_Germany.png"), p_germany, width = 12, height = 7, dpi = 300)
ggsave(file.path(ADD_DIR, "api_diversification_Germany.pdf"), p_germany, width = 12, height = 7)

ggsave(file.path(ADD_DIR, "api_diversification_EPAR.png"),    p_epar,    width = 12, height = 7, dpi = 300)
ggsave(file.path(ADD_DIR, "api_diversification_Ireland.png"), p_ireland, width = 12, height = 7, dpi = 300)

print(p_germany)
print(p_epar)
print(p_ireland)

clean_names_simple <- function(df) {
  nm <- names(df)
  nm <- str_replace_all(nm, c("ä" = "ae", "ö" = "oe", "ü" = "ue",
                              "Ä" = "ae", "Ö" = "oe", "Ü" = "ue", "ß" = "ss"))
  nm <- str_to_lower(nm)
  nm <- str_replace_all(nm, "[^a-z0-9]+", "_")
  nm <- str_remove_all(nm, "^_+|_+$")
  names(df) <- make.unique(nm, sep = "_")
  df
}

library(readr)
library(lubridate)   # dmy(), as_date()
library(broom)       # tidy()
## car and pROC are used via car:: / pROC::, so they only need to be installed

## -----------------------------------------------------------------
## 0. Paths -- same folders as the rest of this script (paths.R)
## -----------------------------------------------------------------
path_mfr       <- file.path(PROC_DIR, "manufacturer_registers_combined.csv")
path_shortages <- file.path(RAW_DIR, "LEMeldungen_2026-09-09-13-30-43.csv")
path_gmp_api   <- file.path(RAW_DIR, "gmdp_EML_API_sites_activities_LONG.csv")

## -----------------------------------------------------------------
## 1. Load the manufacturer register (already WHO-EML-filtered)
## -----------------------------------------------------------------
mfr <- read_csv(path_mfr, show_col_types = FALSE) |>
  mutate(
    mfr_company = str_squish(mfr_company),
    mfr_country = str_squish(mfr_country)
  )

cat("Manufacturer register rows:", nrow(mfr), "\n")
cat("Unique critical ATC codes covered by ANY source:", n_distinct(mfr$atc_code), "\n")
print(table(mfr$source))

## -----------------------------------------------------------------
## 1b. Load the EudraGMDP API-site file (for V3)
##    - long format: one row per site x certificate x activity, so it
##      is collapsed to one row per ATC code x site below
##    - only sites that actually MAKE or PROCESS the substance are kept
##      (EU GMP API-certificate sections 3.1-3.4 = chemical synthesis,
##      extraction, biological/fermentation, sterile API; 3.5.1 =
##      physical processing, e.g. micronisation). Sites certified ONLY
##      for packaging (3.5.2-3.5.4) or testing (3.6.x) are dropped, as
##      they are contract service sites rather than manufacturers.
##      Set gmp_production_only <- FALSE to keep every certified site.
##    - the file's is_critical column is the old EU-list flag, so it is
##      NOT used; rows are filtered to the WHO EML codes present in the
##      combined register instead. Multi-code ATC cells
##      ("N07BC01;N02AE01") are split into one row per code.
##    - every row is an active-substance site, so it is tagged
##      manufacturer_step = 1 (see step-1 note in section 3)
##    - two country spellings differ from the Germany/CEP registers and
##      are harmonised so the China/India share and HHI line up
## -----------------------------------------------------------------
gmp_production_only <- TRUE
gmp_production_codes <- "^3\\.(1|2|3|4)\\.|^3\\.5\\.1$"

gmp_raw <- read_csv(path_gmp_api, show_col_types = FALSE)

## Critical codes = full WHO EML list (not unique(mfr$atc_code): GMDP is no
## longer in the combined file, so codes covered only by GMDP must still count).
## (is_critical in the GMDP file is the old EU-list flag, so it is not used)
critical_codes <- critical_codes_who

gmp_api <- gmp_raw |>
  filter(!withdrawn, cert_type == "API", !is.na(atc), atc != "") |>
  { \(d) if (gmp_production_only)
    filter(d, str_detect(activity_code, gmp_production_codes)) else d }() |>
  separate_rows(atc, sep = "\\s*;\\s*") |>            # "N07BC01;N02AE01" -> 2 rows
  transmute(
    atc_code          = str_squish(atc),
    mfr_company       = str_squish(site_name),
    mfr_country       = str_squish(country),
    manufacturer_step = 1,
    source            = "gmp_api"
  ) |>
  filter(atc_code %in% critical_codes) |>
  mutate(mfr_country = dplyr::recode(mfr_country,   # car::recode masks dplyr's
                                     "Czechia"                           = "Czech Republic",
                                     "Korea, Republic of"                = "South Korea",
                                     "United Kingdom (Northern Ireland)" = "United Kingdom")) |>
  distinct()

cat("\nEudraGMDP API file rows:", nrow(gmp_raw),
    "-> unique ATC x site rows kept for V3:", nrow(gmp_api),
    if (gmp_production_only) "(production/processing sites only)" else "(all sites)", "\n")
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
## 3. Manufacturer-concentration metrics per ATC code
##    Computed at three levels:
##      - EPAR-only  (source == "ema")            -> used only for the
##                                                    "25 single-source"
##                                                    reproduction check
##                                                    in section 4
##      - V1: Germany-only (source == "germany")  -> main analysis input
##      - V2: Germany + EPAR combined/deduplicated -> main analysis input
##      - V3: Germany + CEP + EudraGMDP API sites  -> main analysis input
##    Ireland is not used anywhere; CEP is used only in V3.
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

## Company-name harmonisation, used for V3 only. The three V3 registers
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
    group_by(atc_code) |>
    summarise(
      n_manufacturers = n_distinct(mfr_company, na.rm = TRUE),
      n_countries     = n_distinct(mfr_country, na.rm = TRUE),
      n_china_india   = sum(mfr_country %in% china_india, na.rm = TRUE),
      pct_china_india = n_china_india / n(),
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
metrics_v1 <- compute_metrics(filter(mfr, source == "germany"), step1_priority = TRUE) |>
  rename_with(~paste0(.x, "_v1"), -atc_code)

## V2: Germany + EPAR combined (step-1 priority applied within this
## Germany+EPAR subset, so a code with a step-1 EPAR or Germany
## manufacturer uses only step-1 rows; falls back to step-2/NA
## otherwise)
metrics_v2 <- compute_metrics(filter(mfr, source %in% c("germany", "ema")), step1_priority = TRUE) |>
  rename_with(~paste0(.x, "_v2"), -atc_code)

## V3: Germany + CEP + EudraGMDP API sites.
## CEP and GMP API rows are active-substance manufacturers by
## definition, but CEP has no step field (NA). Left as NA, the step-1
## filter would DROP every CEP row for any code where Germany lists a
## step-1 site -- so CEP is tagged step 1 here (GMP already is). Net
## effect: per code, V3 = Germany step-1 sites + all CEP holders + all
## GMP API sites (or Germany step-2/NA rows + CEP + GMP if Germany has
## no step-1 site for that code). Company names are harmonised across
## the three registers (see normalize_company()).
v3_input <- bind_rows(
  mfr |>
    filter(source %in% c("germany", "cep")) |>
    mutate(manufacturer_step = if_else(source == "cep", 1, manufacturer_step)),
  gmp_api
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
## record anywhere (combined register OR GMP API file), so codes with zero
## disclosed EPAR/DE manufacturers still show up (with NA metrics for that
## source) rather than being silently dropped.
all_codes <- tibble(atc_code = union(unique(mfr$atc_code), unique(gmp_api$atc_code)))

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

cat("\nAnalysis table:", nrow(analysis), "critical ATC codes.\n")

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
## denser coverage (~446 of 604 WHO EML codes) than EPAR (~141 codes).
cat("\n--- Single-source (V1: Germany only) vs. currently-active shortage ---\n")
tab_single_source_v1 <- analysis |>
  filter(!is.na(single_source_v1)) |>
  count(single_source_v1, currently_active) |>
  pivot_wider(names_from = currently_active, values_from = n, values_fill = 0)
print(tab_single_source_v1)

ft_v1 <- analysis |>
  filter(!is.na(single_source_v1)) |>
  { \(d) fisher.test(table(d$single_source_v1, d$currently_active)) }()
print(ft_v1)

## Same check on V3 (Germany + CEP + GMP API sites)
cat("\n--- Single-source (V3: Germany + CEP + GMP API) vs. currently-active shortage ---\n")
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
  univariate_test("n_manufacturers_v1", "N manufacturers (V1: Germany)"),
  univariate_test("n_countries_v1",     "N countries (V1: Germany)"),
  univariate_test("pct_china_india_v1", "% China+India (V1: Germany)"),
  univariate_test("hhi_v1",             "Country HHI (V1: Germany)"),
  univariate_test("n_manufacturers_v2", "N manufacturers (V2: Germany+EPAR)"),
  univariate_test("n_countries_v2",     "N countries (V2: Germany+EPAR)"),
  univariate_test("pct_china_india_v2", "% China+India (V2: Germany+EPAR)"),
  univariate_test("hhi_v2",             "Country HHI (V2: Germany+EPAR)"),
  univariate_test("n_manufacturers_v3", "N manufacturers (V3: Germany+CEP+GMP)"),
  univariate_test("n_countries_v3",     "N countries (V3: Germany+CEP+GMP)"),
  univariate_test("pct_china_india_v3", "% China+India (V3: Germany+CEP+GMP)"),
  univariate_test("hhi_v3",             "Country HHI (V3: Germany+CEP+GMP)")
)

cat("\n--- Univariate association with currently-active shortage ---\n")
print(univariate_results, n = Inf)

## -----------------------------------------------------------------
## 6. Multivariate logistic regression
##    shortage ~ manufacturer count + concentration + China/India share
##    Log-transform manufacturer count (right-skewed); scale HHI to
##    0-1 for a readable coefficient. Fit separately on V1 and V2.
## -----------------------------------------------------------------
model_data_v1 <- analysis |>
  filter(!is.na(n_manufacturers_v1)) |>
  mutate(
    log_n_manufacturers = log1p(n_manufacturers_v1),
    hhi_scaled          = hhi_v1 / 10000,
    pct_china_india      = pct_china_india_v1
  )

fit_v1 <- glm(
  currently_active ~ log_n_manufacturers + hhi_scaled + pct_china_india,
  data   = model_data_v1,
  family = binomial()
)

cat("\n--- Logistic regression: shortage ~ manufacturer concentration (V1: Germany only) ---\n")
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

fit_v2 <- glm(
  currently_active ~ log_n_manufacturers + hhi_scaled + pct_china_india,
  data   = model_data_v2,
  family = binomial()
)

cat("\n--- Logistic regression: shortage ~ manufacturer concentration (V2: Germany + EPAR) ---\n")
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

fit_v3 <- glm(
  currently_active ~ log_n_manufacturers + hhi_scaled + pct_china_india,
  data   = model_data_v3,
  family = binomial()
)

cat("\n--- Logistic regression: shortage ~ manufacturer concentration (V3: Germany + CEP + GMP API) ---\n")
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
  print(car::vif(fit))
  
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

model_diagnostics(fit_v1, model_data_v1$currently_active, "V1: Germany only")
model_diagnostics(fit_v2, model_data_v2$currently_active, "V2: Germany + EPAR")
model_diagnostics(fit_v3, model_data_v3$currently_active, "V3: Germany + CEP + GMP API")

## -----------------------------------------------------------------
## 7. Plots (V1: Germany only, as the primary/default view)
## -----------------------------------------------------------------
theme_set(theme_minimal(base_size = 12))

p1 <- analysis |>
  filter(!is.na(n_manufacturers_v1)) |>
  ggplot(aes(x = currently_active, y = n_manufacturers_v1)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_log10() +
  labs(x = "Currently on shortage list", y = "Distinct manufacturers (V1: Germany, log scale)",
       title = "Manufacturer count vs. shortage status")

p2 <- analysis |>
  filter(!is.na(hhi_v1)) |>
  ggplot(aes(x = currently_active, y = hhi_v1)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  labs(x = "Currently on shortage list", y = "Country-level HHI (V1: Germany)",
       title = "Supply concentration vs. shortage status")

p3 <- analysis |>
  filter(!is.na(pct_china_india_v1)) |>
  ggplot(aes(x = currently_active, y = pct_china_india_v1)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_continuous(labels = percent) +
  labs(x = "Currently on shortage list", y = "Share of manufacturers in China/India (V1: Germany)",
       title = "China+India dependence vs. shortage status")

p4 <- model_data_v1 |>
  ggplot(aes(x = n_manufacturers_v1, y = as.numeric(currently_active))) +
  geom_jitter(height = 0.03, alpha = 0.3) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "firebrick") +
  scale_x_log10() +
  labs(x = "Distinct manufacturers (V1: Germany, log scale)", y = "P(currently on shortage list)",
       title = "Fitted shortage probability vs. manufacturer count")

p5 <- analysis |>
  filter(!is.na(n_manufacturers_v2)) |>
  ggplot(aes(x = currently_active, y = n_manufacturers_v2)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_log10() +
  labs(x = "Currently on shortage list", y = "Distinct manufacturers (V2: Germany+EPAR, log scale)",
       title = "Manufacturer count vs. shortage status (V2)")

p6 <- analysis |>
  filter(!is.na(pct_china_india_v2)) |>
  ggplot(aes(x = currently_active, y = pct_china_india_v2)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_continuous(labels = percent) +
  labs(x = "Currently on shortage list", y = "Share of manufacturers in China/India (V2: Germany+EPAR)",
       title = "China+India dependence vs. shortage status (V2)")

p7 <- analysis |>
  filter(!is.na(n_manufacturers_v3)) |>
  ggplot(aes(x = currently_active, y = n_manufacturers_v3)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_log10() +
  labs(x = "Currently on shortage list", y = "Distinct manufacturers (V3: DE+CEP+GMP, log scale)",
       title = "Manufacturer count vs. shortage status (V3)")

p8 <- analysis |>
  filter(!is.na(pct_china_india_v3)) |>
  ggplot(aes(x = currently_active, y = pct_china_india_v3)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_continuous(labels = percent) +
  labs(x = "Currently on shortage list", y = "Share of manufacturers in China/India (V3: DE+CEP+GMP)",
       title = "China+India dependence vs. shortage status (V3)")

p9 <- analysis |>
  filter(!is.na(hhi_v3)) |>
  ggplot(aes(x = currently_active, y = hhi_v3)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  labs(x = "Currently on shortage list", y = "Country-level HHI (V3: DE+CEP+GMP)",
       title = "Supply concentration vs. shortage status (V3)")

ggsave("plot_v1_manufacturers_vs_shortage.png", p1, width = 6, height = 4.5, dpi = 150)
ggsave("plot_v1_hhi_vs_shortage.png",           p2, width = 6, height = 4.5, dpi = 150)
ggsave("plot_v1_china_india_vs_shortage.png",   p3, width = 6, height = 4.5, dpi = 150)
ggsave("plot_v1_shortage_probability_curve.png",p4, width = 6, height = 4.5, dpi = 150)
ggsave("plot_v2_manufacturers_vs_shortage.png", p5, width = 6, height = 4.5, dpi = 150)
ggsave("plot_v2_china_india_vs_shortage.png",   p6, width = 6, height = 4.5, dpi = 150)
ggsave("plot_v3_manufacturers_vs_shortage.png", p7, width = 6, height = 4.5, dpi = 150)
ggsave("plot_v3_china_india_vs_shortage.png",   p8, width = 6, height = 4.5, dpi = 150)
ggsave("plot_v3_hhi_vs_shortage.png",           p9, width = 6, height = 4.5, dpi = 150)

## -----------------------------------------------------------------
## 8. Save the merged analysis table for further digging
## -----------------------------------------------------------------
write_csv(analysis, "atc_shortage_concentration_analysis_WHO.csv")

cat("\nDone. Wrote atc_shortage_concentration_analysis.csv and 9 diagnostic plots.\n")