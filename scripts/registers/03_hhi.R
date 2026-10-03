source("scripts/registers/paths.R")

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)

germany_raw <- read.csv(file.path(RAW_DIR, "bfarm_api_origin_critical_rest_LONG.csv"), stringsAsFactors = FALSE) %>%
  filter(role != "Zulassungsinhaber")

EPAR    <- read.csv(file.path(RAW_DIR, "EMA_data_critical.csv"), stringsAsFactors = FALSE)
ireland <- read.csv(file.path(RAW_DIR, "ireland_critical_atc_review.csv"), stringsAsFactors = FALSE)
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

if (file.exists(file.path(RAW_DIR, "critical.csv"))) {
  critical <- read.csv(file.path(RAW_DIR, "critical.csv"), stringsAsFactors = FALSE)
  critical_codes <- unique(critical$ATC.level.5)
  critical_codes <- critical_codes[!is.na(critical_codes) & critical_codes != ""]
  cat("Critical ATC codes loaded from critical.csv:", length(critical_codes), "\n")
} else {
  critical_codes <- unique(c(epar_clean$atc_code, germany_clean$atc_code, ireland_std$atc_code))
  critical_codes <- critical_codes[!is.na(critical_codes) & critical_codes != ""]
  cat("critical.csv not found in", RAW_DIR, "-- falling back to the derived union of",
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
