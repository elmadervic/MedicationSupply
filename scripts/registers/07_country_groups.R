source("scripts/registers/paths.R")

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(patchwork)
library(readr)
library(janitor)
library(purrr)

germany   <- read.csv(file.path(RAW_DIR, "bfarm_api_origin_critical_rest_LONG.csv"), stringsAsFactors = FALSE)
EPAR    <- read.csv(file.path(RAW_DIR, "EMA_data_critical.csv"), stringsAsFactors = FALSE)
ireland <- read.csv(file.path(RAW_DIR, "ireland_critical_atc_review.csv"), stringsAsFactors = FALSE)

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

critical_codes <- unique(c(
  germany$atc_code,
  EPAR$atc_code,
  ireland$matched_critical_atc
))
critical_codes <- critical_codes[!is.na(critical_codes) & critical_codes != ""]

cep_atc2 <- cep_atc[!is.na(cep_atc$atc_code), ]
cep_atc <- cep_atc %>% filter(atc_code %in% critical_codes)
cep_atc <- cep_atc %>% filter(status_cep == "Valid")

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
