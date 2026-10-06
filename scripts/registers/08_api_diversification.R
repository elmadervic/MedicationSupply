source("scripts/registers/paths.R")

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(ggrepel)
library(purrr)

germany <- read.csv(file.path(RAW_DIR, "bfarm_api_origin_critical_rest_LONG.csv"), stringsAsFactors = FALSE) %>%
  filter(role != "Zulassungsinhaber")
EPAR    <- read.csv(file.path(RAW_DIR, "EMA_data_critical.csv"), stringsAsFactors = FALSE)
ireland <- read.csv(file.path(RAW_DIR, "ireland_critical_atc_review.csv"), stringsAsFactors = FALSE)

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
