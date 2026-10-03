source("scripts/registers/paths.R")

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(tidytext)
library(scales)
library(purrr)

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
