# ---------------------------------------------------------------
# Descriptive plots for manufacturer_registers_combined.csv, split by source
# columns: atc_code, mfr_company, mfr_country, manufacturer_step, source
#
# FIX vs. previous version: the script re-filtered to
# `critical$`ATC level 5`` using Data/critical.csv, but (a) that file
# doesn't exist in this data set, and (b) even if it did, base R's
# read.csv() would have mangled the column name "ATC level 5" into
# "ATC.level.5" (dots replace spaces), so critical$`ATC level 5`
# resolved to NULL and `atc_code %in% NULL` silently evaluated to
# FALSE for every row -- zeroing out the entire dataset before any
# plot was built. Since manufacturer_registers_combined.csv was
# already filtered to critical ATC codes when it was produced (see
# combine_manufacturer_registers.R), that re-filter step is both
# redundant and the actual bug -- it's removed here.
#
# Requires: install.packages(c("dplyr","tidyr","ggplot2","patchwork","stringr","tidytext","scales"))
# ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(tidytext)
library(scales)
library(purrr)

# ---- helper: force whole-number-only breaks on any count axis ----
# (builds the sequence directly with an integer step, rather than
# filtering pretty()'s output -- pretty() can still propose steps
# like 0.5 or 2.5 on small ranges, which floor()+unique() doesn't
# always fully collapse)
integer_breaks <- function(n = 5) {
  function(x) {
    rng <- range(x, na.rm = TRUE)
    if (!is.finite(rng[1]) || !is.finite(rng[2])) return(numeric(0))
    if (diff(rng) == 0) return(round(rng[1]))
    step <- max(1, round(diff(rng) / n))
    seq(floor(rng[1]), ceiling(rng[2]), by = step)
  }
}


data <- read.csv("manufacturer_registers_combined.csv", stringsAsFactors = FALSE)

cat("rows read from manufacturer_registers_combined.csv:", nrow(data), "\n")

x <- data[data$source == "cep", ]
str(x)
str(unique(x$atc_code))

# NOTE: manufacturer_registers_combined.csv is already restricted to
# critical ATC codes (filtered at creation time in
# combine_manufacturer_registers.R against the union of EMA/BfArM/
# Ireland ATC codes) -- no further critical.csv filtering needed or
# possible here, since no standalone critical.csv exists for this
# data set.


# defensive fix: some Ireland rows still have unsplit "A | B" manufacturer
# strings paired with a single country -- split company and country together
# wherever a "|" is present so each manufacturer lines up with its own country.
#
# FIX: this used to be a plain separate_rows(mfr_company, mfr_country, ...),
# which requires both columns to split into the SAME number of pieces per
# row. For ~21 Ireland rows they don't (e.g. 3 companies vs. 4 countries),
# and separate_rows silently recycles the shorter list to match the longer
# one -- mispairing manufacturers with the wrong countries rather than
# erroring or leaving them alone. Replaced with an explicit length check:
# rows split cleanly, wherever the two "|"-lists have the same length; rows
# where they don't are left as a single unsplit fallback row instead of
# guessing a pairing (same approach used in the manufacturer-register
# combining script for the same underlying data).
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

# fix: translate German (and other non-English) country names to English
# BEFORE any counting -- otherwise "Deutschland" and "Germany" (or
# "Spanien" and "Spain") get counted as separate countries
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
    source = str_to_title(source),                 # ireland/ema/germany/cep -> Ireland/Ema/Germany/Cep (Ema, Cep renamed below)
    source = recode(source, "Ema" = "EPAR", "Cep" = "CEP")
  ) %>%
  filter(!is.na(atc_code), atc_code != "",
         !is.na(mfr_company), mfr_company != "",
         !is.na(mfr_country), mfr_country != "")

cat("rows after cleaning/splitting:", nrow(data), "\n")

source_pal <- c(EPAR = "#1D6F5C", Germany = "#B9861A", Ireland = "#C1461D", CEP = "#1A4D7A")
source_levels <- c("EPAR", "Germany", "Ireland", "CEP")
data$source <- factor(data$source, levels = source_levels)

# ---------------------------------------------------------------
# Console summary
# ---------------------------------------------------------------
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

# ---------------------------------------------------------------
# Per-ATC-code x source summary (for the histograms)
# ---------------------------------------------------------------
atc_summary <- data %>%
  distinct(atc_code, mfr_company, mfr_country, source) %>%
  group_by(source, atc_code) %>%
  summarise(
    n_manufacturers = n_distinct(mfr_company),
    n_countries = n_distinct(mfr_country),
    .groups = "drop"
  )

# ---------------------------------------------------------------
# Plot 1: total ATC codes / manufacturers / countries per source
# ---------------------------------------------------------------
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

# ---------------------------------------------------------------
# Plot 2: manufacturers per ATC code, faceted by source (free scales)
# ---------------------------------------------------------------
p_manu <- ggplot(atc_summary, aes(x = n_manufacturers, fill = source)) +
  geom_histogram(binwidth = 2, boundary = 0, color = "white") +
  facet_wrap(~ source, scales = "free", nrow = 1, drop = FALSE) +
  scale_fill_manual(values = source_pal, guide = "none") +
  scale_x_continuous(breaks = integer_breaks(), labels = label_number(accuracy = 1)) +
  scale_y_continuous(breaks = integer_breaks(), labels = label_number(accuracy = 1)) +
  labs(x = "Distinct manufacturers per ATC code", y = "Number of ATC codes") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

# ---------------------------------------------------------------
# Plot 3: countries per ATC code, faceted by source (free scales)
# ---------------------------------------------------------------
p_ctry <- ggplot(atc_summary, aes(x = n_countries, fill = source)) +
  geom_histogram(binwidth = 1, color = "white") +
  facet_wrap(~ source, scales = "free", nrow = 1, drop = FALSE) +
  scale_fill_manual(values = source_pal, guide = "none") +
  scale_x_continuous(breaks = integer_breaks(), labels = label_number(accuracy = 1)) +
  scale_y_continuous(breaks = integer_breaks(), labels = label_number(accuracy = 1)) +
  labs(x = "Distinct countries per ATC code", y = "Number of ATC codes") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

# ---------------------------------------------------------------
# Plot 4: top 10 countries per source
# ---------------------------------------------------------------
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

# ---------------------------------------------------------------
# Combine and save
# ---------------------------------------------------------------
report_plot <- p_totals / p_manu / p_ctry / p_top_countries +
  plot_layout(heights = c(1, 1, 1, 1.2))

ggsave("Data/plots_by_source.png", report_plot, width = 15, height = 20, dpi = 300)
print(report_plot)