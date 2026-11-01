# ---------------------------------------------------------------
# Descriptive plots for manufacturer_registers_combined.csv, split by source
# columns: atc_code, mfr_company, mfr_country, manufacturer_step, source
#
# Counting rules (same as Table 1 of the paper): German distributors and
# local representatives are left out, and manufacturer names are compared
# ignoring upper/lower case and spacing.
#
# Outputs (results/<LIST> folder, file names end in _<LIST>, e.g. _WHO):
#   plots_by_source.png -- Germany, EPAR, GMDP and CEP side by side
#   data_ireland.png    -- the same four panels for the Ireland data set alone (2 x 2)
#
# No critical.csv re-filter here: manufacturer_registers_combined.csv is
# already restricted to critical ATC codes by 01_combine_sources.R.
#
# Requires: install.packages(c("dplyr","tidyr","ggplot2","patchwork","stringr","tidytext","scales","purrr"))
# ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(tidytext)
library(scales)
library(purrr)

## ---- which list -------------------------------------------------------
## "EU" = EU Union list, "WHO" = WHO EML, "overlap" = on both lists.
## run_all_lists.R sets LIST and runs this script once per list.
if (!exists("LIST")) LIST <- "EU"
source("scripts/registers/paths.R", local = TRUE)
OUT_DIR  <- file.path(RESULTS_DIR, LIST)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

## output file name with the list added: out_file("plot.png") -> results/WHO/plot_WHO.png
out_file <- function(name) file.path(OUT_DIR, sub("(\\.[A-Za-z]+)$", paste0("_", LIST, "\\1"), name))

keep_list <- function(df) {
  switch(LIST,
         EU      = dplyr::filter(df, on_eu_list),
         WHO     = dplyr::filter(df, on_who_eml),
         overlap = dplyr::filter(df, on_eu_list, on_who_eml),
         stop('LIST must be "EU", "WHO" or "overlap"'))
}
cat("List:", LIST, "-> outputs in", OUT_DIR, "
")
# ---- helper: force whole-number-only breaks on any count axis ----
integer_breaks <- function(n = 5) {
  function(x) {
    rng <- range(x, na.rm = TRUE)
    if (!is.finite(rng[1]) || !is.finite(rng[2])) return(numeric(0))
    if (diff(rng) == 0) return(round(rng[1]))
    step <- max(1, round(diff(rng) / n))
    seq(floor(rng[1]), ceiling(rng[2]), by = step)
  }
}

data <- read.csv(file.path(PROC_DIR, "manufacturer_registers_combined.csv"),
                 stringsAsFactors = FALSE) %>%
  keep_list()

cat("rows read from manufacturer_registers_combined.csv:", nrow(data), "\n")

# defensive fix: some Ireland rows still have unsplit "A | B" manufacturer
# strings. Split company and country together only where both "|"-lists
# have the same length; otherwise keep the row unsplit rather than guess
# a pairing (separate_rows would silently recycle and mispair them).
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

# translate German (and other variant) country names to English BEFORE any
# counting -- otherwise "Deutschland" and "Germany" count as two countries
country_map <- c(
  "Argentinien" = "Argentina", "Australien" = "Australia", "Belgien" = "Belgium",
  "Brasilien" = "Brazil", "Bulgarien" = "Bulgaria", "Kroatien" = "Croatia",
  "Deutschland" = "Germany", "Dänemark" = "Denmark", "Finnland" = "Finland",
  "Frankreich" = "France", "Griechenland" = "Greece", "Indien" = "India",
  "Irland" = "Ireland", "Island" = "Iceland", "Italien" = "Italy", "Kanada" = "Canada",
  "Korea, Republik" = "South Korea", "Republic of Korea" = "South Korea",
  "Korea, Republic of" = "South Korea",
  "Südkorea" = "South Korea", "Lettland" = "Latvia", "Litauen" = "Lithuania",
  "Mexiko" = "Mexico", "Niederlande" = "Netherlands", "The Netherlands" = "Netherlands",
  "Norwegen" = "Norway", "Polen" = "Poland", "Rumänien" = "Romania",
  "Schweden" = "Sweden", "Schweiz" = "Switzerland", "Singapur" = "Singapore",
  "Slowakei" = "Slovakia", "Slowenien" = "Slovenia", "Südafrika" = "South Africa",
  "Tschechische Republik" = "Czech Republic", "Czechia" = "Czech Republic",
  "Türkei" = "Turkey",
  "UK" = "United Kingdom", "USA" = "United States", "Ungarn" = "Hungary",
  "Vereinigte Staaten" = "United States", "Vereinigtes Königreich" = "United Kingdom",
  "Vereinigtes Königreich (Nordirland)" = "United Kingdom", "Zypern" = "Cyprus",
  "Österreich" = "Austria"
)

source_map <- c("ema" = "EPAR", "germany" = "Germany", "gmdp" = "GMDP",
                "cep" = "CEP", "ireland" = "Ireland")

# lookups use plain named-vector indexing instead of recode(), which is
# masked by car::recode (and others) when those packages are loaded

data <- data %>%
  mutate(
    mfr_company = str_trim(mfr_company),
    mfr_country = str_trim(mfr_country),
    mfr_country = coalesce(unname(country_map[mfr_country]), mfr_country),
    source = str_to_lower(source),
    source = coalesce(unname(source_map[source]), source)
  ) %>%
  filter(!is.na(atc_code), atc_code != "",
         !is.na(mfr_company), mfr_company != "",
         !is.na(mfr_country), mfr_country != "") %>%
  ## Germany: rows without a step are distributors / local representatives,
  ## not manufacturers -- leave them out (as in Table 1, the HHI and the
  ## manufacturing-site figures)
  filter(!(source == "Germany" & is.na(manufacturer_step))) %>%
  ## manufacturers are counted by name regardless of upper/lower case and
  ## spacing ("PFIZER" = "Pfizer"), the same rule as in Table 1
  mutate(mfr_company = str_to_lower(str_squish(mfr_company)))

cat("rows after cleaning/splitting:", nrow(data), "\n")

source_pal <- c(EPAR = "#1D6F5C", Germany = "#B9861A", GMDP = "#6B4C9A",
                CEP = "#1A4D7A", Ireland = "#C1461D")

# ---------------------------------------------------------------
# Console summary (all sources, Ireland included)
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
# Builds the four-panel report for whichever sources are passed in:
#   1. total ATC codes / manufacturers / countries per source
#   2. manufacturers per ATC code (histogram)
#   3. countries per ATC code (histogram)
#   4. top 10 countries per source
# ---------------------------------------------------------------
## grid = FALSE: the four panels stacked in one column (main report);
## grid = TRUE:  2 x 2 layout with panel tags A-D (single-source report)
make_report <- function(df, levels, grid = FALSE) {
  df <- df %>%
    filter(source %in% levels) %>%
    mutate(source = factor(source, levels = levels))
  pal <- source_pal[levels]

  atc_summary <- df %>%
    distinct(atc_code, mfr_company, mfr_country, source) %>%
    group_by(source, atc_code) %>%
    summarise(
      n_manufacturers = n_distinct(mfr_company),
      n_countries = n_distinct(mfr_country),
      .groups = "drop"
    )

  totals <- df %>%
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
    scale_fill_manual(values = pal, name = NULL, drop = FALSE) +
    labs(x = NULL, y = "Count (log scale)") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top", panel.grid.minor = element_blank())

  p_manu <- ggplot(atc_summary, aes(x = n_manufacturers, fill = source)) +
    geom_histogram(binwidth = 2, boundary = 0, color = "white") +
    facet_wrap(~ source, scales = "free", nrow = 1, drop = FALSE) +
    scale_fill_manual(values = pal, guide = "none") +
    scale_x_continuous(breaks = integer_breaks(), labels = label_number(accuracy = 1)) +
    scale_y_continuous(breaks = integer_breaks(), labels = label_number(accuracy = 1)) +
    labs(x = "Distinct manufacturers per ATC code", y = "Number of ATC codes") +
    theme_minimal(base_size = 12) +
    theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

  p_ctry <- ggplot(atc_summary, aes(x = n_countries, fill = source)) +
    geom_histogram(binwidth = 1, color = "white") +
    facet_wrap(~ source, scales = "free", nrow = 1, drop = FALSE) +
    scale_fill_manual(values = pal, guide = "none") +
    scale_x_continuous(breaks = integer_breaks(), labels = label_number(accuracy = 1)) +
    scale_y_continuous(breaks = integer_breaks(), labels = label_number(accuracy = 1)) +
    labs(x = "Distinct countries per ATC code", y = "Number of ATC codes") +
    theme_minimal(base_size = 12) +
    theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

  top_countries <- df %>%
    distinct(atc_code, mfr_company, mfr_country, source) %>%
    count(source, mfr_country, name = "n_records") %>%
    group_by(source) %>%
    slice_max(n_records, n = 10) %>%
    ungroup()

  p_top_countries <- ggplot(top_countries,
                            aes(x = reorder_within(mfr_country, n_records, source),
                                y = n_records, fill = source)) +
    geom_col() +
    coord_flip() +
    tidytext::scale_x_reordered() +
    facet_wrap(~ source, scales = "free", nrow = 1, drop = FALSE) +
    scale_fill_manual(values = pal, guide = "none") +
    scale_y_continuous(breaks = integer_breaks(), labels = label_number(accuracy = 1)) +
    labs(x = NULL, y = "Manufacturing records") +
    theme_minimal(base_size = 11) +
    theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

  if (grid) {
    ## one source only: the colour legend says nothing, drop it
    p_totals <- p_totals + theme(legend.position = "none")
    return((p_totals | p_manu) / (p_ctry | p_top_countries) +
             plot_annotation(tag_levels = "A") &
             theme(plot.tag = element_text(face = "bold", size = 14)))
  }

  p_totals / p_manu / p_ctry / p_top_countries +
    plot_layout(heights = c(1, 1, 1, 1.2))
}

# ---------------------------------------------------------------
# Main report: Germany, EPAR, GMDP, CEP (Ireland excluded)
# ---------------------------------------------------------------
report_plot <- make_report(data, levels = c("Germany", "EPAR", "GMDP", "CEP"))
ggsave(out_file("plots_by_source.png"), report_plot,
       width = 15, height = 20, dpi = 300)
print(report_plot)

# ---------------------------------------------------------------
# Separate report: Ireland only
# ---------------------------------------------------------------
ireland_plot <- make_report(data, levels = "Ireland", grid = TRUE)   # 2 x 2 panels
ggsave(out_file("data_ireland.png"), ireland_plot,
       width = 12, height = 9, dpi = 300)
print(ireland_plot)
