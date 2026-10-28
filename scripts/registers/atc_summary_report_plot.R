# ---------------------------------------------------------------
# Summary report per source, from Data/manufacturer_registers_combined.csv
# (built by 01_combine_sources.R):
#   panel A: total ATC codes / manufacturers / countries per source
#   panel B: per ATC code, distinct manufacturers vs distinct countries
#            (hex bins), one panel per source
#
# Outputs (Results/<LIST> folder, file names end in _<LIST>, e.g. _WHO):
#   atc_summary_report_plot.png          -- A + B, Germany, EPAR, GMDP, CEP
#   atc_summary_report_plot_v2.png       -- B only, Germany, EPAR, GMDP, CEP
#   atc_summary_report_plot_Ireland.png  -- A + B, Ireland on its own
#
# Requires: install.packages(c("dplyr","tidyr","ggplot2","patchwork","stringr","hexbin"))
# ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)

## ---- which list -------------------------------------------------------
## "EU" = EU Union list, "WHO" = WHO EML, "overlap" = on both lists.
## run_all_lists.R sets LIST and runs this script once per list.
if (!exists("LIST")) LIST <- "EU"
DATA_DIR <- "Data"
OUT_DIR  <- file.path("Results", LIST)          # outputs go to Results/EU, /WHO, /overlap
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

## output file name with the list added: out_file("plot.png") -> Results/WHO/plot_WHO.png
out_file <- function(name) file.path(OUT_DIR, sub("(\\.[A-Za-z]+)$", paste0("_", LIST, "\\1"), name))

keep_list <- function(df) {
  switch(LIST,
         EU      = dplyr::filter(df, on_eu_list),
         WHO     = dplyr::filter(df, on_who_eml),
         overlap = dplyr::filter(df, on_eu_list, on_who_eml),
         stop('LIST must be "EU", "WHO" or "overlap"'))
}
cat("List:", LIST, "-> outputs in", OUT_DIR, "\n")

main_sources <- c("Germany", "EPAR", "GMDP", "CEP")
source_pal   <- c(Germany = "#B9861A", EPAR = "#1D6F5C", GMDP = "#6B4C9A",
                  CEP = "#1A4D7A", Ireland = "#C1461D")

# named-vector lookup instead of recode(), which car::recode masks
source_map <- c(ema = "EPAR", germany = "Germany", gmdp = "GMDP",
                cep = "CEP", ireland = "Ireland")

data <- read.csv(file.path(DATA_DIR, "manufacturer_registers_combined.csv"),
                 stringsAsFactors = FALSE) %>%
  keep_list() %>%
  mutate(
    source      = unname(source_map[str_to_lower(source)]),
    mfr_company = str_squish(mfr_company),
    mfr_country = str_squish(mfr_country)
  ) %>%
  filter(!is.na(atc_code), atc_code != "")

has_mfr <- function(company, country) {
  !is.na(company) & company != "" & !is.na(country) & country != ""
}

# ---- panel A: totals per source ----
totals <- data %>%
  group_by(source) %>%
  summarise(
    `ATC codes`   = n_distinct(atc_code),
    Manufacturers = n_distinct(mfr_company[has_mfr(mfr_company, mfr_country)]),
    Countries     = n_distinct(mfr_country[!is.na(mfr_country) & mfr_country != ""]),
    .groups = "drop"
  ) %>%
  pivot_longer(-source, names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, levels = c("ATC codes", "Manufacturers", "Countries")))

# ---- panel B: manufacturers vs countries per ATC code ----
per_code <- data %>%
  distinct(source, atc_code, mfr_company, mfr_country) %>%
  group_by(source, atc_code) %>%
  summarise(
    n_manufacturers = n_distinct(mfr_company[has_mfr(mfr_company, mfr_country)]),
    n_countries     = n_distinct(mfr_country[!is.na(mfr_country) & mfr_country != ""]),
    .groups = "drop"
  )

make_panels <- function(sources) {
  tot <- totals   %>% filter(source %in% sources) %>% mutate(source = factor(source, levels = sources))
  pc  <- per_code %>% filter(source %in% sources) %>% mutate(source = factor(source, levels = sources))

  panelA <- ggplot(tot, aes(x = metric, y = value, fill = source)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    geom_text(aes(label = value), position = position_dodge(width = 0.75),
              vjust = -0.3, size = 3.2) +
    scale_y_log10() +
    scale_fill_manual(values = source_pal[sources], name = NULL) +
    labs(x = NULL, y = "Count") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top", panel.grid.minor = element_blank())

  panelB <- ggplot(pc, aes(x = n_manufacturers, y = n_countries)) +
    geom_hex(bins = 15) +
    scale_fill_gradient(low = "#F4E1D6", high = "#7A2C10", name = "ATC codes") +
    facet_wrap(~ source, scales = "free_x", nrow = 1) +
    labs(x = "Distinct manufacturers", y = "Distinct countries") +
    theme_minimal(base_size = 12) +
    theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

  list(A = panelA, B = panelB)
}

# ---- main sources ----
main <- make_panels(main_sources)
report_plot <- main$A / main$B + plot_layout(heights = c(1, 1.6))
ggsave(out_file("atc_summary_report_plot.png"), report_plot,
       width = 15, height = 9, dpi = 300, bg = "white")
ggsave(out_file("atc_summary_report_plot_v2.png"), main$B,
       width = 16, height = 5, dpi = 300, bg = "white")
print(report_plot)

# ---- Ireland on its own ----
ire <- make_panels("Ireland")
ireland_plot <- ire$A / ire$B + plot_layout(heights = c(1, 1.6))
ggsave(out_file("atc_summary_report_plot_Ireland.png"), ireland_plot,
       width = 6, height = 9, dpi = 300, bg = "white")
print(ireland_plot)
