# ---------------------------------------------------------------
# API-producer diversification plot, one per source, computed from
# data/registers/processed/manufacturer_registers_combined.csv (built by 01_combine_sources.R):
#   x = number of distinct producer sites for the substance (ATC code)
#   y = share of those sites located in China + India (%)
# Substances above the threshold share are highlighted in red (most
# dependent on China/India); only the MAX_LABELS of them with the most
# sites are named (see the options below).
# Substances that are on both lists (EU Union list and WHO EML) are drawn
# as triangles, the others as circles (not in the overlap plots, where
# every substance is on both lists).
#
# Step handling: for sources with step data (Germany, EPAR, GMDP), per
# ATC code, step-1 (API) records are used if the code has any; step-2
# only as a fallback for codes with no step-1 data (same rule as the
# HHI analysis). CEP and Ireland have no step field, so all their
# records are used -- read those two plots with that caveat.
#
# Substances are grouped by ATC code and labelled with the `substance`
# column of the combined file (falls back to the ATC code if missing).
# Sites are identified by manufacturer name in every source.
#
# Outputs (results/<LIST> folder, file names end in _<LIST>, e.g. _WHO), one PNG per source, Ireland included as its own file:
#   api_diversification_Germany.png (+ .pdf), _EPAR, _GMDP, _CEP, _Ireland
#
# Requires: install.packages(c("dplyr","ggplot2","stringr","ggrepel"))
# ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(stringr)
library(ggrepel)

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
## ---- plot options ------------------------------------------------------
HIGHLIGHT_THRESHOLD <- 65   # red above this China + India share (%)
MAX_LABELS          <- 25   # at most this many red substances get a name,
# ranked by number of sites (most first): a high
# share across many sites is named before a 100%
# share from one or two sites. Inf = name all.
MIN_SITES_TO_NAME   <- 2    # red substances with fewer sites stay unnamed
# (with 1-2 sites the share can only be 0, 50 or
# 100%, and many substances pile up on one point)

main_sources  <- c("Germany", "EPAR", "GMDP", "CEP")
all_sources   <- c(main_sources, "Ireland")

# named-vector lookup instead of recode(), which car::recode masks
source_map <- c(ema = "EPAR", germany = "Germany", gmdp = "GMDP",
                cep = "CEP", ireland = "Ireland")

data <- read.csv(file.path(PROC_DIR, "manufacturer_registers_combined.csv"),
                 stringsAsFactors = FALSE) %>%
  keep_list() %>%
  mutate(
    source      = unname(source_map[str_to_lower(source)]),
    mfr_company = str_squish(mfr_company),
    mfr_country = str_squish(mfr_country),
    substance   = coalesce(na_if(str_squish(substance), ""), atc_code)
  ) %>%
  filter(!is.na(atc_code), atc_code != "",
         !is.na(mfr_company), mfr_company != "",
         !is.na(mfr_country), mfr_country != "")

# ---- step-1 priority (CEP and Ireland have no step field: keep all) ----
stepless_sources <- c("CEP", "Ireland")

priority <- data %>%
  group_by(source, atc_code) %>%
  mutate(has_step1 = any(manufacturer_step == 1, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(
    source %in% stepless_sources |
      ( has_step1 & manufacturer_step %in% 1) |
      (!has_step1 & manufacturer_step %in% 2)
  ) %>%
  transmute(source, atc_code, substance, site_id = mfr_company, country = mfr_country,
            on_both = on_eu_list & on_who_eml)

## name of the "other" list, for the legend
other_list <- c(EU = "also on the WHO EML", WHO = "also on the EU Union list")[LIST]
mark_both  <- LIST != "overlap"   # in the overlap plots every substance is on both

# ---------------------------------------------------------------
# Plot helper
# ---------------------------------------------------------------
make_diversification_plot <- function(df, highlight_threshold = HIGHLIGHT_THRESHOLD,
                                      max_labels = MAX_LABELS,
                                      min_sites_to_label = 1, base_color = "#7FA8C9") {
  
  summary_df <- df %>%
    distinct(atc_code, substance, site_id, country, on_both) %>%
    group_by(atc_code) %>%
    summarise(
      substance       = first(substance),
      n_sites         = n_distinct(site_id),
      pct_china_india = 100 * mean(country %in% c("China", "India")),
      on_both         = any(on_both, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_sites >= min_sites_to_label) %>%
    mutate(
      highlight       = pct_china_india > highlight_threshold & n_sites <= 30,
      substance_label = str_trunc(substance, 25, ellipsis = "..."),
      list_group      = factor(if_else(on_both, "both", "one"), levels = c("one", "both"))
    )
  
  ## the red substances that get a name: most sites first, then highest share
  label_df <- summary_df %>%
    filter(highlight, n_sites >= MIN_SITES_TO_NAME) %>%
    arrange(desc(n_sites), desc(pct_china_india), substance) %>%
    slice_head(n = max_labels)
  
  shape_values <- c(one = 16, both = 17)
  shape_labels <- c(one = paste(c(EU = "Critical", WHO = "WHO EML", overlap = "")[LIST], "only"),
                    both = paste("Critical and essential", sep = ""))
  
  p <- ggplot(summary_df, aes(x = n_sites, y = pct_china_india)) +
    geom_hline(yintercept = highlight_threshold, linetype = "dotted", color = "grey60") +
    geom_point(aes(color = highlight, shape = if (mark_both) list_group else NULL),
               size = 2.6, alpha = 0.85) +
    geom_text_repel(
      data = label_df,
      aes(label = substance_label),
      size = 3.7, max.overlaps = Inf, segment.size = 0.3, segment.color = "grey50",
      min.segment.length = 0, box.padding = 0.4, point.padding = 0.2,
      force = 2, force_pull = 0.5, max.iter = 1e5, max.time = 5, seed = 1
    ) +
    ## room above 100% so the labels of the 100% substances can spread out
    scale_y_continuous(breaks = seq(0, 100, 25), limits = c(-2, 125),
                       expand = expansion(mult = c(0.02, 0))) +
    scale_color_manual(values = c(`TRUE` = "#B33018", `FALSE` = base_color), guide = "none") +
    labs(
      x = "Number of distinct API-producer sites  (→ more diversified)",
      y = "Share of sites in China + India (%)  (↑ more dependent)",
      shape = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(), legend.position = "top",
          legend.justification = "left")
  
  if (mark_both)
    p <- p + scale_shape_manual(values = shape_values, labels = shape_labels, drop = FALSE)
  p
}

# ---------------------------------------------------------------
# One plot per source, each saved separately (Ireland included)
# ---------------------------------------------------------------
# cairo_pdf draws the arrow characters in the axis titles; the default
# pdf() device replaces them with dots
pdf_device <- if (capabilities("cairo")) cairo_pdf else "pdf"

plots <- list()
for (src in all_sources) {
  plots[[src]] <- make_diversification_plot(filter(priority, source == src))
  ggsave(out_file(paste0("api_diversification_", src, ".png")),
         plots[[src]], width = 13, height = 8, dpi = 300)
}

ggsave(out_file("api_diversification_Germany.pdf"), plots[["Germany"]],
       width = 13, height = 8, device = pdf_device)

for (src in all_sources) print(plots[[src]])