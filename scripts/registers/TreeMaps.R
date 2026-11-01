## =================================================================
## Treemaps of ATC Level 1 chapters for the critical-medicine lists
## (replaces TreeMapCritical.R, TreeMapWHOEML.R, TreeMapWHOEMLandCritical.R
## and TreeMapBoth.R -- same colour per chapter in every figure)
##
## Input:  data/registers/processed/list_codes.csv (from 01_combine_sources.R): every code on
##         the EU Union list and/or the WHO EML, with on_eu_list, in_eml
##         and on_who_eml flags. Counts are list codes, whether or not a
##         register covers them.
## Outputs:
##   results/EU/treemap_chapters_EU.png    EU Union list (299 codes)
##   results/WHO/treemap_chapters_WHO.png  WHO EML (540 codes as given in the EML,
##                                         incl. 5 level-4 codes)
##   results/overlap/treemap_chapters_overlap.png  codes on both lists
##   results/treemap_chapters_EU_vs_WHO.png  EU and WHO side by side, one legend
##   data/registers/processed/atc_in_both_lists.csv            the overlap codes
##
## Requires: install.packages(c("dplyr","readr","ggplot2","treemapify"))
## =================================================================
library(readr)
library(dplyr)
library(ggplot2)
library(treemapify)

source("scripts/registers/paths.R", local = TRUE)
codes <- read_csv(file.path(PROC_DIR, "list_codes.csv"), show_col_types = FALSE)

lists <- list(
  EU      = list(codes = codes$atc_code[codes$on_eu_list],
                 title = "EU Union list of critical medicines"),
  WHO     = list(codes = codes$atc_code[codes$in_eml],
                 title = "WHO Model List of Essential Medicines"),
  overlap = list(codes = codes$atc_code[codes$on_eu_list & codes$on_who_eml],
                 title = "On both lists")
)
write_csv(filter(codes, on_eu_list, on_who_eml), file.path(PROC_DIR, "atc_in_both_lists.csv"))
cat("EU:", length(lists$EU$codes), "| WHO EML:", length(lists$WHO$codes),
    "| on both:", length(lists$overlap$codes), "\n")

chapter_names <- c(
  A = "Alimentary tract & metabolism", B = "Blood & blood forming organs",
  C = "Cardiovascular system",         D = "Dermatologicals",
  G = "Genito-urinary system & sex hormones", H = "Systemic hormonal preparations",
  J = "Antiinfectives (systemic)",     L = "Antineoplastic & immunomodulating",
  M = "Musculo-skeletal system",       N = "Nervous system",
  P = "Antiparasitic products",        R = "Respiratory system",
  S = "Sensory organs",                V = "Various"
)

## shorter names for inside the tiles (full names stay in the legend)
chapter_short <- c(
  A = "Alimentary & metabolism", B = "Blood", C = "Cardiovascular",
  D = "Dermatologicals", G = "Genito-urinary & sex hormones", H = "Systemic hormones",
  J = "Antiinfectives", L = "Antineoplastic & immunomodulating", M = "Musculo-skeletal",
  N = "Nervous system", P = "Antiparasitic", R = "Respiratory",
  S = "Sensory organs", V = "Various"
)

## one colour per chapter, the same in every treemap (original EU treemap
## colours; D, only on the WHO list, added)
chapter_cols <- c(
  A = "#A6CBE8", B = "#2F5A8C", C = "#A8D8B5", D = "#D98BB0",
  G = "#4FA65A", H = "#E0857A", J = "#A33228", L = "#DFA670",
  M = "#D06C2E", N = "#C6A9D8", P = "#4B2470", R = "#D3B53F",
  S = "#5C3A1A", V = "#9A9A9A"
)
chapter_legend <- setNames(paste(names(chapter_names), "-", chapter_names), names(chapter_names))

## dark text on light tiles, white text on dark tiles
is_light <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  (0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]) > 0.62
}

chapter_counts <- function(list_codes, label) {
  tibble(atc_code = unique(list_codes)) |>
    mutate(chapter = substr(atc_code, 1, 1)) |>
    filter(chapter %in% names(chapter_names)) |>
    count(chapter, name = "n") |>
    mutate(share       = n / sum(n),
           panel       = sprintf("%s (n = %d)", label, sum(n)),
           tile_label  = sprintf("%s\n%s\n%d (%.0f%%)", chapter, chapter_short[chapter], n, 100 * share),
           text_colour = if_else(is_light(chapter_cols[chapter]), "grey15", "white"))
}

treemap_plot <- function(tm) {
  used <- intersect(names(chapter_cols), tm$chapter)
  ggplot(tm, aes(area = n, fill = chapter, label = tile_label, colour = text_colour)) +
    geom_treemap(colour = "white", size = 2) +
    geom_treemap_text(fontface = "bold", place = "centre", grow = FALSE, size = 13,
                      min.size = 5, reflow = TRUE,
                      padding.x = grid::unit(1.5, "mm"), padding.y = grid::unit(1.5, "mm")) +
    facet_wrap(~ panel, nrow = 1) +
    scale_fill_manual(values = chapter_cols, breaks = used,
                      labels = chapter_legend[used], name = NULL) +
    scale_colour_identity() +
    guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom",
          legend.text     = element_text(size = 10),
          legend.key.size = unit(0.45, "cm"),
          strip.text      = element_text(face = "bold", size = 13),
          panel.spacing   = unit(1, "lines"))
}

## ---- one treemap per list --------------------------------------------
for (l in names(lists)) {
  tm <- chapter_counts(lists[[l]]$codes, lists[[l]]$title)
  cat("\n", lists[[l]]$title, "\n")
  print(tm |> arrange(desc(n)) |> mutate(pct = round(100 * share, 1)) |> select(chapter, n, pct))
  out_dir <- file.path(RESULTS_DIR, l)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(out_dir, paste0("treemap_chapters_", l, ".png")), treemap_plot(tm),
         width = 8, height = 7.5, dpi = 300, bg = "white")
}

## ---- EU and WHO side by side ------------------------------------------
tm_both <- bind_rows(chapter_counts(lists$EU$codes,  lists$EU$title),
                     chapter_counts(lists$WHO$codes, lists$WHO$title)) |>
  mutate(panel = factor(panel, levels = unique(panel)))
p <- treemap_plot(tm_both)
ggsave(file.path(RESULTS_DIR, "treemap_chapters_EU_vs_WHO.png"), p,
       width = 13, height = 7.5, dpi = 300, bg = "white")
print(p)
