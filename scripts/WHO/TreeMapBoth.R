## =================================================================
## Treemaps of ATC Level 1 chapters: EU Union list vs WHO EML
## Two panels side by side, one shared legend (same colour per chapter)
## =================================================================
library(dplyr)
library(ggplot2)
library(treemapify)

## ---- paths ----------------------------------------------------------
path_eu_list   <- "critical.csv"                        # EU Union list (ATC.level.5)
path_who_codes <- "processed/who_eml_critical_codes.csv"    # from 01_combine_sources_WHO.R
out_png        <- "figures/plot4_treemap_chapters_eu_who.png"

## ---- colours ----------------------------------------------------------
## Chapter colours of the original EU treemap (plot4_treemap_chapters.png).
## Chapters not listed get a colour from `extra_cols` automatically.
old_pal <- c(
  A = "#9FCCED", B = "#1E5D92", C = "#9BDAB2", G = "#25A24E", H = "#EB7F77",
  J = "#B02621", L = "#EBA367", M = "#DE691B", N = "#CDADDB", P = "#582170",
  R = "#D8B408", S = "#64320F", V = "#9A9A9A"
)

default_pal <- c(A = "#F28E2B", B = "#A0CBE8", C = "#E15759", D = "#FFBE7D",
                 G = "#8CD17D", H = "#B6992D", J = "#2E5A87", L = "#59A14F",
                 M = "#9D7660", N = "#499894", P = "#D37295", R = "#FF9D9A",
                 S = "#79706E", V = "#BAB0AC")

## colours for chapters missing from old_pal (used in this order)
## (D - Dermatologicals is only on the WHO list -> first colour, teal)
extra_cols <- c("#1B8A8A", "#9C6ADE", "#C45A8A", "#7F9C3D", "#5B7DB1")

## ---- codes per list -------------------------------------------------
eu_codes  <- read.csv(path_eu_list, stringsAsFactors = FALSE)$ATC.level.5
who_codes <- read.csv(path_who_codes, stringsAsFactors = FALSE)$atc_code

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

chapter_counts <- function(codes, list_label) {
  tibble(atc_code = unique(codes[!is.na(codes) & codes != ""])) |>
    mutate(chapter = substr(atc_code, 1, 1)) |>
    filter(chapter %in% names(chapter_names)) |>
    count(chapter, name = "n") |>
    mutate(share = n / sum(n),
           list  = sprintf("%s (n = %d)", list_label, sum(n)))
}

tm <- bind_rows(
  chapter_counts(eu_codes,  "EU Union list of critical medicines"),
  chapter_counts(who_codes, "WHO Model List of Essential Medicines")
)

## ---- one colour per chapter, shared by both panels -------------------
chapters_used <- names(chapter_names)[names(chapter_names) %in% tm$chapter]
base_pal      <- if (length(old_pal) > 0) old_pal else default_pal
missing_ch    <- setdiff(chapters_used, names(base_pal))
chapter_cols  <- c(base_pal, setNames(extra_cols[seq_along(missing_ch)], missing_ch))
if (length(missing_ch) > 0)
  cat("Chapters not in old_pal, given new colours:", paste(missing_ch, collapse = ", "), "\n")

## dark text on light tiles, white text on dark tiles
is_light <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  (0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]) > 0.62
}

tm <- tm |>
  mutate(
    chapter_label = factor(paste0(chapter, " - ", chapter_names[chapter]),
                           levels = paste0(chapters_used, " - ", chapter_names[chapters_used])),
    tile_label    = sprintf("%s\n%s\n%.0f%%", chapter, chapter_short[chapter], 100 * share),
    list          = factor(list, levels = unique(list)),
    text_colour   = if_else(is_light(chapter_cols[chapter]), "grey15", "white")
  )

chapter_pal <- setNames(unname(chapter_cols[chapters_used]), levels(tm$chapter_label))

## print the numbers used in the text
tm |> group_by(list) |> arrange(desc(share), .by_group = TRUE) |>
  mutate(pct = round(100 * share, 1)) |> select(list, chapter, n, pct) |> print(n = Inf)

## ---- plot -------------------------------------------------------------
p <- ggplot(tm, aes(area = n, fill = chapter_label, label = tile_label, colour = text_colour)) +
  geom_treemap(colour = "white", size = 2) +
  geom_treemap_text(fontface = "bold", place = "centre",
                    grow = FALSE, size = 13, min.size = 5, reflow = TRUE,
                    padding.x = grid::unit(1.5, "mm"), padding.y = grid::unit(1.5, "mm")) +
  facet_wrap(~ list, nrow = 1) +
  scale_fill_manual(values = chapter_pal, name = NULL, drop = FALSE) +
  scale_colour_identity() +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 10),
    legend.key.size = unit(0.45, "cm"),
    strip.text      = element_text(face = "bold", size = 13),
    panel.spacing   = unit(1, "lines")
  )

ggsave(out_png, p, width = 13, height = 7.5, dpi = 300, bg = "white")
print(p)