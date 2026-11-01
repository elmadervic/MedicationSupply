# ---------------------------------------------------------------
# Share of unique manufacturing sites by ATC chapter, split into
# China / India / EU-EEA / Other, each with a darker shade (step 1,
# API disclosed) and a lighter shade (step 2 or step not disclosed),
# computed from data/registers/processed/manufacturer_registers_combined.csv (built by
# 01_combine_sources.R).
#
# Figures (results/<LIST> folder, file names end in _<LIST>, e.g. _WHO):
#   manufacturing_sites_by_chapter_country_group_source.png  -- Germany, EPAR, GMDP, CEP
#   manufacturing_sites_by_chapter_country_group_Ireland.png -- Ireland on its own
#
# Notes on the data:
#  - CEP certificates are held by API manufacturers and GMDP holds API
#    certificates only, so all CEP and GMDP sites are step 1 (darker
#    shades). Ireland has no step field, so its sites only appear in the
#    lighter shades.
#  - Germany: rows without a step (distributors, local representatives)
#    are not manufacturers and are left out.
#  - Germany: marketing-authorisation holders are already excluded in
#    the combined file. Sites are identified by manufacturer name in all
#    sources (the combined file has no BfArM pu_nummer).
#  - EPAR rows with a missing step are treated as step 2 (as before).
#  - Chapter order follows Germany's total site count, the same
#    reference as in EEAnonEEASteps_V2.R, so both figures match.
#
# Requires: install.packages(c("dplyr","ggplot2","stringr"))
# ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(stringr)

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
main_sources  <- c("Germany", "EPAR", "GMDP", "CEP")

# named-vector lookups instead of recode(), which car::recode masks
source_map <- c(ema = "EPAR", germany = "Germany", gmdp = "GMDP",
                cep = "CEP", ireland = "Ireland")

atc_chapter_names <- c(
  A = "A - Alimentary & metabolism", B = "B - Blood & blood forming organs",
  C = "C - Cardiovascular system", D = "D - Dermatologicals",
  G = "G - Genito-urinary system", H = "H - Systemic hormonal preparations",
  J = "J - Antiinfectives (systemic)", L = "L - Antineoplastic & immunomodulating",
  M = "M - Musculo-skeletal system", N = "N - Nervous system",
  P = "P - Antiparasitic products", R = "R - Respiratory system",
  S = "S - Sensory organs", V = "V - Various"
)

## one fixed colour per ATC chapter (same as the treemaps and the HHI
## figures), used for the chapter names on the y axis
chapter_cols <- c(
  A = "#A6CBE8", B = "#2F5A8C", C = "#A8D8B5", D = "#D98BB0",
  G = "#4FA65A", H = "#E0857A", J = "#A33228", L = "#DFA670",
  M = "#D06C2E", N = "#C6A9D8", P = "#4B2470", R = "#D3B53F",
  S = "#5C3A1A", V = "#9A9A9A"
)
## y-axis label colours for the chapters shown, bottom to top
label_colours <- function(chapters) {
  unname(chapter_cols[names(atc_chapter_names)[match(chapters, atc_chapter_names)]])
}

# EU/EEA member states (EU-27 + Iceland, Liechtenstein, Norway)
EEA <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
  "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
  "Spain", "Sweden", "Iceland", "Liechtenstein", "Norway"
)

# ---------------------------------------------------------------
# Load the combined data
# ---------------------------------------------------------------
sites <- read.csv(file.path(PROC_DIR, "manufacturer_registers_combined.csv"),
                  stringsAsFactors = FALSE) %>%
  keep_list() %>%
  mutate(
    source  = unname(source_map[str_to_lower(source)]),
    site_id = str_squish(mfr_company),
    country = str_squish(mfr_country),
    chapter = unname(atc_chapter_names[substr(atc_code, 1, 1)]),
    step    = as.numeric(manufacturer_step),
    step    = if_else(source == "EPAR" & is.na(step), 2, step), # missing EPAR step -> batch release
    step    = if_else(source == "CEP", 1, step)                 # CEPs are held by API manufacturers
  ) %>%
  ## Germany: rows without a step are distributors / local representatives,
  ## not manufacturers -- leave them out (as in the HHI and dataset counts)
  filter(!(source == "Germany" & is.na(step))) %>%
  filter(!is.na(atc_code), atc_code != "",
         !is.na(site_id), site_id != "",
         !is.na(country), country != "")

# ---------------------------------------------------------------
# Classify each site within each chapter by country group and step
# ---------------------------------------------------------------
classified <- sites %>%
  group_by(source, chapter, site_id) %>%
  summarise(
    country   = first(country),
    has_step1 = any(step == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    country_group = case_when(
      country == "China" ~ "China",
      country == "India" ~ "India",
      country %in% EEA   ~ "EU/EEA",
      TRUE               ~ "Other"
    ),
    step_label = ifelse(has_step1, "API (step 1)", "batch release / not disclosed"),
    category   = paste(country_group, "-", step_label)
  )

chapter_summary <- classified %>%
  count(source, chapter, category) %>%
  group_by(source, chapter) %>%
  mutate(total = sum(n), pct = 100 * n / total) %>%
  ungroup()

chapter_order <- chapter_summary %>%
  filter(source == "Germany") %>%
  distinct(chapter, total) %>%
  arrange(desc(total)) %>%
  pull(chapter)
chapter_order <- c(chapter_order, setdiff(unique(chapter_summary$chapter), chapter_order))

category_levels <- c(
  "China - API (step 1)",  "China - batch release / not disclosed",
  "India - API (step 1)",  "India - batch release / not disclosed",
  "EU/EEA - API (step 1)", "EU/EEA - batch release / not disclosed",
  "Other - API (step 1)",  "Other - batch release / not disclosed"
)

## one hue per country group; the step is shown by the shade of that hue
## (full colour = API / step 1, light = batch release / not disclosed)
group_colors <- c(China = "#D7301F", India = "#F28E2B", "EU/EEA" = "#2B5C9E", Other = "#7F7F7F")
step_light   <- c("API (step 1)" = 0, "batch release / not disclosed" = 0.55)  # share of white mixed in

## mix a colour with white: amount 0 = unchanged, 1 = white
lighten <- function(col, amount) {
  m <- col2rgb(col) / 255
  rgb(t(m + (1 - m) * amount))
}

# ---------------------------------------------------------------
# Plot helper. Bar thickness is proportional to sqrt(site count),
# scaled within each source (ggplot2 warns "Ignoring unknown
# aesthetics: width" -- the thickness is applied anyway).
# ---------------------------------------------------------------
make_chapter_plot <- function(summary_df, sources) {
  df <- summary_df %>%
    filter(source %in% sources) %>%
    mutate(chapter  = factor(chapter, levels = rev(chapter_order)),
           source   = factor(source, levels = sources),
           category = factor(category, levels = category_levels))

  totals_label <- df %>%
    distinct(source, chapter, total) %>%
    group_by(source) %>%
    mutate(bar_height = 0.25 + 0.65 * sqrt(total / max(total))) %>%
    ungroup()

  df <- df %>% left_join(select(totals_label, source, chapter, bar_height),
                         by = c("source", "chapter"))

  ## split each category ("China - API (step 1)") into hue and step, and
  ## give it its solid colour: the group's hue, lightened for later steps
  df <- df %>%
    mutate(group_col = factor(sub(" - .*$", "", category), levels = names(group_colors)),
           step_col  = factor(sub("^.* - ", "", category), levels = names(step_light)),
           fill_hex  = mapply(lighten, group_colors[as.character(group_col)],
                              step_light[as.character(step_col)]))

  ggplot(df, aes(x = pct, y = chapter, fill = fill_hex, group = category)) +  # group = stacking order
    geom_col(aes(width = bar_height)) +
    geom_vline(xintercept = 50, linetype = "dashed", color = "grey40") +
    # white background keeps the counts readable on dark segments (e.g. GMDP)
    geom_label(data = totals_label, aes(x = 50, y = chapter, label = total),
               inherit.aes = FALSE, color = "black", size = 5, fontface = "bold",
               fill = "white", alpha = 0.85, label.size = 0,
               label.padding = unit(0.12, "lines")) +
    scale_fill_identity() +
    facet_wrap(~ source, nrow = 1) +
    coord_cartesian(xlim = c(0, 100), clip = "off") +
    labs(x = "Share of unique manufacturing sites (%)", y = NULL) +
    theme_minimal(base_size = 17) +
    theme(
      legend.position = "none",
      legend.text = element_text(size = 16),
      strip.text = element_text(face = "bold", size = 19),
      axis.text.y = element_text(size = 15, face = "bold",
                                 colour = label_colours(levels(droplevels(df$chapter)))),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 17),
      legend.title = element_text(size = 16, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),   # would show through the lighter bars
      plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
    )
}


# ---------------------------------------------------------------
# Legend as a small colour grid: one column per country group, one row per
# step, so each colour is shown next to its lighter shade. Only the
# combinations that occur in the plotted data are drawn.
# ---------------------------------------------------------------
make_legend <- function(summary_df, sources) {
  leg <- summary_df %>%
    filter(source %in% sources) %>%
    distinct(category) %>%
    mutate(group_col = factor(sub(" - .*$", "", category), levels = names(group_colors)),
           step_col  = factor(sub("^.* - ", "", category), levels = rev(names(step_light))),
           fill_hex  = mapply(lighten, group_colors[as.character(group_col)],
                              step_light[as.character(step_col)]))
  ggplot(leg, aes(x = group_col, y = step_col, fill = fill_hex)) +
    geom_tile(width = 0.9, height = 0.8) +
    scale_fill_identity() +
    scale_x_discrete(position = "top", drop = FALSE) +
    scale_y_discrete(drop = TRUE) +
    theme_void(base_size = 17) +
    theme(axis.text.x = element_text(face = "bold", size = 17, margin = margin(b = 4)),
          axis.text.y = element_text(size = 16, hjust = 1, margin = margin(r = 6)))
}

## chapter plot with the colour-grid legend centred above it
with_legend <- function(p, summary_df, sources, legend_width = 0.35) {
  patchwork::wrap_plots(
    patchwork::wrap_plots(patchwork::plot_spacer(), make_legend(summary_df, sources),
                          patchwork::plot_spacer(),
                          widths = c((1 - legend_width) / 2, legend_width, (1 - legend_width) / 2)),
    p, ncol = 1, heights = c(0.12, 1))
}

p <- with_legend(make_chapter_plot(chapter_summary, main_sources), chapter_summary, main_sources)
ggsave(out_file("manufacturing_sites_by_chapter_country_group_source.png"), p,
       width = 25, height = 9.5, dpi = 300)
print(p)

p_ireland <- with_legend(make_chapter_plot(chapter_summary, "Ireland"), chapter_summary, "Ireland",
                          legend_width = 0.8)
ggsave(out_file("manufacturing_sites_by_chapter_country_group_Ireland.png"), p_ireland,
       width = 11, height = 9.5, dpi = 300)
print(p_ireland)
