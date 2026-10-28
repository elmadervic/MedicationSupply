# ---------------------------------------------------------------
# Country-level HHI per critical ATC code, per source, computed from
# Data/manufacturer_registers_combined.csv (built by 01_combine_sources.R).
#
#   1. Step-1 priority: for sources with step data (Germany, EPAR, GMDP),
#      per ATC code keep only step-1 (API) producers if the code has any;
#      otherwise keep step-2 producers. CEP and Ireland have no step field,
#      so all their records are used.
#   2. HHI over countries per ATC code, per source.
#   3. How often each MAIN source (Germany, EPAR, GMDP, CEP) gives the
#      highest HHI for an ATC code. Ireland is kept out of this comparison
#      and out of the combined figures, and gets its own PNGs instead.
#
# Sites are identified by manufacturer name in every source (the combined
# file has no BfArM pu_nummer), so Germany's site counts can differ
# slightly from the old pu_nummer-based version.
#
# Outputs (Results/<LIST> folder, file names end in _<LIST>, e.g. _WHO):
#   hhi_step1_priority_final.csv       -- HHI per ATC code, all 5 sources
#   hhi_highest_by_source_final.csv    -- "highest HHI" counts, main sources
#   hhi_highest_by_source.tex          -- the same as a LaTeX table (caption + label)
#   hhi_by_chapter_by_source.png       -- boxplots by chapter, main sources
#   hhi_by_chapter_<source>.png        -- one per source, incl. Ireland
#   hhi_step1_priority_by_source.png   -- HHI histograms, main sources
#   hhi_step1_priority_Ireland.png     -- HHI histogram, Ireland
#
# Requires: install.packages(c("dplyr","ggplot2","stringr","xtable","RColorBrewer","ggh4x"))
# ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
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
cat("List:", LIST, "-> outputs in", OUT_DIR, "
")
main_sources  <- c("Germany", "EPAR", "GMDP", "CEP")
all_sources   <- c(main_sources, "Ireland")
source_pal    <- c(Germany = "#B9861A", EPAR = "#1D6F5C", GMDP = "#6B4C9A",
                   CEP = "#1A4D7A", Ireland = "#C1461D")

# ---------------------------------------------------------------
# Load the combined data
# (named-vector lookups instead of recode(), which car::recode masks)
# ---------------------------------------------------------------
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
  filter(!is.na(atc_code), atc_code != "",
         !is.na(mfr_company), mfr_company != "",
         !is.na(mfr_country), mfr_country != "")

# ---------------------------------------------------------------
# 1. Step-1 priority
# ---------------------------------------------------------------
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
  transmute(source, atc_code, site_id = mfr_company, country = mfr_country)

step_counts <- data %>%
  filter(!source %in% stepless_sources) %>%
  group_by(source, atc_code) %>%
  summarise(has_step1 = any(manufacturer_step == 1, na.rm = TRUE), .groups = "drop") %>%
  count(source, has_step1)
cat("ATC codes per source using step 1 (TRUE) or step-2 fallback (FALSE):\n")
print(step_counts)

# ---------------------------------------------------------------
# 2. HHI per ATC code, per source
# ---------------------------------------------------------------
hhi_all <- priority %>%
  distinct(source, atc_code, site_id, country) %>%
  count(source, atc_code, country, name = "n_sites") %>%
  group_by(source, atc_code) %>%
  mutate(share = n_sites / sum(n_sites)) %>%
  summarise(
    n_countries         = n_distinct(country),
    n_sites_total       = sum(n_sites),
    hhi                 = sum(share^2) * 10000,
    effective_countries = 10000 / hhi,
    .groups = "drop"
  ) %>%
  mutate(source = factor(source, levels = all_sources)) %>%
  arrange(source, atc_code)

write.csv(mutate(hhi_all, list = LIST, .before = 1), out_file("hhi_step1_priority_final.csv"), row.names = FALSE)

# ---------------------------------------------------------------
# 3. How often does each main source hold the HIGHEST HHI per ATC code?
# ---------------------------------------------------------------
hhi_main <- hhi_all %>%
  filter(source %in% main_sources) %>%
  mutate(source = factor(source, levels = main_sources))

highest <- hhi_main %>%
  group_by(atc_code) %>%
  filter(hhi == max(hhi)) %>%          # ties counted for every tied source
  ungroup()

winner_pct <- highest %>%
  count(source, name = "n_times_highest", .drop = FALSE) %>%
  left_join(count(hhi_main, source, name = "n_codes_covered", .drop = FALSE), by = "source") %>%
  mutate(pct_of_own_coverage = round(100 * n_times_highest / n_codes_covered, 1))

n_ties <- highest %>% count(atc_code) %>% filter(n > 1) %>% nrow()

cat("=========================================================\n")
cat("HOW OFTEN EACH SOURCE HOLDS THE HIGHEST HHI (Germany, EPAR, GMDP, CEP)\n")
cat("=========================================================\n")
print(winner_pct)
cat("\nATC codes with data from at least one main source:", n_distinct(hhi_main$atc_code), "\n")
cat("ATC codes with an exact tie for highest HHI between 2+ sources:", n_ties, "\n")

write.csv(mutate(winner_pct, list = LIST, .before = 1), out_file("hhi_highest_by_source_final.csv"), row.names = FALSE)

## LaTeX table: clear column names, caption and label; printed and saved
## as hhi_highest_by_source_<LIST>.tex
library(xtable)
list_name <- c(EU      = "the EU Union list of critical medicines",
               WHO     = "the WHO Model List of Essential Medicines",
               overlap = "medicines on both the EU Union list and the WHO EML")[[LIST]]
n_codes_any <- n_distinct(hhi_main$atc_code)

winner_tex <- winner_pct %>%
  transmute(
    `Data source`                                      = as.character(source),
    `\\shortstack[r]{ATC codes\\\\covered}`            = n_codes_covered,
    `\\shortstack[r]{ATC codes with\\\\highest HHI}`   = n_times_highest,
    `\\shortstack[r]{Share of covered\\\\codes (\\%)}` = pct_of_own_coverage
  )

winner_xt <- xtable(
  winner_tex,
  caption = paste0(
    "Number of ATC codes for which each manufacturer data source gives the highest ",
    "country-level Herfindahl--Hirschman index (HHI), for ", list_name, " (", n_codes_any,
    " ATC codes with data from at least one source). For each ATC code, the HHI was computed ",
    "over the countries of its manufacturing sites in each source separately (step-1/API sites ",
    "where available, otherwise step-2 sites; all sites for CEP), and the source with the ",
    "highest HHI, i.e.\\ the most concentrated picture of the supply, was counted. ",
    "Ties are counted for every tied source (", n_ties, " ATC codes). ",
    "The share refers to the ATC codes covered by that source."),
  label  = paste0("tab:hhi_highest_by_source_", LIST),
  align  = c("l", "l", "r", "r", "r"),
  digits = c(0, 0, 0, 0, 1)
)
for (f in list("", out_file("hhi_highest_by_source.tex")))
  print(winner_xt, file = f, include.rownames = FALSE, booktabs = TRUE,
        caption.placement = "top", table.placement = "htbp",
        sanitize.colnames.function = identity,
        sanitize.text.function = identity)

# ---------------------------------------------------------------
# HHI distribution by ATC Level 1 chapter
#
# Dashed reference lines at HHI = 1500 and 2500 mark the standard
# antitrust thresholds (US DOJ/FTC Horizontal Merger Guidelines):
#   < 1500 = unconcentrated, 1500-2500 = moderately concentrated,
#   > 2500 = highly concentrated. Used here only as an intuition
#   anchor, not a formal application of antitrust doctrine.
# ---------------------------------------------------------------
chapter_names <- c(
  A = "A - Alimentary & metabolism", B = "B - Blood & blood forming organs",
  C = "C - Cardiovascular system", D = "D - Dermatologicals",
  G = "G - Genito-urinary system", H = "H - Systemic hormonal preparations",
  J = "J - Antiinfectives (systemic)", L = "L - Antineoplastic & immunomodulating",
  M = "M - Musculo-skeletal system", N = "N - Nervous system",
  P = "P - Antiparasitic products", R = "R - Respiratory system",
  S = "S - Sensory organs", V = "V - Various"
)

hhi_all <- hhi_all %>%
  mutate(chapter = unname(chapter_names[substr(atc_code, 1, 1)]))

# chapter order: median HHI across the MAIN sources, so the combined
# figure's row order isn't shifted by Ireland; chapters only Ireland
# has are appended at the end
chapter_order <- hhi_all %>%
  filter(source %in% main_sources) %>%
  group_by(chapter) %>%
  summarise(med = median(hhi), .groups = "drop") %>%
  arrange(desc(med)) %>%
  pull(chapter)
chapter_order <- c(chapter_order, setdiff(unique(hhi_all$chapter), chapter_order))

hhi_all$chapter <- factor(hhi_all$chapter, levels = rev(chapter_order))

# one fixed colour per ATC chapter, the same in every figure and list
# (same colours as the treemaps and the manufacturing-site figures)
chapter_cols <- c(
  A = "#A6CBE8", B = "#2F5A8C", C = "#A8D8B5", D = "#D98BB0",
  G = "#4FA65A", H = "#E0857A", J = "#A33228", L = "#DFA670",
  M = "#D06C2E", N = "#C6A9D8", P = "#4B2470", R = "#D3B53F",
  S = "#5C3A1A", V = "#9A9A9A"
)
chapter_pal <- setNames(unname(chapter_cols[names(chapter_names)]), unname(chapter_names))

# y-axis label colours must follow the chapters actually shown, bottom to top
label_colours <- function(df) unname(chapter_pal[levels(droplevels(df$chapter))])

chapter_boxplot <- function(df) {
  ggplot(df, aes(x = hhi, y = chapter, fill = chapter)) +
    geom_vline(xintercept = c(1500, 2500), linetype = "dashed", color = "grey40") +
    geom_boxplot(outlier.size = 1, show.legend = FALSE, varwidth = TRUE) +
    scale_fill_manual(values = chapter_pal) +
    scale_x_continuous(breaks = c(0, 2500, 5000, 7500, 10000)) +
    labs(x = "HHI", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_text(face = "bold", size = 9, colour = label_colours(df)),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

# ---- combined figure: main sources ----
hhi_main_ch <- hhi_all %>%
  filter(source %in% main_sources) %>%
  mutate(source = factor(source, levels = main_sources))

p <- chapter_boxplot(hhi_main_ch) +
  ggh4x::facet_wrap2(~ source, nrow = 1, strip = ggh4x::strip_themed(
    text_x = ggh4x::elem_list_text(
      colour = unname(source_pal[main_sources]),
      face   = rep("bold", length(main_sources)),
      size   = rep(13, length(main_sources))
    )
  ))

ggsave(out_file("hhi_by_chapter_by_source.png"), p, width = 17, height = 8, dpi = 300)
print(p)

# ---- one figure per source, Ireland included ----
for (src in all_sources) {
  df_src <- hhi_all %>% filter(source == src)
  p_single <- chapter_boxplot(df_src) +
    labs(title = src) +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5,
                                    colour = source_pal[[src]]))
  ggsave(out_file(paste0("hhi_by_chapter_", src, ".png")),
         p_single, width = 8, height = 8, dpi = 300)
}

# ---------------------------------------------------------------
# HHI histograms (step-1-priority data)
# ---------------------------------------------------------------
hhi_histogram <- function(df, title) {
  ggplot(df, aes(x = hhi, fill = source)) +
    geom_vline(xintercept = c(1500, 2500), linetype = "dashed", color = "grey50") +
    geom_histogram(binwidth = 500, boundary = 0, color = "white") +
    facet_wrap(~ source, nrow = 1, scales = "free_y") +
    scale_fill_manual(values = source_pal, guide = "none") +
    scale_x_continuous(breaks = c(0, 2500, 5000, 7500, 10000)) +
    labs(title = title,
         x = "HHI (0 = diversified, 10000 = single-country)",
         y = "Number of ATC codes") +
    theme_minimal(base_size = 12) +
    theme(strip.text = element_text(face = "bold", size = 13),
          panel.grid.minor = element_blank())
}

p_hist <- hhi_histogram(hhi_main_ch,
                        "Country-level HHI per critical ATC code (step-1-priority filtered)")
ggsave(out_file("hhi_step1_priority_by_source.png"), p_hist, width = 12, height = 5, dpi = 300)
print(p_hist)

p_hist_ire <- hhi_histogram(hhi_all %>% filter(source == "Ireland"),
                            "Country-level HHI per critical ATC code\n(all manufacturers, no step data)")
ggsave(out_file("hhi_step1_priority_Ireland.png"), p_hist_ire, width = 6, height = 5, dpi = 300)
print(p_hist_ire)
