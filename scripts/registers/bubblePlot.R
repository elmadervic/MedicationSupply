# ---------------------------------------------------------------
# Bubble-matrix plot per source: rows = country, columns = ATC
# anatomical main group (first letter of ATC code), dot size =
# unique manufacturing sites, dot color = unique ATC-5 codes.
# Styled after a reference plot with grouped columns, bold/boxed
# non-EU countries, and a blue size+color legend.
#
# Outputs (results/<LIST> folder, file names end in _<LIST>, e.g. _WHO):
#   bubble_matrix_Germany.png, _EPAR.png, _GMDP.png, _CEP.png -- one per source
#   bubble_matrix_by_source.png -- Germany, EPAR, GMDP, CEP in a 2x2 grid
#   bubble_matrix_Ireland.png   -- Ireland on its own (not in the grid)
#
# Requires: install.packages(c("dplyr","tidyr","ggplot2","stringr","forcats","patchwork","scales","purrr"))
# ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(patchwork)
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
# ---------------------------------------------------------------
# Convert text to Unicode mathematical bold characters.
# Renders as genuinely bold text via plain character substitution --
# no ggtext/markdown package or parsing required, so it can't
# silently fail to render like "**text**" can.
# ---------------------------------------------------------------
bold_unicode <- function(x) {
  upper       <- LETTERS
  lower       <- letters
  digits      <- as.character(0:9)
  bold_upper  <- vapply(0:25, function(i) intToUtf8(0x1D400 + i), character(1))
  bold_lower  <- vapply(0:25, function(i) intToUtf8(0x1D41A + i), character(1))
  bold_digits <- vapply(0:9,  function(i) intToUtf8(0x1D7CE + i), character(1))
  map <- setNames(c(bold_upper, bold_lower, bold_digits), c(upper, lower, digits))
  
  vapply(x, function(s) {
    chars <- strsplit(s, "")[[1]]
    paste0(ifelse(chars %in% names(map), map[chars], chars), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

# Reads the combined file produced by 01_combine_sources.R (EMA, Germany,
# Ireland, CEP, GMDP). It is already restricted to critical ATC codes, so
# no critical.csv re-filter is needed here.
data <- read.csv(file.path(PROC_DIR, "manufacturer_registers_combined.csv"),
                 stringsAsFactors = FALSE) %>%
  keep_list()
cat("rows read from manufacturer_registers_combined.csv:", nrow(data), "\n")


# fix: translate German (and other non-English) country names to English
# BEFORE any counting -- otherwise "Deutschland" and "Germany" (or
# "Spanien" and "Spain") get counted as separate countries. (Same map
# used across the other report scripts, kept here for consistency.)
country_map <- c(
  "Argentinien" = "Argentina", "Australien" = "Australia", "Belgien" = "Belgium",
  "Brasilien" = "Brazil", "Bulgarien" = "Bulgaria", "Kroatien" = "Croatia",
  "Deutschland" = "Germany", "Finnland" = "Finland",
  "Frankreich" = "France", "Griechenland" = "Greece", "Indien" = "India",
  "Irland" = "Ireland", "Island" = "Iceland", "Italien" = "Italy", "Kanada" = "Canada",
  "Korea, Republik" = "South Korea", "Republic of Korea" = "South Korea",
  "Lettland" = "Latvia", "Litauen" = "Lithuania",
  "Mexiko" = "Mexico", "Niederlande" = "Netherlands", "The Netherlands" = "Netherlands",
  "Norwegen" = "Norway", "Polen" = "Poland",
  "Schweden" = "Sweden", "Schweiz" = "Switzerland", "Singapur" = "Singapore",
  "Slowakei" = "Slovakia", "Slowenien" = "Slovenia",
  "Tschechische Republik" = "Czech Republic",
  "UK" = "United Kingdom", "USA" = "United States", "Ungarn" = "Hungary",
  "Vereinigte Staaten" = "United States",
  "Zypern" = "Cyprus",
  # added: missing German names + common English variants/abbreviations
  "Spanien" = "Spain", "Luxemburg" = "Luxembourg", "Estland" = "Estonia",
  "Volksrepublik China" = "China", "Tschechien" = "Czech Republic",
  "Czechia" = "Czech Republic", "Korea" = "South Korea", "Korea, Republic of" = "South Korea",
  "Great Britain" = "United Kingdom", "Britain" = "United Kingdom", "GB" = "United Kingdom",
  "England" = "United Kingdom", "Scotland" = "United Kingdom", "Wales" = "United Kingdom",
  "Northern Ireland" = "United Kingdom",
  "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
  "US" = "United States", "United States of America" = "United States",
  "People's Republic of China" = "China", "PR China" = "China", "P.R. China" = "China",
  "Republic of Ireland" = "Ireland", "Holland" = "Netherlands",
  "Turkiye" = "Turkey", "Russian Federation" = "Russia",
  setNames(
    c("Denmark", "South Korea", "Romania", "South Africa", "Turkey", "United Kingdom",
      "United Kingdom", "Austria", "Turkey"),
    c("D\u00e4nemark", "S\u00fcdkorea", "Rum\u00e4nien", "S\u00fcdafrika", "T\u00fcrkei",
      "Vereinigtes K\u00f6nigreich", "Vereinigtes K\u00f6nigreich (Nordirland)", "\u00d6sterreich",
      "T\u00fcrkiye"))
)

# ---------------------------------------------------------------
# Standardise country names so spelling/formatting variants collapse
# into one row:  "Italy." -> "Italy", "SPAIN" -> "Spain",
# "United Kingdom." / "UK" / "U.K." -> "United Kingdom", etc.
#   1. trim + collapse whitespace, drop trailing punctuation
#   2. look up the alias map case-insensitively, ignoring dots
#   3. ALL-CAPS names -> Title Case
#   4. snap to an already-known canonical spelling, case-insensitively
# ---------------------------------------------------------------
standardise_country <- function(x) {
  x   <- str_squish(x)
  x   <- str_squish(str_remove(x, "[[:punct:][:space:]]+$"))   # "Italy." -> "Italy"
  key <- tolower(str_remove_all(x, "\\."))                      # "U.K."   -> "uk"
  
  alias_map <- setNames(unname(country_map), tolower(str_remove_all(names(country_map), "\\.")))
  out <- ifelse(key %in% names(alias_map), alias_map[key], x)
  
  # ALL-CAPS (more than 3 letters, so leftover codes aren't mangled) -> Title Case
  is_caps <- !is.na(out) & out == toupper(out) & nchar(out) > 3
  out[is_caps] <- str_to_title(out[is_caps])
  out <- str_replace_all(out, "\\b(And|Of|The)\\b(?!$)", tolower)  # "Bosnia and Herzegovina"
  
  # snap to known canonical spelling (case-insensitive)
  canon <- unique(c(unname(country_map), EU_EEA_canon))
  hit   <- match(tolower(out), tolower(canon))
  out[!is.na(hit)] <- canon[hit[!is.na(hit)]]
  out
}

# canonical names used for case-insensitive snapping (EU/EEA list below + common non-EU)
EU_EEA_canon <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark",
                  "Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy",
                  "Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal",
                  "Romania","Slovakia","Slovenia","Spain","Sweden","Iceland","Norway","Liechtenstein",
                  "China","India","Japan","Israel","Taiwan","Chile","Monaco","Puerto Rico",
                  "Ukraine","Switzerland","United States","United Kingdom","South Korea")

# Split leftover "A | B" Ireland rows only where company and country lists
# have the same length; otherwise keep the row unsplit (separate_rows would
# silently recycle the shorter list and mispair companies with countries).
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

data <- bind_rows(
  data %>%
    filter(.lengths_match) %>%
    select(-mfr_company, -mfr_country, -.lengths_match) %>%
    unnest(cols = c(.company_list, .country_list)) %>%
    rename(mfr_company = .company_list, mfr_country = .country_list),
  data %>%
    filter(!.lengths_match) %>%
    select(-.company_list, -.country_list, -.lengths_match)
)

# lookups use plain named-vector indexing instead of recode(), which is
# masked by car::recode (and others) when those packages are loaded
source_map <- c("ema" = "EPAR", "germany" = "Germany", "gmdp" = "GMDP",
                "cep" = "CEP", "ireland" = "Ireland")

data <- data %>%
  mutate(
    mfr_company = str_trim(mfr_company),
    mfr_country = standardise_country(mfr_country),
    source = str_to_lower(str_trim(source)),
    source = coalesce(unname(source_map[source]), source)
  ) %>%
  filter(!is.na(atc_code), atc_code != "",
         !is.na(mfr_company), mfr_company != "",
         !is.na(mfr_country), mfr_country != "")

# sanity check: every distinct country name after cleaning -- scan this
# list for any remaining duplicates and add them to country_map above
print(sort(unique(data$mfr_country)))

# ATC anatomical main group (first letter) -> full name
atc_group_names <- c(
  A = "Alimentary tract\n& metabolism", B = "Blood & blood\nforming organs",
  C = "Cardiovascular\nsystem", D = "Dermatologicals",
  G = "Genito urinary\n& sex hormones", H = "Systemic hormonal\npreparations",
  J = "Antiinfectives", L = "Antineoplastic &\nimmunomodulating",
  M = "Musculo-skeletal\nsystem", N = "Nervous system",
  P = "Antiparasitic\nproducts", R = "Respiratory\nsystem",
  S = "Sensory organs", V = "Various"
)

data <- data %>%
  mutate(atc_group = substr(atc_code, 1, 1),
         atc_group_label = unname(atc_group_names[atc_group]))

# EU/EEA countries shown as plain row labels; everyone else bold (non-EU dependency)
EU_EEA <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark",
            "Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy",
            "Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal",
            "Romania","Slovakia","Slovenia","Spain","Sweden","Iceland","Norway","Liechtenstein")

# ---------------------------------------------------------------
# Build the country x group summary for a set of sources.
# Country rows are ordered by total unique sites across THOSE sources
# only, so Ireland's countries don't add empty rows to the main plots
# (the y axis uses drop = FALSE, so every country in the set is shown).
# ---------------------------------------------------------------
group_order <- atc_group_names[c("A","B","C","D","G","H","J","L","M","N","P","R","S","V")]

build_matrix <- function(df, sources) {
  m <- df %>%
    filter(source %in% sources) %>%
    distinct(source, atc_code, mfr_company, mfr_country, atc_group_label) %>%
    group_by(source, mfr_country, atc_group_label) %>%
    summarise(
      unique_sites = n_distinct(mfr_company),
      unique_atc   = n_distinct(atc_code),
      .groups = "drop"
    )

  country_order <- m %>%
    group_by(mfr_country) %>%
    summarise(total_sites = sum(unique_sites), .groups = "drop") %>%
    arrange(desc(total_sites)) %>%
    pull(mfr_country)

  m %>%
    mutate(
      # bold non-EU/EEA country names via real Unicode bold characters (see bold_unicode() above)
      mfr_country_label = ifelse(mfr_country %in% EU_EEA, mfr_country, bold_unicode(mfr_country)),
      mfr_country_label = factor(mfr_country_label,
                                 levels = rev(ifelse(country_order %in% EU_EEA, country_order,
                                                     bold_unicode(country_order)))),
      source = factor(source, levels = sources),
      atc_group_label = factor(atc_group_label, levels = group_order)
    )
}

main_sources <- c("Germany", "EPAR", "GMDP", "CEP")
matrix_main    <- build_matrix(data, main_sources)
matrix_ireland <- build_matrix(data, "Ireland")

# ---------------------------------------------------------------
# Plot -- one independent plot per source, each with its own
# size/color scale and its own legend (not shared across sources),
# colored using the established per-source palette:
#   Germany = amber, EPAR = teal, GMDP = purple, CEP = navy, Ireland = coral
# ---------------------------------------------------------------
source_pal <- c(Germany = "#B9861A", EPAR = "#1D6F5C", GMDP = "#6B4C9A",
                CEP = "#1A4D7A", Ireland = "#C1461D")

make_bubble_plot <- function(df, source_name, base_color, show_y_labels = TRUE) {

  sub <- df %>% filter(source == source_name)

  p <- ggplot(sub, aes(x = atc_group_label, y = mfr_country_label)) +
    geom_point(aes(size = unique_sites, fill = unique_atc),
               shape = 21, color = base_color, stroke = 0.8, alpha = 1) +
    scale_size_continuous(name = "Unique sites", range = c(1, 10),
                          breaks = function(x) unique(round(scales::extended_breaks()(x))),
                          labels = scales::label_number(accuracy = 1)) +
    scale_fill_gradient(low = "#F0EDE3", high = base_color, name = "Unique ATC\ncodes",
                        breaks = function(x) unique(round(scales::extended_breaks()(x))),
                        labels = scales::label_number(accuracy = 1)) +
    scale_y_discrete(drop = FALSE,
                     expand = expansion(add = 1.2)) +  # extra room top & bottom so the
    # biggest bubbles (top row) don't clip
    guides(size = guide_legend(override.aes = list(fill = "grey50", color = "grey50"))) +  # grey size-legend dots
    labs(x = NULL, y = NULL, title = source_name) +
    coord_cartesian(clip = "off") +   # let bubbles overflow the panel edge instead of being cut
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 40, hjust = 1, size = 11),
      axis.text.y = if (show_y_labels) element_text(size = 12) else element_blank(),
      panel.grid.major = element_line(color = "grey92"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      legend.position = "top",
      legend.box = "horizontal",
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      legend.key.size = unit(0.7, "cm"),
      legend.margin = margin(b = -8, t = 0, l = 0, r = 0),
      legend.box.margin = margin(b = -8),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
    )

  p
}

p_germany <- make_bubble_plot(matrix_main, "Germany", source_pal["Germany"])
p_ema     <- make_bubble_plot(matrix_main, "EPAR",    source_pal["EPAR"])
p_gmdp    <- make_bubble_plot(matrix_main, "GMDP",    source_pal["GMDP"])
p_cep     <- make_bubble_plot(matrix_main, "CEP",     source_pal["CEP"])

# ---- save each plot as its own separate file ----
ggsave(out_file("bubble_matrix_Germany.png"), p_germany, width = 8, height = 11, dpi = 300)
ggsave(out_file("bubble_matrix_EPAR.png"),    p_ema,     width = 8, height = 11, dpi = 300)
ggsave(out_file("bubble_matrix_GMDP.png"),    p_gmdp,    width = 8, height = 11, dpi = 300)
ggsave(out_file("bubble_matrix_CEP.png"),     p_cep,     width = 8, height = 11, dpi = 300)

# ---- combined 2x2 grid: Germany | EPAR on top, GMDP | CEP below ----
# no plot_layout(guides = "collect") here -- each panel keeps its own
# independent legend since the scales genuinely differ.
p <- (p_germany | p_ema) / (p_gmdp | p_cep)

ggsave(out_file("bubble_matrix_by_source.png"), p, width = 18, height = 20, dpi = 300)
print(p)

# ---------------------------------------------------------------
# Ireland -- separate plot, own country rows, not in the grid above
# ---------------------------------------------------------------
p_ireland <- make_bubble_plot(matrix_ireland, "Ireland", source_pal["Ireland"])
ggsave(out_file("bubble_matrix_Ireland.png"), p_ireland, width = 8, height = 11, dpi = 300)
print(p_ireland)
