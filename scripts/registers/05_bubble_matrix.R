source("scripts/registers/paths.R")

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(patchwork)
library(scales)

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

data <- read.csv(file.path(PROC_DIR, "manufacturer_registers_combined.csv"), stringsAsFactors = FALSE)
nrow(data)

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
  separate_rows(mfr_company, mfr_country, sep = "\\s*\\|\\s*") %>%
  mutate(
    mfr_company = str_trim(mfr_company),
    mfr_country = recode(str_trim(mfr_country), !!!country_map),
    source = str_to_title(str_trim(source)),
    source = recode(source, "Ema" = "EPAR", "Cep" = "CEP")
  ) %>%
  filter(!is.na(atc_code), atc_code != "",
         !is.na(mfr_company), mfr_company != "",
         !is.na(mfr_country), mfr_country != "")

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
         atc_group_label = recode(atc_group, !!!atc_group_names))

EU_EEA <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark",
            "Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy",
            "Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal",
            "Romania","Slovakia","Slovenia","Spain","Sweden","Iceland","Norway","Liechtenstein")

matrix_data <- data %>%
  distinct(source, atc_code, mfr_company, mfr_country, atc_group_label) %>%
  group_by(source, mfr_country, atc_group_label) %>%
  summarise(
    unique_sites = n_distinct(mfr_company),
    unique_atc   = n_distinct(atc_code),
    .groups = "drop"
  )

country_order <- matrix_data %>%
  group_by(mfr_country) %>%
  summarise(total_sites = sum(unique_sites)) %>%
  arrange(desc(total_sites)) %>%
  pull(mfr_country)

matrix_data <- matrix_data %>%
  mutate(
    mfr_country_label = ifelse(mfr_country %in% EU_EEA, mfr_country, bold_unicode(mfr_country)),
    mfr_country_label = factor(mfr_country_label,
                               levels = rev(ifelse(country_order %in% EU_EEA, country_order,
                                                   bold_unicode(country_order)))),
    source = factor(source, levels = c("EPAR", "Germany", "Ireland", "CEP"))
  )

group_order <- atc_group_names[c("A","B","C","D","G","H","J","L","M","N","P","R","S","V")]
matrix_data$atc_group_label <- factor(matrix_data$atc_group_label, levels = group_order)

source_pal <- c(EPAR = "#1D6F5C", Germany = "#B9861A", Ireland = "#C1461D", CEP = "#1A4D7A")

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
                     expand = expansion(add = 1.2)) +
    guides(size = guide_legend(override.aes = list(fill = "grey50", color = "grey50"))) +
    labs(x = NULL, y = NULL, title = source_name) +
    coord_cartesian(clip = "off") +
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

p_ema     <- make_bubble_plot(matrix_data, "EPAR",    source_pal["EPAR"],    show_y_labels = TRUE)
p_germany <- make_bubble_plot(matrix_data, "Germany", source_pal["Germany"], show_y_labels = TRUE)
p_ireland <- make_bubble_plot(matrix_data, "Ireland", source_pal["Ireland"], show_y_labels = TRUE)
p_cep     <- make_bubble_plot(matrix_data, "CEP",     source_pal["CEP"],     show_y_labels = TRUE)

ggsave(file.path(ADD_DIR, "bubble_matrix_EPAR.png"),     p_ema,     width = 8,  height = 11, dpi = 300)
ggsave(file.path(ADD_DIR, "bubble_matrix_Germany.png"), p_germany, width = 8,  height = 11, dpi = 300)
ggsave(file.path(ADD_DIR, "bubble_matrix_Ireland.png"), p_ireland, width = 8,  height = 11, dpi = 300)
ggsave(file.path(ADD_DIR, "bubble_matrix_CEP.png"),     p_cep,     width = 8,  height = 11, dpi = 300)

p <- (p_ema | p_germany) / (p_ireland | p_cep)

ggsave(file.path(FIG_DIR, "bubble_matrix_by_source.png"), p, width = 18, height = 20, dpi = 300)
print(p)
