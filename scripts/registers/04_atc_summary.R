library(ggplot2)
source("scripts/registers/paths.R")

library(dplyr)
library(tidyr)
library(patchwork)
library(stringr)
library(readr)
library(purrr)

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

germany <- read.csv(file.path(RAW_DIR, "bfarm_api_origin_critical_rest_LONG.csv"))
str(germany)
germany <- germany %>%
  filter(role != "Zulassungsinhaber") %>%
  mutate(land = recode(str_trim(land), !!!country_map))

write.csv(germany, file.path(PROC_DIR, "germany_critical.csv"), row.names = F)

EPAR <- read.csv(file.path(RAW_DIR, "EMA_data_critical.csv")) %>%
  mutate(country = recode(str_trim(country), !!!country_map))

ireland <- read.csv(file.path(RAW_DIR, "ireland_critical_atc_review.csv"))
str(ireland)

ireland <- ireland %>%
  filter(!is.na(matched_critical_atc), matched_critical_atc != "") %>%
  mutate(atc_split = str_split(matched_critical_atc, ",\\s*")) %>%
  unnest(atc_split) %>%
  mutate(atc_code = str_trim(atc_split)) %>%
  select(-atc_split)

if (file.exists(file.path(RAW_DIR, "critical.csv"))) {
  critical <- read.csv(file.path(RAW_DIR, "critical.csv"))
  str(critical)
  critical_codes <- unique(critical$ATC.level.5)
  critical_codes <- critical_codes[!is.na(critical_codes) & critical_codes != ""]
  cat("Critical ATC codes loaded from critical.csv:", length(critical_codes), "\n")
} else {
  critical_codes <- unique(c(EPAR$atc_code, germany$atc_code, ireland$atc_code))
  critical_codes <- critical_codes[!is.na(critical_codes) & critical_codes != ""]
  cat("critical.csv not found in", RAW_DIR, "-- falling back to the derived union of",
      "EMA/Germany/Ireland's own ATC codes:", length(critical_codes), "\n")
}

split_trim <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  str_squish(str_split(x, "\\|")[[1]])
}

ireland <- ireland %>%
  mutate(
    .company_list  = map(mfr_company, split_trim),
    .country_list  = map(mfr_country, split_trim),
    .lengths_match = map2_lgl(.company_list, .country_list,
                              ~ length(.x) == length(.y) && length(.x) > 0)
  )

ireland_matched <- ireland %>%
  filter(.lengths_match) %>%
  select(-mfr_company, -mfr_country, -.lengths_match) %>%
  unnest(cols = c(.company_list, .country_list)) %>%
  rename(mfr_company = .company_list, mfr_country = .country_list)

ireland_fallback <- ireland %>%
  filter(!.lengths_match) %>%
  select(-.company_list, -.country_list, -.lengths_match)

n_fallback <- nrow(ireland_fallback)
if (n_fallback > 0) {
  cat(n_fallback, "Ireland rows kept unsplit (mfr_company/mfr_country '|'-list length mismatch) -- inspect manually if needed.\n")
}

ireland <- bind_rows(ireland_matched, ireland_fallback)

ireland <- ireland %>% filter(atc_code %in% critical_codes)
str(germany)

if (!exists("cep_atc")) {
  cep_atc <- read_csv(file.path(RAW_DIR, "EXPORT_WEB_CEP_with_ATC_drugbank.csv"), show_col_types = FALSE)
}

cep_atc <- janitor::clean_names(cep_atc)

if (!"holder_country" %in% names(cep_atc)) {
  cep_atc <- cep_atc %>%
    mutate(
      holder_country          = str_extract(certificate_cep_holder, "(?<=\\s)[A-Z]{2}$"),
      certificate_cep_holder  = str_trim(str_remove(certificate_cep_holder, "\\s[A-Z]{2}$"))
    )
}

iso2_to_name <- c(
  AT = "Austria", BE = "Belgium", BG = "Bulgaria", HR = "Croatia",
  CY = "Cyprus", CZ = "Czech Republic", DK = "Denmark", EE = "Estonia",
  FI = "Finland", FR = "France", DE = "Germany", GR = "Greece",
  HU = "Hungary", IE = "Ireland", IT = "Italy", LV = "Latvia",
  LT = "Lithuania", LU = "Luxembourg", MT = "Malta", NL = "Netherlands",
  PL = "Poland", PT = "Portugal", RO = "Romania", SK = "Slovakia",
  SI = "Slovenia", ES = "Spain", SE = "Sweden", IS = "Iceland",
  LI = "Liechtenstein", NO = "Norway", GB = "United Kingdom",
  US = "United States", CN = "China", IN = "India", JP = "Japan",
  CH = "Switzerland", CA = "Canada", AU = "Australia", KR = "South Korea",
  BR = "Brazil", MX = "Mexico", IL = "Israel", TR = "Turkey",
  ZA = "South Africa", AR = "Argentina", NZ = "New Zealand",
  SG = "Singapore", TW = "Taiwan", RU = "Russia", UA = "Ukraine",
  AE = "United Arab Emirates", CO = "Colombia", HK = "Hong Kong",
  ID = "Indonesia", JO = "Jordan", MA = "Morocco", MC = "Monaco",
  MH = "Marshall Islands", MO = "Macao", MY = "Malaysia",
  OM = "Oman", PK = "Pakistan", PR = "Puerto Rico",
  SA = "Saudi Arabia", TH = "Thailand"
)

cep_full <- cep_atc %>%
  filter(!is.na(atc_code), atc_code != "") %>%
  separate_rows(atc_code, sep = ",\\s*") %>%
  filter(atc_code %in% critical_codes) %>%
  transmute(
    atc_code,
    medicine    = substance,
    manufacturer = str_trim(certificate_cep_holder),
    country      = unname(iso2_to_name[holder_country])
  )

n_missing_country <- sum(is.na(cep_full$country) & !is.na(cep_full$manufacturer))
if (n_missing_country > 0) {
  cat(n_missing_country, "CEP rows have an unmapped holder_country code -- add missing codes to iso2_to_name.\n")
}

germany_full <- germany
EPAR_full    <- EPAR
ireland_full <- ireland

EPAR    <- EPAR[,c("atc_code", "medicine_name", "manufacturer_name", "country")]
ireland <- ireland[,c("atc_code", "product_name", "mfr_company", "mfr_country")]
germany <- germany[,c("atc_code", "arzneimittel", "name", "land")]
cep     <- cep_full[,c("atc_code", "medicine", "manufacturer", "country")]

names(EPAR)    <- c("atc_code", "medicine", "mfr_company", "mfr_country")
names(ireland) <- c("atc_code", "medicine", "mfr_company", "mfr_country")
names(germany) <- c("atc_code", "medicine", "mfr_company", "mfr_country")
names(cep)     <- c("atc_code", "medicine", "mfr_company", "mfr_country")

EPAR$source    <- "EPAR"
ireland$source <- "Ireland"
germany$source <- "Germany"
cep$source     <- "CEP"

data <- rbind(EPAR, ireland, germany, cep)

pal <- c(Germany = "#B9861A", EPAR = "#1D6F5C", Ireland = "#C1461D", CEP = "#1A4D7A")

totals <- bind_rows(
  germany_full %>% transmute(source = "Germany", atc_code,
                             manufacturer = str_trim(name), country = str_trim(land)),
  EPAR_full %>% transmute(source = "EPAR", atc_code,
                          manufacturer = str_trim(manufacturer_name), country = str_trim(country)),
  ireland_full %>% transmute(source = "Ireland", atc_code,
                             manufacturer = str_trim(mfr_company), country = str_trim(mfr_country)),
  cep_full %>% transmute(source = "CEP", atc_code, manufacturer, country)
) %>%
  group_by(source) %>%
  summarise(
    `ATC codes`   = n_distinct(atc_code, na.rm = TRUE),
    Manufacturers = n_distinct(manufacturer[!is.na(manufacturer) & manufacturer != "" &
                                              !is.na(country) & country != ""]),
    Countries     = n_distinct(country[!is.na(country) & country != ""])
  ) %>%
  pivot_longer(-source, names_to = "metric", values_to = "value") %>%
  mutate(source = factor(source, levels = c("EPAR","Germany","Ireland","CEP")),
         metric = factor(metric, levels = c("ATC codes","Manufacturers","Countries")))

panelA <- ggplot(totals, aes(x = metric, y = value, fill = source)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = value), position = position_dodge(width = 0.75),
            vjust = -0.3, size = 3.2) +
  scale_y_log10() +
  scale_fill_manual(values = pal, name = NULL) +
  labs(x = NULL, y = "Count") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

per_code <- bind_rows(
  germany_full %>% transmute(source = "Germany", atc_code, mfr_company = str_trim(name), mfr_country = str_trim(land)),
  EPAR_full %>% transmute(source = "EPAR", atc_code, mfr_company = str_trim(manufacturer_name), mfr_country = str_trim(country)),
  ireland_full %>% transmute(source = "Ireland", atc_code, mfr_company = str_trim(mfr_company), mfr_country = str_trim(mfr_country)),
  cep_full %>% transmute(source = "CEP", atc_code, mfr_company = manufacturer, mfr_country = country)
) %>%
  distinct(source, atc_code, mfr_company, mfr_country) %>%
  group_by(source, atc_code) %>%
  summarise(
    n_manufacturers = n_distinct(mfr_company[!is.na(mfr_company) & mfr_company != "" &
                                               !is.na(mfr_country) & mfr_country != ""]),
    n_countries     = n_distinct(mfr_country[!is.na(mfr_country) & mfr_country != ""]),
    .groups = "drop"
  ) %>%
  mutate(source = factor(source, levels = c("EPAR","Germany","Ireland","CEP")))

panelB <- ggplot(per_code, aes(x = n_manufacturers, y = n_countries)) +
  geom_hex(bins = 15) +
  scale_fill_gradient(low = "#F4E1D6", high = "#7A2C10", name = "ATC codes") +
  facet_wrap(~ source, scales = "free_x", nrow = 1) +
  labs(x = "Distinct manufacturers", y = "Distinct countries") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

report_plot <- panelA / panelB + plot_layout(heights = c(1, 1.6))

ggsave(file.path(ADD_DIR, "atc_summary_report_plot.png"), report_plot, width = 15, height = 9, dpi = 300)
print(report_plot)

ggsave(file.path(FIG_DIR, "atc_summary_report_plot_v2.png"), panelB, width = 16, height = 5, dpi = 300,
       bg = "white")
