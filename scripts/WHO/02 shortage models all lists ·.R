## =================================================================
## Shortage ~ manufacturer concentration, for three critical-medicine lists
##   WHO EML  |  EU Union list  |  intersection (on both lists)
## and three manufacturer databases
##   V1: Germany  |  V2: Germany + EPAR  |  V3: Germany + CEP + GMP API sites
##
## Runs everything from the input files (no saved results needed) and writes
##   shortage_analysis_all_codes.csv    one row per ATC code, metrics + list flags
##   shortage_models_all_lists.csv      all model numbers (OR, CI, p, N, AUC, R2)
##   shortage_models_all_lists.tex      the combined LaTeX table
##
## Needs manufacturer_registers_combined_WHO_EU.csv and who_eml_critical_codes.csv
## from 01_combine_sources_WHO.R (run that first).
## =================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)
## pROC is used via pROC::, so it only needs to be installed

## ---- paths (relative to the working directory) ---------------------
path_mfr       <- "processed/manufacturer_registers_combined_WHO_EU.csv"
path_who_codes <- "processed/who_eml_critical_codes.csv"
path_eu_list   <- "raw/critical.csv"                       # optional
path_gmp_api   <- "raw/gmdp_EML_API_sites_activities_LONG.csv"
path_shortages <- "raw/LEMeldungen_2026-09-09-13-30-43.csv"

out_analysis   <- "shortage_analysis_all_codes.csv"
out_csv        <- "shortage_models_all_lists.csv"
out_tex        <- "shortage_models_all_lists.tex"

## ---- settings (same as the main analysis) --------------------------
reference_date       <- as_date("2026-09-09")   # "currently active" shortage on this date
gmp_production_only  <- TRUE                    # GMP sections 3.1-3.4 and 3.5.1 only
gmp_production_codes <- "^3\\.(1|2|3|4)\\.|^3\\.5\\.1$"
include_model_rows   <- TRUE                    # N and AUC/R2 rows in the LaTeX table

## =================================================================
## 1. Manufacturer data and the three code lists
## =================================================================
mfr <- read_csv(path_mfr, show_col_types = FALSE) |>
  mutate(mfr_company = str_squish(mfr_company),
         mfr_country = str_squish(mfr_country))

who_codes <- read_csv(path_who_codes, show_col_types = FALSE)$atc_code

gmp_raw <- read_csv(path_gmp_api, show_col_types = FALSE)

## EU list = critical.csv (if present) + codes flagged as EU in the combined file
##           + codes flagged is_critical (= EU list) in the GMDP file
eu_codes <- unique(c(
  if (file.exists(path_eu_list)) read.csv(path_eu_list, stringsAsFactors = FALSE)$ATC.level.5,
  mfr$atc_code[mfr$on_eu_list],
  unlist(str_split(gmp_raw$atc[gmp_raw$is_critical %in% TRUE], "\\s*;\\s*"))
))
eu_codes <- eu_codes[!is.na(eu_codes) & eu_codes != ""]

cat("WHO EML codes:", length(who_codes), "| EU list codes:", length(eu_codes),
    "| on both:", length(intersect(who_codes, eu_codes)), "\n")

## =================================================================
## 2. EudraGMDP API sites (for V3)
## =================================================================
gmp_api <- gmp_raw |>
  filter(!withdrawn, cert_type == "API", !is.na(atc), atc != "") |>
  { \(d) if (gmp_production_only)
    filter(d, str_detect(activity_code, gmp_production_codes)) else d }() |>
  separate_rows(atc, sep = "\\s*;\\s*") |>
  transmute(
    atc_code          = str_squish(atc),
    mfr_company       = str_squish(site_name),
    mfr_country       = str_squish(country),
    manufacturer_step = 1,
    source            = "gmp_api"
  ) |>
  filter(atc_code %in% union(who_codes, eu_codes)) |>
  mutate(mfr_country = dplyr::recode(mfr_country,
                                     "Czechia"                           = "Czech Republic",
                                     "Korea, Republic of"                = "South Korea",
                                     "United Kingdom (Northern Ireland)" = "United Kingdom")) |>
  distinct()

cat("GMP API site rows:", nrow(gmp_api), "| codes:", n_distinct(gmp_api$atc_code), "\n")

## =================================================================
## 3. BfArM shortage register -> currently active shortage per ATC code
## =================================================================
clean_names_simple <- function(df) {
  nm <- names(df)
  nm <- str_replace_all(nm, c("ä" = "ae", "ö" = "oe", "ü" = "ue",
                              "Ä" = "ae", "Ö" = "oe", "Ü" = "ue", "ß" = "ss"))
  nm <- str_to_lower(nm)
  nm <- str_replace_all(nm, "[^a-z0-9]+", "_")
  nm <- str_remove_all(nm, "^_+|_+$")
  names(df) <- make.unique(nm, sep = "_")
  df
}

parse_de_date <- function(x) dmy(na_if(x, "N/A"))

shortage_status <- read_delim(path_shortages, delim = ";",
                              locale = locale(encoding = "ISO-8859-1", decimal_mark = ","),
                              show_col_types = FALSE) |>
  clean_names_simple() |>
  mutate(beginn = parse_de_date(beginn), ende = parse_de_date(ende)) |>
  filter(!is.na(atc_code), atc_code != "") |>
  separate_rows(atc_code, sep = ",\\s*") |>
  mutate(atc_code = str_trim(atc_code)) |>
  filter(nchar(atc_code) == 7) |>
  group_by(atc_code) |>
  summarise(
    currently_active = any(beginn <= reference_date & (is.na(ende) | ende >= reference_date)),
    n_reports        = n(),
    .groups = "drop"
  )

cat("ATC codes with >=1 shortage report:", nrow(shortage_status), "\n")

## =================================================================
## 4. Concentration metrics per ATC code (same definitions as the main analysis)
## =================================================================
apply_step1_priority <- function(df) {
  df |>
    group_by(atc_code) |>
    mutate(has_step1 = any(manufacturer_step == 1, na.rm = TRUE)) |>
    filter(!has_step1 | (has_step1 & manufacturer_step == 1)) |>
    ungroup() |>
    select(-has_step1)
}

legal_forms <- c("ltd", "limited", "pvt", "private", "plc", "llc", "inc", "corp",
                 "corporation", "co", "company", "gmbh", "mbh", "ag", "kg", "ohg",
                 "se", "sa", "sas", "sarl", "spa", "srl", "sl", "slu", "bv", "nv",
                 "as", "aps", "ab", "oy", "oyj", "kft", "zrt", "sro", "doo", "dd",
                 "sp", "zoo", "the", "s", "a", "p", "r", "l", "o", "z", "b", "v")

normalize_company <- function(x) {
  key <- x |>
    str_remove_all("\\(BS\\s*\\d+\\)") |>
    str_remove("\\s+[A-Z]{2}$") |>
    iconv(to = "ASCII//TRANSLIT", sub = "") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", " ") |>
    str_squish() |>
    str_split(" ") |>
    vapply(\(w) paste(head(setdiff(w, legal_forms), 2), collapse = " "), "")
  if_else(key == "", str_to_lower(str_squish(x)), key)
}

compute_metrics <- function(df, china_india = c("China", "India"),
                            step1_priority = TRUE, harmonise_names = FALSE) {
  if (step1_priority) df <- apply_step1_priority(df)
  if (harmonise_names) df <- mutate(df, mfr_company = paste(normalize_company(mfr_company),
                                                            mfr_country, sep = " @ "))
  df |>
    distinct(atc_code, mfr_company, mfr_country) |>
    group_by(atc_code) |>
    summarise(
      n_manufacturers = n_distinct(mfr_company, na.rm = TRUE),
      pct_china_india = sum(mfr_country %in% china_india, na.rm = TRUE) / n(),
      hhi             = {
        shares <- table(mfr_country[!is.na(mfr_country)]) / sum(!is.na(mfr_country))
        sum(shares^2) * 10000
      },
      .groups = "drop"
    )
}

metrics_v1 <- compute_metrics(filter(mfr, source == "germany")) |>
  rename_with(~ paste0(.x, "_v1"), -atc_code)

metrics_v2 <- compute_metrics(filter(mfr, source %in% c("germany", "ema"))) |>
  rename_with(~ paste0(.x, "_v2"), -atc_code)

v3_input <- bind_rows(
  mfr |>
    filter(source %in% c("germany", "cep")) |>
    mutate(manufacturer_step = if_else(source == "cep", 1, manufacturer_step)),
  gmp_api
)
metrics_v3 <- compute_metrics(v3_input, harmonise_names = TRUE) |>
  rename_with(~ paste0(.x, "_v3"), -atc_code)

analysis <- tibble(atc_code = union(unique(mfr$atc_code), unique(gmp_api$atc_code))) |>
  left_join(metrics_v1, by = "atc_code") |>
  left_join(metrics_v2, by = "atc_code") |>
  left_join(metrics_v3, by = "atc_code") |>
  left_join(shortage_status, by = "atc_code") |>
  mutate(
    currently_active = replace_na(currently_active, FALSE),
    n_reports        = replace_na(n_reports, 0L),
    on_who_eml       = atc_code %in% who_codes,
    on_eu_list       = atc_code %in% eu_codes
  )

write_csv(analysis, out_analysis)

## =================================================================
## 5. Fit the model for every list x version
## =================================================================
lists <- list(
  "WHO EML"        = analysis |> filter(on_who_eml),
  "EU Union list"  = analysis |> filter(on_eu_list),
  "WHO $\\cap$ EU" = analysis |> filter(on_who_eml, on_eu_list)
)

fit_one <- function(d, v) {
  d <- d |>
    filter(!is.na(.data[[paste0("n_manufacturers_", v)]])) |>
    transmute(
      currently_active,
      log_n_manufacturers = log1p(.data[[paste0("n_manufacturers_", v)]]),
      hhi_scaled          = .data[[paste0("hhi_", v)]] / 10000,
      pct_china_india     = .data[[paste0("pct_china_india_", v)]]
    )

  fit <- glm(currently_active ~ log_n_manufacturers + hhi_scaled + pct_china_india,
             data = d, family = binomial())

  ci  <- suppressMessages(confint(fit))   # profile-likelihood CIs
  lrt <- drop1(fit, test = "LRT")         # LR test per term (= Type II for this model)

  terms <- c("log_n_manufacturers", "hhi_scaled", "pct_china_india")
  tibble(
    term       = terms,
    or         = exp(coef(fit)[terms]),
    ci_low     = exp(ci[terms, 1]),
    ci_high    = exp(ci[terms, 2]),
    p          = lrt[terms, "Pr(>Chi)"],
    n_codes    = nrow(d),
    n_shortage = sum(d$currently_active),
    auc        = as.numeric(pROC::auc(pROC::roc(d$currently_active, fitted(fit), quiet = TRUE))),
    pseudo_r2  = 1 - fit$deviance / fit$null.deviance
  )
}

results <- expand_grid(list = names(lists), version = c("v1", "v2", "v3")) |>
  mutate(res = map2(list, version, \(l, v) fit_one(lists[[l]], v))) |>
  unnest(res) |>
  mutate(list = factor(list, levels = names(lists)),
         term = factor(term, levels = c("log_n_manufacturers", "hhi_scaled", "pct_china_india")))

write_csv(results |> mutate(list = str_replace(as.character(list), " \\$\\\\cap\\$ ", " & ")),
          out_csv)

## quick check in the console: codes and shortages per model
cat("\nCodes (active shortages) per list and version:\n")
results |>
  distinct(list, version, n_codes, n_shortage) |>
  mutate(cell = sprintf("%d (%d, %.1f%%)", n_codes, n_shortage, 100 * n_shortage / n_codes)) |>
  select(list, version, cell) |>
  pivot_wider(names_from = version, values_from = cell) |>
  print()

## =================================================================
## 6. LaTeX table
## =================================================================
fmt_num <- function(x) ifelse(x < 0.1, formatC(x, format = "f", digits = 3),
                              formatC(x, format = "f", digits = 2))
stars <- function(p) case_when(p < 0.001 ~ "^{***}", p < 0.01 ~ "^{**}",
                               p < 0.05 ~ "^{*}", TRUE ~ "")

term_labels <- c(log_n_manufacturers = "log(1 + N manufacturers)",
                 hhi_scaled          = "Country HHI (0--1)",
                 pct_china_india     = "Share China + India")

cells <- results |>
  mutate(cell = sprintf("$%s\\ (%s\\text{--}%s)%s$",
                        fmt_num(or), fmt_num(ci_low), fmt_num(ci_high), stars(p))) |>
  select(list, term, version, cell) |>
  pivot_wider(names_from = version, values_from = cell) |>
  arrange(list, term)

model_info <- results |>
  distinct(list, version, n_codes, n_shortage, auc, pseudo_r2) |>
  mutate(n_cell   = sprintf("%d (%d)", n_codes, n_shortage),
         fit_cell = sprintf("%.2f / %.2f", auc, pseudo_r2))

lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\small",
  "\\caption{Logistic regression of current shortage status on manufacturer concentration, by critical-medicines list and manufacturer database. Odds ratios with profile-likelihood 95\\% confidence intervals; stars from likelihood-ratio tests.}",
  "\\label{tab:shortage_models_lists}",
  "\\begin{tabular}{llll}",
  "\\toprule",
  " & V1: Germany & V2: Germany + EPAR & V3: Germany + CEP + GMP \\\\",
  "\\midrule"
)

for (l in levels(results$list)) {
  lines <- c(lines, sprintf("\\multicolumn{4}{l}{\\textbf{%s}} \\\\", l))
  blk <- filter(cells, list == l)
  for (i in seq_len(nrow(blk))) {
    lines <- c(lines, sprintf("\\quad %s & %s & %s & %s \\\\",
                              term_labels[as.character(blk$term[i])],
                              blk$v1[i], blk$v2[i], blk$v3[i]))
  }
  if (include_model_rows) {
    mi <- filter(model_info, list == l) |> arrange(version)
    lines <- c(lines,
               sprintf("\\quad \\textit{Codes (active shortages)} & %s \\\\",
                       paste(mi$n_cell, collapse = " & ")),
               sprintf("\\quad \\textit{AUC / McFadden $R^2$} & %s \\\\",
                       paste(mi$fit_cell, collapse = " & ")))
  }
  if (l != tail(levels(results$list), 1)) lines <- c(lines, "\\addlinespace")
}

lines <- c(lines,
  "\\bottomrule",
  "\\multicolumn{4}{l}{\\footnotesize $^{*}p<0.05$, $^{**}p<0.01$, $^{***}p<0.001$.} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(lines, out_tex)
cat("\n"); cat(lines, sep = "\n")
cat("\nWritten:", out_analysis, "|", out_csv, "|", out_tex, "\n")