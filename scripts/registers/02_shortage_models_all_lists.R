## =================================================================
## Shortage ~ manufacturer concentration, for three critical-medicine lists
##   WHO EML  |  EU Union list  |  intersection (on both lists)
## and three manufacturer databases
##   V1: Germany  |  V2: Germany + EPAR  |  V3: Germany + EPAR + CEP + GMP API sites
##
## Runs everything from the input files (no saved results needed) and writes
## to the results folder:
##   shortage_analysis_all_codes.csv    one row per ATC code, metrics + list flags
##   shortage_models_all_lists.csv      all model numbers (OR, CI, p, N, AUC, R2), main + regional
##   shortage_models_all_lists.tex      LaTeX table, main model
##   shortage_models_all_lists_regional.tex  LaTeX table, regional model (EU/EEA = reference)
##
## Needs data/registers/processed/manufacturer_registers_combined.csv from 01_combine_sources.R
## (run that first). List membership comes from its on_eu_list / on_who_eml
## flags and the GMDP API sites from its source == "gmdp" rows, so the
## numbers per list are the same as Shortage.R run with LIST set.
## =================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)
## pROC is used via pROC::, so it only needs to be installed

## ---- paths (relative to the repository root) ------
source("scripts/registers/paths.R", local = TRUE)
OUT_DIR        <- RESULTS_DIR
dir.create(OUT_DIR, showWarnings = FALSE)
path_mfr       <- file.path(PROC_DIR, "manufacturer_registers_combined.csv")
path_shortages <- file.path(SHORTAGE_DIR, "LEMeldungen_2026-09-09-13-30-43.csv")

out_analysis   <- file.path(OUT_DIR, "shortage_analysis_all_codes.csv")
out_csv        <- file.path(OUT_DIR, "shortage_models_all_lists.csv")
out_tex        <- file.path(OUT_DIR, "shortage_models_all_lists.tex")

## ---- settings (same as the main analysis) --------------------------
reference_date       <- as_date("2026-09-09")   # "currently active" shortage on this date
include_model_rows   <- TRUE                    # N and AUC/R2 rows in the LaTeX table
## (the GMDP production-sites-only rule is set in 01_combine_sources.R)

## =================================================================
## 1. Manufacturer data and the three code lists
## =================================================================
cat("Running", "02_shortage_models_all_lists.R", "-- version 2026-10-20: main + regional models (China, India,",
    "other Asia & Oceania, North America, non-EU Europe, rest of world)\n")

if (!"mfr_region" %in% names(readr::read_csv(path_mfr, n_max = 1, show_col_types = FALSE)))
  stop("manufacturer_registers_combined.csv has no mfr_region column. ",
       "Rerun the current 01_combine_sources.R first (or run_all_lists.R with ",
       "REBUILD_COMBINED_DATA <- TRUE).", call. = FALSE)

mfr <- read_csv(path_mfr, show_col_types = FALSE) |>
  mutate(mfr_company = str_squish(mfr_company),
         mfr_country = str_squish(mfr_country))

## list flags per ATC code (from 01_combine_sources.R)
code_flags <- mfr |> distinct(atc_code, on_eu_list, on_who_eml)

cat("Codes with manufacturer data -- WHO EML:", sum(code_flags$on_who_eml),
    "| EU list:", sum(code_flags$on_eu_list),
    "| on both:", sum(code_flags$on_who_eml & code_flags$on_eu_list), "\n")

## =================================================================
## 2. EudraGMDP API sites (for V3) = combined rows with source == "gmdp"
##    (production/processing sites only, step 1 -- see 01_combine_sources.R)
## =================================================================
gmp_api <- mfr |>
  filter(source == "gmdp") |>
  distinct(atc_code, mfr_company, mfr_country, manufacturer_step, source)

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

## country -> region, as assigned in 01_combine_sources.R (mfr_region)
region_of_country <- mfr |>
  filter(!is.na(mfr_country), !is.na(mfr_region)) |>
  distinct(mfr_country, mfr_region) |>
  { \(d) setNames(d$mfr_region, d$mfr_country) }()

compute_metrics <- function(df, china_india = c("China", "India"),
                            step1_priority = TRUE, harmonise_names = FALSE) {
  if (step1_priority) df <- apply_step1_priority(df)
  if (harmonise_names) df <- mutate(df, mfr_company = paste(normalize_company(mfr_company),
                                                            mfr_country, sep = " @ "))
  df |>
    distinct(atc_code, mfr_company, mfr_country) |>
    mutate(region = unname(region_of_country[mfr_country])) |>
    group_by(atc_code) |>
    summarise(
      n_manufacturers   = n_distinct(mfr_company, na.rm = TRUE),
      pct_china_india   = sum(mfr_country %in% china_india, na.rm = TRUE) / n(),
      ## regional shares (EU/EEA = reference in the regional model)
      pct_china         = sum(region %in% "China")                / n(),
      pct_india         = sum(region %in% "India")                / n(),
      pct_other_asia    = sum(region %in% "Other Asia & Oceania") / n(),
      pct_north_america = sum(region %in% "North America")        / n(),
      pct_non_eu_europe = sum(region %in% "Non-EU Europe")        / n(),
      pct_rest_of_world = sum(region %in% "Rest of world")        / n(),
      hhi               = {
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
    filter(source %in% c("germany", "ema", "cep")) |>
    mutate(manufacturer_step = if_else(source == "cep", 1, manufacturer_step)),
  gmp_api
)
metrics_v3 <- compute_metrics(v3_input, harmonise_names = TRUE) |>
  rename_with(~ paste0(.x, "_v3"), -atc_code)

analysis <- code_flags |>
  left_join(metrics_v1, by = "atc_code") |>
  left_join(metrics_v2, by = "atc_code") |>
  left_join(metrics_v3, by = "atc_code") |>
  left_join(shortage_status, by = "atc_code") |>
  mutate(
    currently_active = replace_na(currently_active, FALSE),
    n_reports        = replace_na(n_reports, 0L)
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

## two model specifications, each fitted for V1, V2, V3:
##   main     = log(1 + N manufacturers) + country HHI + share China + India
##   regional = log(1 + N manufacturers) + country HHI + share China, India,
##              other Asia & Oceania, North America, non-EU Europe, rest of
##              world (Latin America + Africa); EU/EEA = reference.
##              A region with no manufacturer in that list/version is left out.
base_terms     <- c("log_n_manufacturers", "hhi_scaled")
regional_terms <- c("pct_china", "pct_india", "pct_other_asia", "pct_north_america",
                    "pct_non_eu_europe", "pct_rest_of_world")
all_terms      <- c(base_terms, "pct_china_india", regional_terms)

fit_one <- function(d, v, model) {
  d <- d |>
    filter(!is.na(.data[[paste0("n_manufacturers_", v)]])) |>
    select(currently_active, ends_with(paste0("_", v))) |>
    rename_with(~ sub(paste0("_", v, "$"), "", .x)) |>
    mutate(log_n_manufacturers = log1p(n_manufacturers),
           hhi_scaled          = hhi / 10000)

  terms <- if (model == "main") c(base_terms, "pct_china_india") else
    c(base_terms, regional_terms[vapply(regional_terms, \(t) any(d[[t]] > 0), logical(1))])

  fit <- glm(reformulate(terms, response = "currently_active"), data = d, family = binomial())

  ci  <- tryCatch(suppressMessages(confint(fit)),   # profile-likelihood CIs
                  error = function(e) confint.default(fit))
  lrt <- drop1(fit, test = "LRT")                   # LR test per term (= Type II here)

  tibble(
    term       = terms,
    or         = exp(coef(fit)[terms]),
    ci_low     = exp(ci[terms, 1]),
    ci_high    = exp(ci[terms, 2]),
    p          = lrt[terms, "Pr(>Chi)"],
    n_codes    = nrow(d),
    n_shortage = sum(d$currently_active),
    events_per_variable = sum(d$currently_active) / length(terms),
    auc        = as.numeric(pROC::auc(pROC::roc(d$currently_active, fitted(fit), quiet = TRUE))),
    pseudo_r2  = 1 - fit$deviance / fit$null.deviance,
    ## (quasi-)separation, e.g. no code with a manufacturer in that region
    ## is in shortage: the odds ratio runs off towards 0 or infinity
    unstable   = is.na(ci_low) | is.na(ci_high) | or < 1e-3 | or > 1e3
  )
}

results <- expand_grid(model   = c("main", "regional"),
                       list    = names(lists),
                       version = c("v1", "v2", "v3")) |>
  mutate(res = pmap(list(list, version, model), \(l, v, m) fit_one(lists[[l]], v, m))) |>
  unnest(res) |>
  mutate(list = factor(list, levels = names(lists)),
         term = factor(term, levels = all_terms))

write_csv(results |> mutate(list = str_replace(as.character(list), " \\$\\\\cap\\$ ", " & ")),
          out_csv)

## quick check in the console: codes and shortages per model
cat("\nCodes (active shortages) per list and version:\n")
results |>
  filter(model == "main") |>
  distinct(list, version, n_codes, n_shortage) |>
  mutate(cell = sprintf("%d (%d, %.1f%%)", n_codes, n_shortage, 100 * n_shortage / n_codes)) |>
  select(list, version, cell) |>
  pivot_wider(names_from = version, values_from = cell) |>
  print()

## =================================================================
## 6. LaTeX tables: main model -> out_tex, regional model -> out_tex_regional
## =================================================================
out_tex_regional <- sub("\\.tex$", "_regional.tex", out_tex)

fmt_num <- function(x) ifelse(x < 0.1, formatC(x, format = "f", digits = 3),
                              formatC(x, format = "f", digits = 2))
stars <- function(p) case_when(p < 0.001 ~ "^{***}", p < 0.01 ~ "^{**}",
                               p < 0.05 ~ "^{*}", TRUE ~ "")

term_labels <- c(log_n_manufacturers = "log(1 + N manufacturers)",
                 hhi_scaled          = "Country HHI (0--1)",
                 pct_china_india     = "Share China + India",
                 pct_china           = "Share China",
                 pct_india           = "Share India",
                 pct_other_asia      = "Share other Asia \\& Oceania",
                 pct_north_america   = "Share North America",
                 pct_non_eu_europe   = "Share non-EU Europe",
                 pct_rest_of_world   = "Share rest of world")

latex_table <- function(res, caption, label, footnote) {
  cells <- res |>
    mutate(cell = if_else(unstable, "n.e.$^{\\dagger}$",
                          sprintf("$%s\\ (%s\\text{--}%s)%s$",
                                  fmt_num(or), fmt_num(ci_low), fmt_num(ci_high), stars(p)))) |>
    select(list, term, version, cell) |>
    pivot_wider(names_from = version, values_from = cell, values_fill = "--") |>
    arrange(list, term)

  model_info <- res |>
    distinct(list, version, n_codes, n_shortage, auc, pseudo_r2) |>
    mutate(n_cell   = sprintf("%d (%d)", n_codes, n_shortage),
           fit_cell = sprintf("%.2f / %.2f", auc, pseudo_r2))

  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\small",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{%s}", label),
    "\\begin{tabular}{llll}",
    "\\toprule",
    " & V1: Germany & V2: Germany + EPAR & V3: Germany + EPAR + CEP + GMP \\\\",
    "\\midrule"
  )
  for (l in levels(res$list)) {
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
    if (l != tail(levels(res$list), 1)) lines <- c(lines, "\\addlinespace")
  }
  c(lines,
    "\\bottomrule",
    sprintf("\\multicolumn{4}{l}{\\footnotesize %s} \\\\", footnote),
    "\\end{tabular}",
    "\\end{table}")
}

star_note <- "$^{*}p<0.05$, $^{**}p<0.01$, $^{***}p<0.001$."

lines_main <- latex_table(
  filter(results, model == "main") |> droplevels(),
  "Logistic regression of current shortage status on manufacturer concentration, by critical-medicines list and manufacturer database. Odds ratios with profile-likelihood 95\\% confidence intervals; stars from likelihood-ratio tests.",
  "tab:shortage_models_lists", star_note)

lines_regional <- latex_table(
  filter(results, model == "regional") |> droplevels(),
  "Logistic regression of current shortage status on manufacturer concentration with the share of manufacturers by region (reference: EU/EEA), by critical-medicines list and manufacturer database. Odds ratios with profile-likelihood 95\\% confidence intervals; stars from likelihood-ratio tests.",
  "tab:shortage_models_lists_regional",
  paste(star_note, "$^{\\dagger}$n.e.: not estimable (no or almost no medicine with a manufacturer in this region is in shortage). --: no manufacturer in this region."))

writeLines(lines_main, out_tex)
writeLines(lines_regional, out_tex_regional)
cat("\n"); cat(lines_regional, sep = "\n")
cat("\nWritten:", out_analysis, "|", out_csv, "|", out_tex, "|", out_tex_regional, "\n")
