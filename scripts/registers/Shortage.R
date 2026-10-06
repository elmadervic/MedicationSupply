## =================================================================
## Does manufacturer concentration predict whether a critical medicine
## is currently on Germany's shortage list (BfArM "Lieferengpass"
## register)?
##
## Inputs (edit paths if needed):
##   - manufacturer_registers_combined.csv   (EPAR/Germany/Ireland/CEP,
##                                             critical ATC codes only)
##   - LEMeldungen_2026-09-09-13-30-43.csv   (BfArM shortage reports,
##                                             semicolon-delimited,
##                                             Latin-1 encoded)
##   - Critical_API_sites_activities_LONG.csv
##                                            (EudraGMDP API GMP
##                                             certificates, one row per
##                                             site x certificate x
##                                             activity, critical ATC
##                                             codes only)
##
## What this script does:
##   1. Builds a shortage flag per critical ATC code from the LE file.
##   2. Builds manufacturer-concentration metrics per ATC code, at
##      four levels: EPAR-only (source == "ema" -- this is the basis
##      for the paper's ">25 single-source" claim, "according to EU
##      authorizations"), V1 = Germany-only (source == "germany"),
##      V2 = Germany + EPAR combined/deduplicated, and
##      V3 = Germany + CEP + EudraGMDP API sites (name-harmonised so
##      the same firm listed in several registers counts once).
##   3. Cross-tabs single-source status against shortage status
##      (reproduces your "1 of 25" observation and tests whether it's
##      more than chance).
##   4. Runs univariate tests (Wilcoxon, point-biserial correlation)
##      and a multivariate logistic regression of shortage status on
##      number of manufacturers, country-level HHI, and share of
##      manufacturers in China/India.
##   5. Makes a few diagnostic plots.
## =================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(broom)
library(ggplot2)
library(scales)
library(car)    # vif()
library(pROC)   # roc()/auc()

## Minimal stand-in for janitor::clean_names() (avoids the extra
## dependency): lowercases, replaces non-alphanumeric runs with "_",
## trims leading/trailing "_".
clean_names_simple <- function(df) {
  nm <- names(df)
  nm <- str_to_lower(nm)
  nm <- str_replace_all(nm, "[^a-z0-9]+", "_")
  nm <- str_replace_all(nm, "^_|_$", "")
  names(df) <- nm
  df
}

## -----------------------------------------------------------------
## 0. Paths -- adjust if your files live elsewhere
## -----------------------------------------------------------------
path_mfr       <- "manufacturer_registers_combined.csv"
path_shortages <- "LEMeldungen_2026-09-09-13-30-43.csv"
path_gmp_api   <- "Critical_API_sites_activities_LONG_2.csv"

## -----------------------------------------------------------------
## 1. Load the manufacturer register (already critical-ATC-filtered)
## -----------------------------------------------------------------
mfr <- read_csv(path_mfr, show_col_types = FALSE) |>
  mutate(
    mfr_company = str_squish(mfr_company),
    mfr_country = str_squish(mfr_country)
  )

cat("Manufacturer register rows:", nrow(mfr), "\n")
cat("Unique critical ATC codes covered by ANY source:", n_distinct(mfr$atc_code), "\n")
print(table(mfr$source))

## -----------------------------------------------------------------
## 1b. Load the EudraGMDP API-site file (for V3)
##    - long format: one row per site x certificate x activity, so it
##      is collapsed to one row per ATC code x site below
##    - only sites that actually MAKE or PROCESS the substance are kept
##      (EU GMP API-certificate sections 3.1-3.4 = chemical synthesis,
##      extraction, biological/fermentation, sterile API; 3.5.1 =
##      physical processing, e.g. micronisation). Sites certified ONLY
##      for packaging (3.5.2-3.5.4) or testing (3.6.x) are dropped, as
##      they are contract service sites rather than manufacturers.
##      Set gmp_production_only <- FALSE to keep every certified site.
##    - all certificates in the file are non-withdrawn API certificates
##      for critical codes; the filters are kept anyway in case the file
##      is regenerated with a wider scope
##    - every row is an active-substance site, so it is tagged
##      manufacturer_step = 1 (see step-1 note in section 3)
##    - two country spellings differ from the Germany/CEP registers and
##      are harmonised so the China/India share and HHI line up
## -----------------------------------------------------------------
gmp_production_only <- TRUE
gmp_production_codes <- "^3\\.(1|2|3|4)\\.|^3\\.5\\.1$"

gmp_raw <- read_csv("Data/Critical_API_sites_activities_LONG.csv", show_col_types = FALSE)

gmp_api <- gmp_raw |>
  filter(is_critical, !withdrawn, cert_type == "API") |>
  { \(d) if (gmp_production_only)
    filter(d, str_detect(activity_code, gmp_production_codes)) else d }() |>
  transmute(
    atc_code          = str_squish(atc),
    mfr_company       = str_squish(site_name),
    mfr_country       = str_squish(country),
    manufacturer_step = 1,
    source            = "gmp_api"
  ) |>
  mutate(mfr_country = dplyr::recode(mfr_country,   # car::recode masks dplyr's
                                     "Czechia"            = "Czech Republic",
                                     "Korea, Republic of" = "South Korea")) |>
  distinct()

cat("\nEudraGMDP API file rows:", nrow(gmp_raw),
    "-> unique ATC x site rows kept for V3:", nrow(gmp_api),
    if (gmp_production_only) "(production/processing sites only)" else "(all sites)", "\n")
cat("Critical ATC codes with >=1 GMP API site:", n_distinct(gmp_api$atc_code), "\n")

## -----------------------------------------------------------------
## 2. Load the BfArM shortage register
##    - semicolon-delimited, Latin-1 encoded
##    - "Atc Code" can hold multiple comma-separated codes
##    - a few rows carry a level-4-only code (5 chars, e.g. "A02AH")
##      or a placeholder "TERM_ID_NA_..." with no ATC at all -- both
##      are dropped from the exact-match shortage flag below, since
##      they can't be pinned to one critical ATC5 code.
## -----------------------------------------------------------------
shortages_raw <- read_delim(
  path_shortages,
  delim = ";",
  locale = locale(encoding = "ISO-8859-1", decimal_mark = ","),
  show_col_types = FALSE
) |>
  clean_names_simple()

cat("\nShortage report rows:", nrow(shortages_raw), "\n")

## Parse dates (dd.mm.yyyy; "N/A" -> NA = open-ended shortage)
parse_de_date <- function(x) dmy(na_if(x, "N/A"))

shortages <- shortages_raw |>
  mutate(
    beginn = parse_de_date(beginn),
    ende   = parse_de_date(ende)
  )

## One row per individual ATC code (split multi-code cells)
shortage_long <- shortages |>
  filter(!is.na(atc_code), atc_code != "") |>
  separate_rows(atc_code, sep = ",\\s*") |>
  mutate(atc_code = str_trim(atc_code))

## Keep only proper 7-character ATC5 codes for the exact-match flag
n_level4  <- sum(nchar(shortage_long$atc_code) == 5, na.rm = TRUE)
n_no_atc  <- sum(str_detect(shortage_long$atc_code, "^TERM_ID_NA"), na.rm = TRUE)
cat("Dropping", n_level4, "level-4-only rows and", n_no_atc,
    "rows with no assigned ATC code from the exact-match flag.\n")

shortage_long <- shortage_long |>
  filter(nchar(atc_code) == 7)

## "Currently active" = start on/before today, and either open-ended
## or end on/after today. Change reference_date if you want the flag
## as of a different point in time (e.g. the file's own pull date).
reference_date <- as_date("2026-09-09")

shortage_status <- shortage_long |>
  group_by(atc_code) |>
  summarise(
    ever_reported   = TRUE,
    currently_active = any(beginn <= reference_date &
                             (is.na(ende) | ende >= reference_date)),
    n_reports       = n(),
    .groups = "drop"
  )

cat("Unique ATC5 codes with >=1 shortage report:", nrow(shortage_status), "\n")

## -----------------------------------------------------------------
## 3. Manufacturer-concentration metrics per ATC code
##    Computed at three levels:
##      - EPAR-only  (source == "ema")            -> used only for the
##                                                    "25 single-source"
##                                                    reproduction check
##                                                    in section 4
##      - V1: Germany-only (source == "germany")  -> main analysis input
##      - V2: Germany + EPAR combined/deduplicated -> main analysis input
##      - V3: Germany + CEP + EudraGMDP API sites  -> main analysis input
##    Ireland is not used anywhere; CEP is used only in V3.
## -----------------------------------------------------------------
## Step-1 (active-substance) priority filter, exactly as in the paper's
## Methods: for each ATC code, keep step-1 rows if any exist for that
## code, otherwise fall back to all disclosed rows (step-2 / NA-step).
## This matters for EPAR and Germany, which distinguish step 1/2;
## Ireland and CEP have no step field, so every row passes through
## unchanged for them.
apply_step1_priority <- function(df) {
  df |>
    group_by(atc_code) |>
    mutate(has_step1 = any(manufacturer_step == 1, na.rm = TRUE)) |>
    filter(!has_step1 | (has_step1 & manufacturer_step == 1)) |>
    ungroup() |>
    select(-has_step1)
}

## Company-name harmonisation, used for V3 only. The three V3 registers
## spell the same firm differently, e.g.
##   Germany: "Zhejiang Supor Pharmaceuticals Chemical Co., Ltd. (BS 1)"
##   CEP:     "ZHEJIANG SUPOR PHARMACEUTICAL CO., LTD. Shangyu CN"
##   GMP:     "Zhejiang Supor Pharmaceuticals Co. Ltd."
## so an exact-string n_distinct() would count one firm up to three
## times. The key is: drop "(BS n)" site suffixes and the CEP trailing
## 2-letter country code, transliterate to ASCII, lowercase, strip
## punctuation and legal-form words, then keep the first two remaining
## words ("zhejiang supor"), and pair it with the country. Deliberately
## coarse: it can merge two distinct same-country firms that share their
## first two words (e.g. "Chemische Fabrik Lehrte" / "Chemische Fabrik
## Berg"), so treat V3 company counts as a slight lower bound.
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
  if_else(key == "", str_to_lower(str_squish(x)), key)   # all-legal-form name: keep as is
}

compute_metrics <- function(df, china_india = c("China", "India"), step1_priority = FALSE,
                            harmonise_names = FALSE) {
  if (step1_priority) df <- apply_step1_priority(df)
  ## harmonised key = name key + country, so "Fresenius Kabi" in Austria
  ## and in Germany stay two manufacturers (as in V1/V2), while the same
  ## firm listed by Germany, CEP and GMP in one country collapses to one
  if (harmonise_names) df <- mutate(df, mfr_company = paste(normalize_company(mfr_company),
                                                            mfr_country, sep = " @ "))
  df |>
    distinct(atc_code, mfr_company, mfr_country) |>
    group_by(atc_code) |>
    summarise(
      n_manufacturers = n_distinct(mfr_company, na.rm = TRUE),
      n_countries     = n_distinct(mfr_country, na.rm = TRUE),
      n_china_india   = sum(mfr_country %in% china_india, na.rm = TRUE),
      pct_china_india = n_china_india / n(),
      hhi             = {
        shares <- table(mfr_country[!is.na(mfr_country)]) / sum(!is.na(mfr_country))
        sum(shares^2) * 10000
      },
      .groups = "drop"
    ) |>
    mutate(
      single_source  = n_manufacturers == 1,   # exactly one manufacturer/company
      single_country = hhi >= 9999              # all disclosed sites in one country
      # (this is the paper's own "single
      # source" definition -- likely what
      # your "25" figure used, not
      # single_source above)
    )
}

metrics_epar <- compute_metrics(filter(mfr, source == "ema"), step1_priority = TRUE) |>
  rename_with(~paste0(.x, "_epar"), -atc_code)

## V1: Germany only
metrics_v1 <- compute_metrics(filter(mfr, source == "germany"), step1_priority = TRUE) |>
  rename_with(~paste0(.x, "_v1"), -atc_code)

## V2: Germany + EPAR combined (step-1 priority applied within this
## Germany+EPAR subset, so a code with a step-1 EPAR or Germany
## manufacturer uses only step-1 rows; falls back to step-2/NA
## otherwise)
metrics_v2 <- compute_metrics(filter(mfr, source %in% c("germany", "ema")), step1_priority = TRUE) |>
  rename_with(~paste0(.x, "_v2"), -atc_code)

## V3: Germany + CEP + EudraGMDP API sites.
## CEP and GMP API rows are active-substance manufacturers by
## definition, but CEP has no step field (NA). Left as NA, the step-1
## filter would DROP every CEP row for any code where Germany lists a
## step-1 site -- so CEP is tagged step 1 here (GMP already is). Net
## effect: per code, V3 = Germany step-1 sites + all CEP holders + all
## GMP API sites (or Germany step-2/NA rows + CEP + GMP if Germany has
## no step-1 site for that code). Company names are harmonised across
## the three registers (see normalize_company()).
v3_input <- bind_rows(
  mfr |>
    filter(source %in% c("germany", "cep")) |>
    mutate(manufacturer_step = if_else(source == "cep", 1, manufacturer_step)),
  gmp_api
)

metrics_v3 <- compute_metrics(v3_input, step1_priority = TRUE, harmonise_names = TRUE) |>
  rename_with(~paste0(.x, "_v3"), -atc_code)

## How much the name harmonisation matters (raw strings vs. harmonised key)
v3_dedup_check <- compute_metrics(v3_input, step1_priority = TRUE, harmonise_names = FALSE) |>
  select(atc_code, n_raw = n_manufacturers) |>
  inner_join(select(metrics_v3, atc_code, n_harm = n_manufacturers_v3), by = "atc_code")
cat(sprintf(paste0("\nV3 name harmonisation: median manufacturers per code %.0f (raw strings)",
                   " -> %.0f (harmonised); %d of %d codes change.\n"),
            median(v3_dedup_check$n_raw), median(v3_dedup_check$n_harm),
            sum(v3_dedup_check$n_raw != v3_dedup_check$n_harm), nrow(v3_dedup_check)))
cat("V3 covers", nrow(metrics_v3), "critical ATC codes (V1:", nrow(metrics_v1),
    "| V2:", nrow(metrics_v2), ")\n")

## Full ATC universe = every critical code with at least one manufacturer
## record anywhere (combined register OR GMP API file), so codes with zero
## disclosed EPAR/DE manufacturers still show up (with NA metrics for that
## source) rather than being silently dropped.
all_codes <- tibble(atc_code = union(unique(mfr$atc_code), unique(gmp_api$atc_code)))

analysis <- all_codes |>
  left_join(metrics_epar,     by = "atc_code") |>
  left_join(metrics_v1,       by = "atc_code") |>
  left_join(metrics_v2,       by = "atc_code") |>
  left_join(metrics_v3,       by = "atc_code") |>
  left_join(shortage_status,  by = "atc_code") |>
  mutate(
    ever_reported    = replace_na(ever_reported, FALSE),
    currently_active = replace_na(currently_active, FALSE),
    n_reports        = replace_na(n_reports, 0L)
  )

cat("\nAnalysis table:", nrow(analysis), "critical ATC codes.\n")

## -----------------------------------------------------------------
## 4. Reproduce your "1 of 25" check
##    single_country (HHI = 10,000, i.e. all disclosed EPAR sites in
##    one country) is almost certainly the definition behind the
##    paper's "25 single-source" claim -- single_source (exactly one
##    manufacturer company) is a stricter, different thing and only
##    matches 12 EPAR codes, not ~25. Both are reported below so you
##    can see which one matches what you counted by hand.
## -----------------------------------------------------------------
cat("\n--- Single-COUNTRY (EPAR/EMA basis, HHI=10000) vs. currently-active shortage ---\n")
tab_single_country <- analysis |>
  filter(!is.na(single_country_epar)) |>
  count(single_country_epar, currently_active) |>
  pivot_wider(names_from = currently_active, values_from = n, values_fill = 0)
print(tab_single_country)

ft_country <- analysis |>
  filter(!is.na(single_country_epar)) |>
  { \(d) fisher.test(table(d$single_country_epar, d$currently_active)) }()
print(ft_country)

cat("\n--- Single-source COMPANY (EPAR/EMA basis, exactly 1 manufacturer) vs. currently-active shortage ---\n")
tab_single_source <- analysis |>
  filter(!is.na(single_source_epar)) |>
  count(single_source_epar, currently_active) |>
  pivot_wider(names_from = currently_active, values_from = n, values_fill = 0)
print(tab_single_source)

ft <- analysis |>
  filter(!is.na(single_source_epar)) |>
  { \(d) fisher.test(table(d$single_source_epar, d$currently_active)) }()
print(ft)

## Same check using V1 (Germany-only) manufacturer count, since the
## shortage register itself is German and Germany's register has far
## denser coverage (251 of 299 critical codes) than EPAR (51 codes).
cat("\n--- Single-source (V1: Germany only) vs. currently-active shortage ---\n")
tab_single_source_v1 <- analysis |>
  filter(!is.na(single_source_v1)) |>
  count(single_source_v1, currently_active) |>
  pivot_wider(names_from = currently_active, values_from = n, values_fill = 0)
print(tab_single_source_v1)

ft_v1 <- analysis |>
  filter(!is.na(single_source_v1)) |>
  { \(d) fisher.test(table(d$single_source_v1, d$currently_active)) }()
print(ft_v1)

## Same check on V3 (Germany + CEP + GMP API sites)
cat("\n--- Single-source (V3: Germany + CEP + GMP API) vs. currently-active shortage ---\n")
tab_single_source_v3 <- analysis |>
  filter(!is.na(single_source_v3)) |>
  count(single_source_v3, currently_active) |>
  pivot_wider(names_from = currently_active, values_from = n, values_fill = 0)
print(tab_single_source_v3)

ft_v3 <- analysis |>
  filter(!is.na(single_source_v3)) |>
  { \(d) fisher.test(table(d$single_source_v3, d$currently_active)) }()
print(ft_v3)

## -----------------------------------------------------------------
## 5. Univariate tests: does each metric differ between shortage and
##    non-shortage codes? Run on both V1 (Germany only) and V2
##    (Germany + EPAR combined).
## -----------------------------------------------------------------
univariate_test <- function(var, label) {
  d <- analysis |> filter(!is.na(.data[[var]]))
  wt <- wilcox.test(d[[var]] ~ d$currently_active)
  ct <- cor.test(d[[var]], as.numeric(d$currently_active))
  tibble(
    metric      = label,
    n           = nrow(d),
    median_no   = median(d[[var]][!d$currently_active], na.rm = TRUE),
    median_yes  = median(d[[var]][d$currently_active], na.rm = TRUE),
    wilcox_p    = wt$p.value,
    point_biserial_r = ct$estimate,
    cor_p       = ct$p.value
  )
}

univariate_results <- bind_rows(
  univariate_test("n_manufacturers_v1", "N manufacturers (V1: Germany)"),
  univariate_test("n_countries_v1",     "N countries (V1: Germany)"),
  univariate_test("pct_china_india_v1", "% China+India (V1: Germany)"),
  univariate_test("hhi_v1",             "Country HHI (V1: Germany)"),
  univariate_test("n_manufacturers_v2", "N manufacturers (V2: Germany+EPAR)"),
  univariate_test("n_countries_v2",     "N countries (V2: Germany+EPAR)"),
  univariate_test("pct_china_india_v2", "% China+India (V2: Germany+EPAR)"),
  univariate_test("hhi_v2",             "Country HHI (V2: Germany+EPAR)"),
  univariate_test("n_manufacturers_v3", "N manufacturers (V3: Germany+CEP+GMP)"),
  univariate_test("n_countries_v3",     "N countries (V3: Germany+CEP+GMP)"),
  univariate_test("pct_china_india_v3", "% China+India (V3: Germany+CEP+GMP)"),
  univariate_test("hhi_v3",             "Country HHI (V3: Germany+CEP+GMP)")
)

cat("\n--- Univariate association with currently-active shortage ---\n")
print(univariate_results, n = Inf)

## -----------------------------------------------------------------
## 6. Multivariate logistic regression
##    shortage ~ manufacturer count + concentration + China/India share
##    Log-transform manufacturer count (right-skewed); scale HHI to
##    0-1 for a readable coefficient. Fit separately on V1 and V2.
## -----------------------------------------------------------------
model_data_v1 <- analysis |>
  filter(!is.na(n_manufacturers_v1)) |>
  mutate(
    log_n_manufacturers = log1p(n_manufacturers_v1),
    hhi_scaled          = hhi_v1 / 10000,
    pct_china_india      = pct_china_india_v1
  )

fit_v1 <- glm(
  currently_active ~ log_n_manufacturers + hhi_scaled + pct_china_india,
  data   = model_data_v1,
  family = binomial()
)

cat("\n--- Logistic regression: shortage ~ manufacturer concentration (V1: Germany only) ---\n")
print(summary(fit_v1))

cat("\nOdds ratios:\n")
print(tidy(fit_v1, exponentiate = TRUE, conf.int = TRUE))

model_data_v2 <- analysis |>
  filter(!is.na(n_manufacturers_v2)) |>
  mutate(
    log_n_manufacturers = log1p(n_manufacturers_v2),
    hhi_scaled          = hhi_v2 / 10000,
    pct_china_india      = pct_china_india_v2
  )

fit_v2 <- glm(
  currently_active ~ log_n_manufacturers + hhi_scaled + pct_china_india,
  data   = model_data_v2,
  family = binomial()
)

cat("\n--- Logistic regression: shortage ~ manufacturer concentration (V2: Germany + EPAR) ---\n")
print(summary(fit_v2))
cat("\nOdds ratios:\n")
print(tidy(fit_v2, exponentiate = TRUE, conf.int = TRUE))

model_data_v3 <- analysis |>
  filter(!is.na(n_manufacturers_v3)) |>
  mutate(
    log_n_manufacturers = log1p(n_manufacturers_v3),
    hhi_scaled          = hhi_v3 / 10000,
    pct_china_india      = pct_china_india_v3
  )

fit_v3 <- glm(
  currently_active ~ log_n_manufacturers + hhi_scaled + pct_china_india,
  data   = model_data_v3,
  family = binomial()
)

cat("\n--- Logistic regression: shortage ~ manufacturer concentration (V3: Germany + CEP + GMP API) ---\n")
print(summary(fit_v3))
cat("\nOdds ratios:\n")
print(tidy(fit_v3, exponentiate = TRUE, conf.int = TRUE))

## -----------------------------------------------------------------
## 6b. More detailed model diagnostics for each fit
##    - profile-likelihood CIs on the odds ratios (more reliable in
##      small samples than the Wald CIs tidy() gives above)
##    - a likelihood-ratio test per term (drop-in-deviance test --
##      generally preferred over the Wald z-test in summary() for
##      small/medium n)
##    - McFadden's pseudo-R^2 (no single "R^2" exists for logistic
##      models; this is the most commonly reported analogue)
##    - VIFs, to check the three predictors aren't confounding each
##      other (values > ~5 would be a concern; not expected here since
##      they capture different aspects of concentration)
##    - in-sample AUC and a confusion matrix at a 0.5 cutoff, i.e. how
##      well the fitted model actually discriminates shortage vs. not
## -----------------------------------------------------------------
model_diagnostics <- function(fit, outcome, label) {
  cat("\n===", label, "===\n")
  
  cat("\nProfile-likelihood CIs (odds ratios):\n")
  print(exp(cbind(OR = coef(fit), confint(fit))))
  
  cat("\nLikelihood-ratio test per term (Type II, drop-in-deviance):\n")
  print(car::Anova(fit, type = "II", test.statistic = "LR"))
  
  mcfadden_r2 <- 1 - (fit$deviance / fit$null.deviance)
  cat(sprintf("\nMcFadden's pseudo-R^2: %.3f\n", mcfadden_r2))
  cat(sprintf("AIC: %.1f   BIC: %.1f   log-likelihood: %.1f\n",
              AIC(fit), BIC(fit), as.numeric(logLik(fit))))
  
  cat("\nVariance inflation factors (multicollinearity check):\n")
  print(car::vif(fit))
  
  pred_prob <- predict(fit, type = "response")
  roc_obj   <- pROC::roc(outcome, pred_prob, quiet = TRUE)
  cat(sprintf("\nIn-sample AUC: %.3f\n", as.numeric(pROC::auc(roc_obj))))
  
  cat("\nConfusion matrix at 0.5 cutoff:\n")
  pred_class <- factor(pred_prob > 0.5, levels = c(FALSE, TRUE))
  print(table(predicted = pred_class, actual = outcome))
  cat("(With shortages this rare (~", round(100 * mean(outcome), 1),
      "% of codes), the model may never cross 0.5 -- that's expected class\n",
      "imbalance, not a broken model. A cutoff at the base rate is more informative:\n", sep = "")
  
  base_rate <- mean(outcome)
  pred_class_br <- factor(pred_prob > base_rate, levels = c(FALSE, TRUE))
  cat(sprintf("Confusion matrix at %.3f (base-rate) cutoff:\n", base_rate))
  print(table(predicted = pred_class_br, actual = outcome))
  
  invisible(list(mcfadden_r2 = mcfadden_r2, auc = as.numeric(pROC::auc(roc_obj))))
}

model_diagnostics(fit_v1, model_data_v1$currently_active, "V1: Germany only")
model_diagnostics(fit_v2, model_data_v2$currently_active, "V2: Germany + EPAR")
model_diagnostics(fit_v3, model_data_v3$currently_active, "V3: Germany + CEP + GMP API")

## -----------------------------------------------------------------
## 7. Plots (V1: Germany only, as the primary/default view)
## -----------------------------------------------------------------
theme_set(theme_minimal(base_size = 12))

p1 <- analysis |>
  filter(!is.na(n_manufacturers_v1)) |>
  ggplot(aes(x = currently_active, y = n_manufacturers_v1)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_log10() +
  labs(x = "Currently on shortage list", y = "Distinct manufacturers (V1: Germany, log scale)",
       title = "Manufacturer count vs. shortage status")

p2 <- analysis |>
  filter(!is.na(hhi_v1)) |>
  ggplot(aes(x = currently_active, y = hhi_v1)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  labs(x = "Currently on shortage list", y = "Country-level HHI (V1: Germany)",
       title = "Supply concentration vs. shortage status")

p3 <- analysis |>
  filter(!is.na(pct_china_india_v1)) |>
  ggplot(aes(x = currently_active, y = pct_china_india_v1)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_continuous(labels = percent) +
  labs(x = "Currently on shortage list", y = "Share of manufacturers in China/India (V1: Germany)",
       title = "China+India dependence vs. shortage status")

p4 <- model_data_v1 |>
  ggplot(aes(x = n_manufacturers_v1, y = as.numeric(currently_active))) +
  geom_jitter(height = 0.03, alpha = 0.3) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "firebrick") +
  scale_x_log10() +
  labs(x = "Distinct manufacturers (V1: Germany, log scale)", y = "P(currently on shortage list)",
       title = "Fitted shortage probability vs. manufacturer count")

p5 <- analysis |>
  filter(!is.na(n_manufacturers_v2)) |>
  ggplot(aes(x = currently_active, y = n_manufacturers_v2)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_log10() +
  labs(x = "Currently on shortage list", y = "Distinct manufacturers (V2: Germany+EPAR, log scale)",
       title = "Manufacturer count vs. shortage status (V2)")

p6 <- analysis |>
  filter(!is.na(pct_china_india_v2)) |>
  ggplot(aes(x = currently_active, y = pct_china_india_v2)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_continuous(labels = percent) +
  labs(x = "Currently on shortage list", y = "Share of manufacturers in China/India (V2: Germany+EPAR)",
       title = "China+India dependence vs. shortage status (V2)")

p7 <- analysis |>
  filter(!is.na(n_manufacturers_v3)) |>
  ggplot(aes(x = currently_active, y = n_manufacturers_v3)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_log10() +
  labs(x = "Currently on shortage list", y = "Distinct manufacturers (V3: DE+CEP+GMP, log scale)",
       title = "Manufacturer count vs. shortage status (V3)")

p8 <- analysis |>
  filter(!is.na(pct_china_india_v3)) |>
  ggplot(aes(x = currently_active, y = pct_china_india_v3)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_continuous(labels = percent) +
  labs(x = "Currently on shortage list", y = "Share of manufacturers in China/India (V3: DE+CEP+GMP)",
       title = "China+India dependence vs. shortage status (V3)")

p9 <- analysis |>
  filter(!is.na(hhi_v3)) |>
  ggplot(aes(x = currently_active, y = hhi_v3)) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.4) +
  labs(x = "Currently on shortage list", y = "Country-level HHI (V3: DE+CEP+GMP)",
       title = "Supply concentration vs. shortage status (V3)")

ggsave("plot_v1_manufacturers_vs_shortage.png", p1, width = 6, height = 4.5, dpi = 150)
ggsave("plot_v1_hhi_vs_shortage.png",           p2, width = 6, height = 4.5, dpi = 150)
ggsave("plot_v1_china_india_vs_shortage.png",   p3, width = 6, height = 4.5, dpi = 150)
ggsave("plot_v1_shortage_probability_curve.png",p4, width = 6, height = 4.5, dpi = 150)
ggsave("plot_v2_manufacturers_vs_shortage.png", p5, width = 6, height = 4.5, dpi = 150)
ggsave("plot_v2_china_india_vs_shortage.png",   p6, width = 6, height = 4.5, dpi = 150)
ggsave("plot_v3_manufacturers_vs_shortage.png", p7, width = 6, height = 4.5, dpi = 150)
ggsave("plot_v3_china_india_vs_shortage.png",   p8, width = 6, height = 4.5, dpi = 150)
ggsave("plot_v3_hhi_vs_shortage.png",           p9, width = 6, height = 4.5, dpi = 150)

## -----------------------------------------------------------------
## 8. Save the merged analysis table for further digging
## -----------------------------------------------------------------
write_csv(analysis, "atc_shortage_concentration_analysis.csv")

cat("\nDone. Wrote atc_shortage_concentration_analysis.csv and 9 diagnostic plots.\n")