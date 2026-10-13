library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(broom)
library(ggplot2)
library(scales)
library(car)
library(pROC)

source("scripts/registers/paths.R")
SHORT_DIR <- "results/shortage"
SHORT_FIG_DIR <- file.path(ADD_DIR, "shortage")
for (d in c(SHORT_DIR, SHORT_FIG_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
sink(file.path(SHORT_DIR, "shortage_model_output.txt"), split = TRUE)
set.seed(1)

clean_names_simple <- function(df) {
  nm <- names(df)
  nm <- str_to_lower(nm)
  nm <- str_replace_all(nm, "[^a-z0-9]+", "_")
  nm <- str_replace_all(nm, "^_|_$", "")
  names(df) <- nm
  df
}

path_mfr       <- file.path(PROC_DIR, "manufacturer_registers_combined.csv")
path_shortages <- "data/shortages/LEMeldungen_2026-09-09-13-30-43.csv"

mfr <- read_csv(path_mfr, show_col_types = FALSE) |>
  mutate(
    mfr_company = str_squish(mfr_company),
    mfr_country = str_squish(mfr_country)
  )

cat("Manufacturer register rows:", nrow(mfr), "\n")
cat("Unique critical ATC codes covered by ANY source:", n_distinct(mfr$atc_code), "\n")
print(table(mfr$source))

shortages_raw <- read_delim(
  path_shortages,
  delim = ";",
  locale = locale(encoding = "ISO-8859-1", decimal_mark = ","),
  show_col_types = FALSE
) |>
  clean_names_simple()

cat("\nShortage report rows:", nrow(shortages_raw), "\n")

parse_de_date <- function(x) dmy(na_if(x, "N/A"))

shortages <- shortages_raw |>
  mutate(
    beginn = parse_de_date(beginn),
    ende   = parse_de_date(ende)
  )

shortage_long <- shortages |>
  filter(!is.na(atc_code), atc_code != "") |>
  separate_rows(atc_code, sep = ",\\s*") |>
  mutate(atc_code = str_trim(atc_code))

n_level4  <- sum(nchar(shortage_long$atc_code) == 5, na.rm = TRUE)
n_no_atc  <- sum(str_detect(shortage_long$atc_code, "^TERM_ID_NA"), na.rm = TRUE)
cat("Dropping", n_level4, "level-4-only rows and", n_no_atc,
    "rows with no assigned ATC code from the exact-match flag.\n")

shortage_long <- shortage_long |>
  filter(nchar(atc_code) == 7)

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

apply_step1_priority <- function(df) {
  df |>
    group_by(atc_code) |>
    mutate(has_step1 = any(manufacturer_step == 1, na.rm = TRUE)) |>
    filter(!has_step1 | (has_step1 & manufacturer_step == 1)) |>
    ungroup() |>
    select(-has_step1)
}

compute_metrics <- function(df, china_india = c("China", "India"), step1_priority = FALSE) {
  if (step1_priority) df <- apply_step1_priority(df)
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
      single_source  = n_manufacturers == 1,
      single_country = hhi >= 9999
    )
}

metrics_epar <- compute_metrics(filter(mfr, source == "ema"), step1_priority = TRUE) |>
  rename_with(~paste0(.x, "_epar"), -atc_code)

metrics_v1 <- compute_metrics(filter(mfr, source == "germany"), step1_priority = TRUE) |>
  rename_with(~paste0(.x, "_v1"), -atc_code)

metrics_v2 <- compute_metrics(filter(mfr, source %in% c("germany", "ema")), step1_priority = TRUE) |>
  rename_with(~paste0(.x, "_v2"), -atc_code)

all_codes <- tibble(atc_code = unique(mfr$atc_code))

analysis <- all_codes |>
  left_join(metrics_epar,     by = "atc_code") |>
  left_join(metrics_v1,       by = "atc_code") |>
  left_join(metrics_v2,       by = "atc_code") |>
  left_join(shortage_status,  by = "atc_code") |>
  mutate(
    ever_reported    = replace_na(ever_reported, FALSE),
    currently_active = replace_na(currently_active, FALSE),
    n_reports        = replace_na(n_reports, 0L)
  )

cat("\nAnalysis table:", nrow(analysis), "critical ATC codes.\n")

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
  univariate_test("hhi_v2",             "Country HHI (V2: Germany+EPAR)")
)

cat("\n--- Univariate association with currently-active shortage ---\n")
print(univariate_results, n = Inf)

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

ggsave(file.path(SHORT_FIG_DIR, "plot_v1_manufacturers_vs_shortage.png"), p1, width = 6, height = 4.5, dpi = 150)
ggsave(file.path(SHORT_FIG_DIR, "plot_v1_hhi_vs_shortage.png"),           p2, width = 6, height = 4.5, dpi = 150)
ggsave(file.path(SHORT_FIG_DIR, "plot_v1_china_india_vs_shortage.png"),   p3, width = 6, height = 4.5, dpi = 150)
ggsave(file.path(SHORT_FIG_DIR, "plot_v1_shortage_probability_curve.png"),p4, width = 6, height = 4.5, dpi = 150)
ggsave(file.path(SHORT_FIG_DIR, "plot_v2_manufacturers_vs_shortage.png"), p5, width = 6, height = 4.5, dpi = 150)
ggsave(file.path(SHORT_FIG_DIR, "plot_v2_china_india_vs_shortage.png"),   p6, width = 6, height = 4.5, dpi = 150)

write_csv(analysis, file.path(SHORT_DIR, "atc_shortage_concentration_analysis.csv"))

cat("\nDone. Wrote atc_shortage_concentration_analysis.csv and 6 diagnostic plots.\n")
sink()
