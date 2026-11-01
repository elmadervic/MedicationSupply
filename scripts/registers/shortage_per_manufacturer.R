## =================================================================
## shortage_per_manufacturer.R -- translates the pooled shortage model
## (China + India share) into the effect of ONE manufacturer:
##
##   "moving one of a medicine's manufacturers to China or India, with the
##    total number of manufacturers unchanged, increases its predicted
##    shortage risk on average by X% (95% CI ...), from a mean risk of Y%
##    (+Z percentage points)"
##
## How it works, per list (EU, WHO, overlap) and version (V1-V3):
##   1. Refit the pooled model of Shortage.R:
##        shortage ~ log(1 + n) + HHI/10000 + share China+India
##   2. For every medicine with at least one manufacturer outside China and
##      India, predict the shortage probability as it is (p0) and with one
##      manufacturer moved to China/India, i.e. share + 1/n (p1).
##   3. Relative change = mean(p1) / mean(p0) - 1; absolute change =
##      mean(p1 - p0) in percentage points.
##   4. 95% CI: 4000 draws of the coefficients from their estimated
##      sampling distribution (multivariate normal), steps 2-3 per draw.
##
## Two shortage outcomes ("streams"), each per list and version:
##   Germany    = German BfArM register (Shortage.R)
##   GermanyEMA = German BfArM register OR EMA shortage list
##                (GermanyEMA_Shortage.R)
##
## Input (written by Shortage.R / GermanyEMA_Shortage.R, one per list):
##   results/<LIST>/atc_shortage_concentration_analysis_<LIST>.csv
##   results/GermanyEMAShortage/atc_shortage_concentration_analysis_GermanyEMA_<LIST>.csv
## Output:
##   results/shortage_per_manufacturer_effect.csv  (all outcomes, lists, versions)
##
## Run from the repository root: source("scripts/registers/shortage_per_manufacturer.R")
## Requires: install.packages(c("dplyr", "readr", "MASS"))
## =================================================================
library(dplyr)
library(readr)
source("scripts/registers/paths.R", local = TRUE)

LISTS    <- c("EU", "WHO", "overlap")   # lists to compute (skipped if the file is missing)
OUTCOMES <- c("Germany", "GermanyEMA")  # shortage outcome streams (skipped if missing)
analysis_path <- function(outcome, L) switch(outcome,
  Germany    = file.path(RESULTS_DIR, L, paste0("atc_shortage_concentration_analysis_", L, ".csv")),
  GermanyEMA = file.path(RESULTS_DIR, "GermanyEMAShortage",
                         paste0("atc_shortage_concentration_analysis_GermanyEMA_", L, ".csv")))
VERSIONS <- c("v1", "v2", "v3")
N_DRAWS  <- 4000
set.seed(1)                              # same CIs every run

results <- list()

for (O in OUTCOMES) for (L in LISTS) {
  path <- analysis_path(O, L)
  if (!file.exists(path)) {
    cat("SKIPPED", O, L, "-- not found:", path,
        if (O == "Germany") "(run Shortage.R for this list first)\n" else "(run GermanyEMA_Shortage.R first)\n")
    next
  }
  analysis <- read_csv(path, show_col_types = FALSE)

  for (v in VERSIONS) {
    d <- analysis |>
      transmute(
        shortage = as.integer(currently_active %in% TRUE),
        n        = .data[[paste0("n_manufacturers_", v)]],
        n_ci     = .data[[paste0("n_china_india_", v)]],
        hhi      = .data[[paste0("hhi_", v)]] / 10000,
        share_ci = .data[[paste0("pct_china_india_", v)]]      # share 0-1
      ) |>
      filter(!is.na(n), n > 0)

    ## 1. pooled model, as in Shortage.R
    fit <- glm(shortage ~ log1p(n) + hhi + share_ci, family = binomial, data = d)

    ## 2. medicines that still have a manufacturer outside China/India
    movable <- filter(d, n_ci < n)
    X0 <- model.matrix(~ log1p(n) + hhi + share_ci, movable)
    X1 <- X0
    X1[, "share_ci"] <- X1[, "share_ci"] + 1 / movable$n           # one manufacturer moved

    ## 3. relative and absolute change in mean predicted risk
    change <- function(beta) {
      p0 <- plogis(X0 %*% beta)
      p1 <- plogis(X1 %*% beta)
      c(relative = mean(p1) / mean(p0) - 1, absolute = mean(p1 - p0))
    }
    est <- change(coef(fit))

    ## 4. CI from coefficient draws
    draws <- MASS::mvrnorm(N_DRAWS, coef(fit), vcov(fit))
    sims  <- t(apply(draws, 1, change))

    results[[length(results) + 1]] <- tibble(
      outcome               = O,
      list                  = L,
      version               = v,
      n_codes               = nrow(d),
      n_shortage            = sum(d$shortage),
      mean_risk_pct         = 100 * mean(d$shortage),
      or_share_china_india  = unname(exp(coef(fit)["share_ci"])),  # check vs shortage_models file
      risk_change_pct       = 100 * est[["relative"]],
      risk_change_lo        = 100 * unname(quantile(sims[, "relative"], 0.025)),
      risk_change_hi        = 100 * unname(quantile(sims[, "relative"], 0.975)),
      abs_change_pp         = 100 * est[["absolute"]],
      abs_change_lo         = 100 * unname(quantile(sims[, "absolute"], 0.025)),
      abs_change_hi         = 100 * unname(quantile(sims[, "absolute"], 0.975))
    )
  }
}

out <- bind_rows(results)
dir.create(RESULTS_DIR, showWarnings = FALSE)
write_csv(out, file.path(RESULTS_DIR, "shortage_per_manufacturer_effect.csv"))

## ---- readable summary --------------------------------------------------
cat("\nOne manufacturer moved to China/India (total number unchanged):\n")
for (i in seq_len(nrow(out))) {
  r <- out[i, ]
  cat(sprintf("%-10s %-8s %s  (%d codes, %d shortages, mean risk %.1f%%; OR share %.2f):  %+.0f%% (95%% CI %.0f to %.0f%%),  %+.1f pp (%.1f to %.1f)\n",
              r$outcome, r$list, toupper(r$version), r$n_codes, r$n_shortage, r$mean_risk_pct, r$or_share_china_india,
              r$risk_change_pct, r$risk_change_lo, r$risk_change_hi,
              r$abs_change_pp, r$abs_change_lo, r$abs_change_hi))
}
cat("\nSaved:", file.path(RESULTS_DIR, "shortage_per_manufacturer_effect.csv"), "\n")
