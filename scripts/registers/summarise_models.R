## =================================================================
## summarise_models.R -- writes ONE readable text report of all model
## results, so the CSV files don't have to be read:
##
##   results/model_summary.txt
##
## Contents:
##   1. Overview: China + India odds ratio for every outcome, list, version
##   2. Per manufacturer: effect of moving one manufacturer to China/India
##   3. Full results: main and regional model per outcome, list, version
##
## Reads whatever exists of:
##   results/<LIST>/shortage_models_V1_V2_V3_<LIST>.csv            (Germany)
##   results/GermanyEMAShortage/shortage_models_V1_V2_V3_GermanyEMA_<LIST>.csv
##   results/GermanyEMAallShortage/shortage_models_V1_V2_V3_GermanyEMAall_<LIST>.csv
##   results/IrelandShortage/shortage_models_V1_V2_V3_Ireland_<LIST>.csv
##   results/shortage_per_manufacturer_effect.csv
##
## Which streams and which file: set SUMMARY_STREAMS / SUMMARY_FILE before
## sourcing (defaults: all streams found -> results/model_summary.txt), e.g.
##   SUMMARY_STREAMS <- c("Germany", "GermanyEMAall")
##   SUMMARY_FILE    <- "results/model_summary_EMA_all.txt"
##
## Run AFTER Shortage.R / GermanyEMA_Shortage.R / shortage_per_manufacturer.R:
##   source("scripts/registers/summarise_models.R")
## =================================================================
library(dplyr)
library(readr)
source("scripts/registers/paths.R", local = TRUE)

LISTS   <- c("EU", "WHO", "overlap")
OUT     <- if (exists("SUMMARY_FILE")) SUMMARY_FILE else file.path(RESULTS_DIR, "model_summary.txt")

list_names <- c(EU = "EU Union list of critical medicines", WHO = "WHO Essential Medicines List",
                overlap = "On both lists (overlap)")
version_names <- c(v1 = "V1", v2 = "V2", v3 = "V3")
stream_names <- c(Germany    = "Shortage outcome: German BfArM register",
                  GermanyEMA = "Shortage outcome: German BfArM register OR EMA shortage list (active on 2026-09-09)",
                  GermanyEMAall = "Shortage outcome: German BfArM register OR ever on EMA shortage list (all statuses)",
                  Ireland    = "Manufacturers from the Irish register (HPRA); outcome: German BfArM register")
term_names <- c(
  log_n_manufacturers = "Number of manufacturers (log)",
  hhi_scaled          = "Country concentration (HHI, 0-1)",
  pct_china_india     = "Share of manufacturers in China + India",
  pct_china           = "Share in China",
  pct_india           = "Share in India",
  pct_other_asia      = "Share in other Asia & Oceania",
  pct_north_america   = "Share in North America",
  pct_non_eu_europe   = "Share in non-EU Europe",
  pct_rest_of_world   = "Share in rest of world",
  atc_A = "ATC A: Alimentary tract & metabolism", atc_B = "ATC B: Blood & blood-forming organs",
  atc_C = "ATC C: Cardiovascular system",         atc_D = "ATC D: Dermatologicals",
  atc_G = "ATC G: Genito-urinary & sex hormones", atc_H = "ATC H: Systemic hormones",
  atc_J = "ATC J: Anti-infectives (systemic)",    atc_L = "ATC L: Antineoplastic & immunomod.",
  atc_M = "ATC M: Musculo-skeletal system",       atc_N = "ATC N: Nervous system",
  atc_P = "ATC P: Antiparasitic products",        atc_R = "ATC R: Respiratory system",
  atc_S = "ATC S: Sensory organs",                atc_V = "ATC V: Various",
  atc_joint = "All ATC dummies (joint LR test)"
)
model_names <- c(main         = "Main model",
                 main_atc     = "Main model + ATC chapter dummies",
                 regional     = "Regional model (EU/EEA = reference)",
                 regional_atc = "Regional model + ATC chapter dummies (EU/EEA = reference)")

## ---- read all model files that exist ---------------------------------
model_file <- function(stream, L) switch(stream,
                                         Germany    = file.path(RESULTS_DIR, L, paste0("shortage_models_V1_V2_V3_", L, ".csv")),
                                         GermanyEMA = file.path(RESULTS_DIR, "GermanyEMAShortage", paste0("shortage_models_V1_V2_V3_GermanyEMA_", L, ".csv")),
                                         GermanyEMAall = file.path(RESULTS_DIR, "GermanyEMAallShortage", paste0("shortage_models_V1_V2_V3_GermanyEMAall_", L, ".csv")),
                                         Ireland    = file.path(RESULTS_DIR, "IrelandShortage", paste0("shortage_models_V1_V2_V3_Ireland_", L, ".csv")))

streams_wanted <- if (exists("SUMMARY_STREAMS")) SUMMARY_STREAMS else names(stream_names)
models <- list()
for (S in intersect(names(stream_names), streams_wanted)) for (L in LISTS) {
  f <- model_file(S, L)
  if (file.exists(f))
    models[[length(models) + 1]] <- read_csv(f, show_col_types = FALSE) |>
      mutate(stream = S, list = L, version_label = as.character(version_label),
             note = coalesce(as.character(note), ""))
}
if (!length(models)) stop("No model files found. Run Shortage.R first.")
models <- bind_rows(models) |>
  mutate(stream = factor(stream, levels = names(stream_names)),
         list   = factor(list, levels = LISTS))
streams_found <- levels(droplevels(models$stream))

## ---- formatting helpers ----------------------------------------------
fmt_num <- function(x, d = 2) ifelse(is.na(x), "  -  ",
                                     ifelse(abs(x) >= 1000 | (abs(x) < 0.01 & x != 0), formatC(x, format = "e", digits = 1),
                                            formatC(x, format = "f", digits = d)))
fmt_or <- function(or, lo, hi) ifelse(is.na(or), "not estimable",
                                      paste0(fmt_num(or), " (", fmt_num(lo), " to ", fmt_num(hi), ")"))
fmt_p  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
stars  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))))
pad    <- function(x, w) formatC(as.character(x), width = -w)
rule   <- function(ch = "-", n = 92) strrep(ch, n)

lines <- character()
out <- function(...) lines <<- c(lines, paste0(...))

## ---- header -----------------------------------------------------------
out(rule("="))
out("SHORTAGE MODELS -- SUMMARY OF ALL RESULTS")
out("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M"))
out(rule("="))
out("")
out("How to read this")
out("  Logistic regression: is a critical medicine (ATC code) currently in shortage?")
out("  OR = odds ratio with 95% confidence interval. OR > 1: higher shortage odds;")
out("  OR < 1: lower. A CI that does not include 1 is statistically significant.")
out("  p = likelihood-ratio test.  * p<0.05  ** p<0.01  *** p<0.001")
out("  Share terms compare a medicine with ALL its manufacturers in that region to one")
out("  with all manufacturers in the EU/EEA (reference). Concentration (HHI) runs from")
out("  0 (spread over many countries) to 1 (all manufacturers in one country).")
out("  Manufacturer data versions:")
out("    V1 = national register only (Germany)")
out("    V2 = V1 + EMA EPAR manufacturers")
out("    V3 = V2 + CEP holders + EudraGMDP active-substance sites")
out("  AUC: how well the model separates shortage from non-shortage (0.5 = chance).")
out("  EPV = shortages per predictor; below 10 the estimates are unstable.")
out("  ATC chapter dummies (+ ATC models): one dummy per ATC chapter (first letter of the")
out("  ATC code) with at least 15 codes in that list and version; smaller chapters pooled")
out("  as the reference. 'All ATC dummies (joint LR test)' tests whether therapeutic area")
out("  adds information; dAIC < 0 means the model with ATC dummies fits better.")
out("")
out("Outcome streams included:")
for (S in streams_found) out("  - ", S, ": ", stream_names[[S]])
out("")

## ---- 1. overview: China + India OR ---------------------------------------
out(rule("="))
out("1. OVERVIEW -- share of manufacturers in China + India (main model)")
out(rule("="))
ov <- models |> filter(model == "main", term == "pct_china_india") |> arrange(stream, list, version)
out(pad("Outcome", 15), pad("List", 9), pad("Ver", 5), pad("Codes", 7), pad("Short.", 7),
    pad("OR (95% CI)", 30), pad("p", 11), "AUC")
out(rule())
for (i in seq_len(nrow(ov))) {
  r <- ov[i, ]
  out(pad(r$stream, 15), pad(r$list, 9), pad(toupper(r$version), 5), pad(r$n_codes, 7), pad(r$n_shortage, 7),
      pad(fmt_or(r$odds_ratio, r$ci_low, r$ci_high), 30), pad(paste0(fmt_p(r$p_lrt), stars(r$p_lrt)), 11),
      fmt_num(r$auc))
}
out("")

## ---- 1b. overview: with ATC chapter dummies --------------------------------
if (any(models$model == "main_atc")) {
  out(rule("="))
  out("1b. OVERVIEW -- China + India share with and without ATC chapter dummies (main model)")
  out(rule("="))
  w <- models |> filter(model %in% c("main", "main_atc"), term == "pct_china_india") |>
    select(stream, list, version, model, odds_ratio, ci_low, ci_high, p_lrt, auc)
  j <- models |> filter(model == "main_atc", term == "atc_joint") |>
    transmute(stream, list, version, joint_p = p_lrt,
              d_aic = as.numeric(sub(".*dAIC ([-+0-9.]+);.*", "\\1", note)))
  out(pad("Outcome", 15), pad("List", 9), pad("Ver", 5), pad("OR without ATC", 27), pad("OR with ATC", 27),
      pad("ATC joint p", 13), pad("dAIC", 7), "AUC without / with")
  out(rule())
  keys <- w |> distinct(stream, list, version) |> arrange(stream, list, version)
  for (i in seq_len(nrow(keys))) {
    k  <- keys[i, ]
    a0 <- w |> filter(stream == k$stream, list == k$list, version == k$version, model == "main")
    a1 <- w |> filter(stream == k$stream, list == k$list, version == k$version, model == "main_atc")
    jj <- j |> filter(stream == k$stream, list == k$list, version == k$version)
    if (!nrow(a0) || !nrow(a1) || !nrow(jj)) next
    out(pad(k$stream, 15), pad(k$list, 9), pad(toupper(k$version), 5),
        pad(paste0(fmt_or(a0$odds_ratio, a0$ci_low, a0$ci_high), stars(a0$p_lrt)), 27),
        pad(paste0(fmt_or(a1$odds_ratio, a1$ci_low, a1$ci_high), stars(a1$p_lrt)), 27),
        pad(fmt_p(jj$joint_p), 13), pad(sprintf("%+.1f", jj$d_aic), 7),
        fmt_num(a0$auc), " / ", fmt_num(a1$auc))
  }
  out("")
}

## ---- 2. per-manufacturer effect -------------------------------------------
pm_file <- file.path(RESULTS_DIR, "shortage_per_manufacturer_effect.csv")
out(rule("="))
out("2. EFFECT OF MOVING ONE MANUFACTURER TO CHINA/INDIA (total number unchanged)")
out(rule("="))
if (file.exists(pm_file)) {
  pm <- read_csv(pm_file, show_col_types = FALSE)
  if (!"outcome" %in% names(pm)) pm$outcome <- "Germany"
  pm <- pm |> filter(outcome %in% streams_wanted) |> mutate(outcome = factor(outcome, levels = names(stream_names)),
                                                            list = factor(list, levels = LISTS)) |> arrange(outcome, list, version)
  out("Change in a medicine's predicted shortage risk, averaged over medicines that")
  out("still have a manufacturer outside China/India (95% CI from 4000 simulations).")
  out("")
  out(pad("Outcome", 15), pad("List", 9), pad("Ver", 5), pad("Mean risk", 11),
      pad("Relative change (95% CI)", 28), "Absolute change, percentage points (95% CI)")
  out(rule())
  for (i in seq_len(nrow(pm))) {
    r <- pm[i, ]
    out(pad(r$outcome, 15), pad(r$list, 9), pad(toupper(r$version), 5),
        pad(sprintf("%.1f%%", r$mean_risk_pct), 11),
        pad(sprintf("%+.0f%% (%.0f to %.0f%%)", r$risk_change_pct, r$risk_change_lo, r$risk_change_hi), 28),
        sprintf("%+.1f pp (%.1f to %.1f)", r$abs_change_pp, r$abs_change_lo, r$abs_change_hi))
  }
} else {
  out("(not available -- run shortage_per_manufacturer.R first)")
}
out("")

## ---- 3. full results ------------------------------------------------------
out(rule("="))
out("3. FULL MODEL RESULTS")
out(rule("="))
blocks <- models |> distinct(stream, list, version) |> arrange(stream, list, version)
for (i in seq_len(nrow(blocks))) {
  b <- blocks[i, ]
  d <- models |> filter(stream == b$stream, list == b$list, version == b$version)
  out("")
  out(rule("#"))
  out(stream_names[[as.character(b$stream)]])
  out(list_names[[as.character(b$list)]], "  |  ", d$version_label[1])
  out(rule("#"))
  for (M in names(model_names)) {
    dm <- d |> filter(model == M)
    if (!nrow(dm)) next
    f <- dm[1, ]
    out("")
    out(model_names[[M]],
        sprintf(":  %d codes, %d in shortage (%.1f%%)  |  AUC %s  |  McFadden R2 %s  |  AIC %s  |  EPV %s",
                as.integer(f$n_codes), as.integer(f$n_shortage), 100 * f$n_shortage / f$n_codes,
                fmt_num(f$auc), fmt_num(f$mcfadden_r2, 3), fmt_num(f$aic, 1), fmt_num(f$events_per_variable, 1)))
    out("  ", pad("Predictor", 42), pad("OR (95% CI)", 30), "p")
    out("  ", rule("-", 86))
    for (j in seq_len(nrow(dm))) {
      r <- dm[j, ]
      if (r$term == "atc_joint") {
        out("  ", pad(term_names[["atc_joint"]], 42), pad("", 30), paste0(fmt_p(r$p_lrt), stars(r$p_lrt)))
        out("  ", pad("", 42), "   ", sub("joint LR test of ", "", r$note))
        next
      }
      label <- if (r$term %in% names(term_names)) term_names[[r$term]] else r$term
      out("  ", pad(label, 42), pad(fmt_or(r$odds_ratio, r$ci_low, r$ci_high), 30),
          paste0(fmt_p(r$p_lrt), stars(r$p_lrt)))
      if (nzchar(r$note) && !grepl("few events", r$note)) out("  ", pad("", 42), "   ! ", r$note)
    }
    if (any(grepl("few events", dm$note))) out("  ! few events per predictor (<10): interpret with care")
  }
}
out("")
out(rule("="))
out("End of summary")

dir.create(RESULTS_DIR, showWarnings = FALSE)
writeLines(lines, OUT)
cat("Saved:", OUT, "(", length(lines), "lines )\n")