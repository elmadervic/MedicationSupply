## =================================================================
## run_all_lists.R -- runs every analysis script for the list you choose
##
## 1. Choose the list below (or several), e.g.
##      LIST <- "WHO"
##      LIST <- c("EU", "WHO", "overlap")      # all three, one after another
##    "EU"      = EU Union list of critical medicines
##    "WHO"     = WHO Model List of Essential Medicines
##    "overlap" = medicines on both lists
## 2. Run it from the repository root:
##      source("scripts/registers/run_all_lists.R")
##    or from a terminal:  Rscript scripts/registers/run_all_lists.R WHO
##    (a list given on the command line overrides the one below)
##
## Outputs go to results/<list>/ (results/EU, results/WHO, results/overlap);
## the figures used in the paper are then copied to figures/.
## =================================================================

## ---- input --------------------------------------------------------------
LIST <- c("EU", "WHO", "overlap")

## ---- options ----------------------------------------------------------------
REBUILD_COMBINED_DATA <- TRUE   # rerun 01_combine_sources.R first (FALSE = reuse
                                # data/registers/processed/manufacturer_registers_combined.csv)
RUN_TREEMAPS          <- TRUE   # TreeMaps.R (always draws EU, WHO and overlap)
RUN_CROSS_LIST_MODELS <- TRUE   # 02_shortage_models_all_lists.R: one table with all
                                # three lists (results/shortage_models_all_lists.*)
RUN_CONSUMPTION       <- TRUE   # scripts/descriptives/02_antibiotic_consumption.R
COPY_PAPER_FIGURES    <- TRUE   # copy the paper figures from results/ to figures/

## scripts run for the chosen list, in this order
per_list_scripts <- c(
  "PlotbySource_V2.R",
  "bubblePlot.R",
  "atc_summary_report_plot.R",
  "HHI_V2.R",
  "api_diversification_by_source.R",
  "IndiaChina_Steps_V2.R",
  "Shortage.R",           # V1-V3 with the German register (BfArM)
  "Ireland_Shortage.R"     # V1-V3 with the Irish register (HPRA)
)

## =================================================================
## nothing to change below
## =================================================================
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
if (!l10n_info()$`UTF-8`)
  stop("A UTF-8 locale is required: the country maps contain non-ASCII names.", call. = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) LIST <- args

valid <- c("EU", "WHO", "overlap")
bad   <- setdiff(LIST, valid)
if (length(bad) > 0)
  stop("Unknown list: ", paste(bad, collapse = ", "),
       ' -- choose "EU", "WHO" or "overlap".', call. = FALSE)

source("scripts/registers/paths.R")
if (!dir.exists(RAW_DIR))
  stop("No ", RAW_DIR, " folder in ", getwd(),
       "\nSet the working directory to the repository root.", call. = FALSE)

find_script <- function(script) {
  path <- if (grepl("/", script, fixed = TRUE)) script else file.path("scripts", "registers", script)
  if (file.exists(path)) path else NA_character_
}

## the combine script and the per-list scripts are required; stop early,
## naming every missing one, instead of failing halfway through
required <- c(if (REBUILD_COMBINED_DATA) "01_combine_sources.R", per_list_scripts)
missing_required <- required[is.na(vapply(required, find_script, ""))]
if (length(missing_required) > 0)
  stop("These scripts are not in scripts/registers: ",
       paste(missing_required, collapse = ", "), call. = FALSE)

## each script runs in its own environment, so nothing carries over
## between scripts or lists; the working directory stays the repository root
run_script <- function(script, list = NULL, optional = FALSE) {
  path <- find_script(script)
  if (is.na(path)) {
    if (optional) {
      cat("\nSKIPPED:", script, "was not found\n")
      return(invisible(FALSE))
    }
    stop(script, " not found", call. = FALSE)
  }
  env <- new.env()
  if (!is.null(list)) env$LIST <- list
  cat("\n==========", path, if (!is.null(list)) paste("--", list), "==========\n")
  sys.source(path, envir = env)
  grDevices::graphics.off()
  invisible(TRUE)
}

started <- Sys.time()
if (REBUILD_COMBINED_DATA) run_script("01_combine_sources.R")

## the WHO list needs the WHO top-up files; without them the "WHO" results
## would silently be almost the overlap list
if ("WHO" %in% LIST) {
  who_topup_files <- c("data.csv", "hpra_final.csv", "bfarm_api_origin_eml_LONG.csv",
                       "gmdp_EML_API_sites_activities_LONG.csv")
  missing_who <- who_topup_files[!file.exists(file.path(RAW_DIR, who_topup_files))]
  if (length(missing_who) > 0)
    stop("The WHO list needs these files in ", RAW_DIR, "/, which are missing:\n   ",
         paste(missing_who, collapse = "\n   "),
         "\nAdd them (and keep REBUILD_COMBINED_DATA <- TRUE), or run EU / overlap only.",
         call. = FALSE)
}

for (l in LIST) {
  for (s in per_list_scripts) run_script(s, l)
}

if (RUN_TREEMAPS)          run_script("TreeMaps.R", optional = TRUE)
if (RUN_CROSS_LIST_MODELS) run_script("02_shortage_models_all_lists.R", optional = TRUE)
if (RUN_CONSUMPTION)       run_script("scripts/descriptives/02_antibiotic_consumption.R")

paper_figures <- c(
  "EU/hhi_step1_priority_by_source_EU.png",
  "WHO/hhi_step1_priority_by_source_WHO.png",
  "EU/manufacturing_sites_by_chapter_country_group_source_EU.png",
  "WHO/manufacturing_sites_by_chapter_country_group_source_WHO.png",
  "EU/api_diversification_Germany_EU.png",
  "EU/plots_by_source_EU.png",
  "EU/hhi_by_chapter_by_source_EU.png",
  "WHO/hhi_by_chapter_by_source_WHO.png",
  "EU/atc_summary_report_plot_v2_EU.png",
  "WHO/atc_summary_report_plot_v2_WHO.png",
  "EU/bubble_matrix_by_source_EU.png",
  "WHO/bubble_matrix_by_source_WHO.png",
  "EU/data_ireland_EU.png",
  "EU/hhi_step1_priority_Ireland_EU.png"
)
paper_figures <- setNames(file.path(RESULTS_DIR, paper_figures), file.path(FIG_DIR, basename(paper_figures)))
paper_figures[file.path(FIG_DIR, "plot4_treemap_chapters_eu_who.png")] <-
  file.path(RESULTS_DIR, "treemap_chapters_EU_vs_WHO.png")
if (COPY_PAPER_FIGURES) {
  found <- file.exists(paper_figures)
  file.copy(paper_figures[found], names(paper_figures)[found], overwrite = TRUE)
  cat("\nPaper figures copied to", FIG_DIR, ":", sum(found), "of", length(paper_figures), "\n")
  if (any(!found)) cat("Not produced in this run:", paste(basename(paper_figures[!found]), collapse = ", "), "\n")
}

cat("\nDone:", paste(LIST, collapse = ", "), "in",
    round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1), "min.",
    "Results in", paste(file.path(RESULTS_DIR, LIST), collapse = ", "),
    if ("Ireland_Shortage.R" %in% per_list_scripts) paste("+", file.path(RESULTS_DIR, "IrelandShortage")), "\n")
