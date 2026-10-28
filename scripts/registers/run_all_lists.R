## =================================================================
## run_all_lists.R -- runs every analysis script for the list you choose
##
## 1. Choose the list below (or several), e.g.
##      LIST <- "WHO"
##      LIST <- c("EU", "WHO", "overlap")      # all three, one after another
##    "EU"      = EU Union list of critical medicines
##    "WHO"     = WHO Model List of Essential Medicines
##    "overlap" = medicines on both lists
## 2. Run it from the project folder that contains Data/:
##      source("run_all_lists.R")
##    or from a terminal:  Rscript run_all_lists.R WHO
##    (a list given on the command line overrides the one below)
##
## Outputs go to Results/<list>/ (Results/EU, Results/WHO, Results/overlap).
## =================================================================

## ---- input --------------------------------------------------------------
LIST <- "EU"

## ---- options ----------------------------------------------------------------
REBUILD_COMBINED_DATA <- TRUE   # rerun 01_combine_sources.R first (FALSE = reuse
                                # Data/manufacturer_registers_combined.csv)
RUN_TREEMAPS          <- TRUE   # TreeMaps.R (always draws EU, WHO and overlap)
RUN_CROSS_LIST_MODELS <- FALSE  # 02_shortage_models_all_lists.R: one table with all
                                # three lists (Results/shortage_models_all_lists.*)

## scripts run for the chosen list, in this order
per_list_scripts <- c(
  "PlotbySource_V2.R",
  "bubblePlot.R",
  "atc_summary_report_plot.R",
  "HHI_V2.R",
  "api_diversification_by_source.R",
  "EEAnonEEASteps_V2.R",
  "IndiaChina_Steps_V2.R",
  "Shortage.R"            # V1-V3 with the German register (BfArM)
  # "Ireland_Shortage.R"     # V1-V3 with the Irish register (HPRA)
)

## =================================================================
## nothing to change below
## =================================================================
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) LIST <- args

valid <- c("EU", "WHO", "overlap")
bad   <- setdiff(LIST, valid)
if (length(bad) > 0)
  stop("Unknown list: ", paste(bad, collapse = ", "),
       ' -- choose "EU", "WHO" or "overlap".', call. = FALSE)

if (!dir.exists("Data"))
  stop("No Data/ folder in ", getwd(),
       "\nSet the working directory to the project folder that contains Data/.", call. = FALSE)

## find a script in the project folder or any subfolder (Data/ and Results/
## are skipped); NA if it is not there
find_script <- function(script) {
  if (file.exists(script)) return(script)
  hits <- list.files(".", pattern = paste0("^", gsub(".", "\\.", script, fixed = TRUE), "$"),
                     recursive = TRUE)
  hits <- hits[!grepl("^(Data|Results)/", hits)]
  if (length(hits) == 0) NA_character_ else hits[1]
}

## the combine script and the per-list scripts are required; stop early,
## naming every missing one, instead of failing halfway through
required <- c(if (REBUILD_COMBINED_DATA) "01_combine_sources.R", per_list_scripts)
missing_required <- required[is.na(vapply(required, find_script, ""))]
if (length(missing_required) > 0)
  stop("These scripts are not in ", getwd(), " (or a subfolder): ",
       paste(missing_required, collapse = ", "), call. = FALSE)

## each script runs in its own environment, so nothing carries over
## between scripts or lists; the working directory stays the project folder
run_script <- function(script, list = NULL, optional = FALSE) {
  path <- find_script(script)
  if (is.na(path)) {
    if (optional) {
      cat("\nSKIPPED:", script, "is not in the project folder\n")
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
  missing_who <- who_topup_files[!file.exists(file.path("Data", who_topup_files))]
  if (length(missing_who) > 0)
    stop("The WHO list needs these files in Data/, which are missing:\n   ",
         paste(missing_who, collapse = "\n   "),
         "\nAdd them (and keep REBUILD_COMBINED_DATA <- TRUE), or run EU / overlap only.",
         call. = FALSE)
}

for (l in LIST) {
  for (s in per_list_scripts) run_script(s, l)
}

if (RUN_TREEMAPS)          run_script("TreeMaps.R", optional = TRUE)
if (RUN_CROSS_LIST_MODELS) run_script("02_shortage_models_all_lists.R", optional = TRUE)

cat("\nDone:", paste(LIST, collapse = ", "), "in",
    round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1), "min.",
    "Results in", paste(file.path("Results", LIST), collapse = ", "),
    if ("Ireland_Shortage.R" %in% per_list_scripts) "+ Results/IrelandShortage", "\n")
