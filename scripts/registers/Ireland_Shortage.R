## =================================================================
## Ireland_Shortage.R -- the shortage analysis of Shortage.R with the
## Irish register (HPRA) as base instead of the German one (BfArM):
##   V1: Ireland
##   V2: Ireland + EPAR
##   V3: Ireland + EPAR + CEP + GMDP API sites
## Everything else is the same as Shortage.R: the outcome is a currently
## active shortage in the German BfArM register, main + regional models,
## step-1 (API) priority, same diagnostics and plots.
##
## Notes on the Irish data:
##  - HPRA lists the manufacturers named in the product information, mostly
##    finished-product / batch-release sites, and has no step field. Under
##    the step-1 priority rule, Irish rows are therefore used where no API
##    (step-1) data exists for a code: in V2 they give way to EPAR step-1
##    sites, in V3 to EPAR step-1 sites, CEP holders and GMDP API sites -- the same rule that
##    replaces German step-2 sites in the Germany versions.
##  - Ireland rows whose company and country lists could not be paired
##    ("Belgium | Germany" with one company string) have no single country
##    and are left out (the number is printed).
##
##  - The Irish register lists no manufacturer in China or India, so in V1
##    the China + India share (and the China / India shares in the regional
##    model) is 0 for every code and cannot be estimated: those terms are
##    left out of the V1 models and marked "not estimable" in the table.
##
## Outputs -- all lists in one folder, results/IrelandShortage/, with the
## list (EU, WHO or overlap) in every file name:
##   shortage_models_V1_V2_V3_Ireland_<LIST>.csv   main + regional models
##   shortage_region_counts_Ireland_<LIST>.csv
##   atc_shortage_concentration_analysis_Ireland_<LIST>.csv, plot_v*_Ireland_<LIST>.png
##
## Run from the repository root:
##   LIST <- "WHO"; source("scripts/registers/Ireland_Shortage.R")
## run_all_lists.R runs it for every chosen list.
## =================================================================
if (!exists("LIST")) LIST <- "EU"

shortage_script <- "scripts/registers/Shortage.R"
if (!file.exists(shortage_script)) stop(shortage_script, " not found; run from the repository root", call. = FALSE)

## an older Shortage.R ignores BASE and would silently run Germany again
if (!any(grepl("base_source", readLines(shortage_script, warn = FALSE), fixed = TRUE)))
  stop(shortage_script, " is an older version without Ireland support -- ",
       "replace it with the current Shortage.R (version 2026-10-20 or later).", call. = FALSE)

## run Shortage.R in its own environment with BASE = "Ireland", so BASE is
## never left behind in your R session (a leftover BASE would make a later
## run of Shortage.R use Ireland instead of Germany)
ireland_env <- new.env(parent = globalenv())
ireland_env$LIST <- LIST
ireland_env$BASE <- "Ireland"
sys.source(shortage_script, envir = ireland_env)
rm(ireland_env)
