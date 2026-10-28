## =================================================================
## GermanyEMA_Shortage.R -- second shortage outcome stream:
##   German BfArM shortage register COMBINED WITH the EMA shortage list
##
## Runs Shortage.R once per list (EU, WHO, overlap) with
## OUTCOME = "GermanyEMA": a critical ATC code counts as currently in
## shortage if it is on the German BfArM register OR on the EMA shortage
## list on the reference date (2026-09-09, as for the German flag).
## Manufacturer data (V1 = Germany, V2 = + EPAR, V3 = + CEP + GMDP) are
## exactly as in the German-only stream; only the outcome changes.
##
## A second stream, GermanyEMAall, counts EVERY code ever on the EMA list
## (status and dates ignored; German part unchanged). Outputs:
## Results/GermanyEMAallShortage/*_GermanyEMAall_<LIST>.*
## Choose which streams to run in EMA_STREAMS below.
##
## Each run uses its own environment, so OUTCOME never carries over to a
## later plain Shortage.R run (which stays German-only).
##
## Inputs (Data folder), on top of those Shortage.R already uses:
##   Medicines_output_shortages_en.json  EMA shortage list download
##   ema_shortages_atc_crosswalk.csv     EMA record -> ATC code(s), curated;
##                                       a new EMA download stops the run
##                                       until its new records are added
## Outputs: Results/GermanyEMAShortage/*_GermanyEMA_<LIST>.*
##   same files as the German stream, plus ema_shortage_atc_matches.csv;
##   the analysis CSV keeps currently_active_de and ema_active separately
##
## Run from the project folder: source("GermanyEMA_Shortage.R")
## Requires (on top of Shortage.R's packages): install.packages("jsonlite")
## =================================================================
LISTS       <- c("EU", "WHO", "overlap")
EMA_STREAMS <- c("GermanyEMA",      # EMA records active on the reference date
                 "GermanyEMAall")   # every EMA record, status and dates ignored

for (O in EMA_STREAMS) for (L in LISTS) {
  cat("\n\n################", O, "outcome --", L, "################\n")
  env <- new.env()
  env$LIST    <- L
  env$OUTCOME <- O
  env$EMA_COUNT_DISCONTINUED <- FALSE   # GermanyEMA only; TRUE: count "Medicine discontinued"
  sys.source("Shortage.R", envir = env)
}
cat("\nDone ->", paste0("Results/", EMA_STREAMS, "Shortage", collapse = ", "), "\n")