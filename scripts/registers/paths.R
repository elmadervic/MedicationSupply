RAW_DIR         <- "data/registers/raw"
PROC_DIR        <- "data/registers/processed"
SHORTAGE_DIR    <- "data/shortages"
CONSUMPTION_DIR <- "data/consumption"
RESULTS_DIR     <- "results"
FIG_DIR         <- "figures"
ADD_DIR         <- "figures/additional"
TAB_DIR         <- "results/tables"
for (d in c(PROC_DIR, RESULTS_DIR, FIG_DIR, ADD_DIR, TAB_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
