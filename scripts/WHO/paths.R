RAW_DIR  <- "raw"
PROC_DIR <- "processed"
FIG_DIR  <- "figures"
ADD_DIR  <- "figures/additional"
TAB_DIR  <- "results/tables"
for (d in c(PROC_DIR, FIG_DIR, ADD_DIR, TAB_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
