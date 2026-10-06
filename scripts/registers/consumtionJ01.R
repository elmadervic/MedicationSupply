# ============================================================
# Antibacterial Consumption per Country Over Time
# Data: ECDC Antibacterials for Systemic Use (DDD/1000/day)
# ============================================================

# --- 1. Dependencies -----------------------------------------------------------
if (!requireNamespace("readxl",  quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr",   quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("forcats", quietly = TRUE)) install.packages("forcats")

library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)

# --- 2. Read ALL Excel files from folder ---------------------------------------
data_dir <- "Consumtion"   # <-- change if files are in a different folder

# Pick up every .xls / .xlsx / .xlsm regardless of filename
xlsx_files <- list.files(
  path       = data_dir,
  pattern    = "\\.xlsx?$",          # matches .xls, .xlsx, .xlsm
  full.names = TRUE,
  ignore.case = TRUE                 # handles .XLSX, .Xlsx, etc.
)

# Fallback: also grab files via Sys.glob for case-insensitive systems
if (length(xlsx_files) == 0) {
  xlsx_files <- c(
    Sys.glob(file.path(data_dir, "*.xlsx")),
    Sys.glob(file.path(data_dir, "*.XLSX")),
    Sys.glob(file.path(data_dir, "*.xls")),
    Sys.glob(file.path(data_dir, "*.XLS"))
  )
  xlsx_files <- unique(xlsx_files)
}

if (length(xlsx_files) == 0) {
  stop("No Excel files found in folder: ", normalizePath(data_dir))
}

cat("Found", length(xlsx_files), "file(s):\n")
cat(paste(" -", basename(xlsx_files), collapse = "\n"), "\n\n")

# --- 3. Read and stack all files -----------------------------------------------
read_one <- function(path) {
  df <- read_excel(path, sheet = 1)
  # Normalise to two columns regardless of header text
  names(df) <- c("Country", "DDD_per_1000_per_day")
  # Extract year from filename (first 4-digit number found)
  year <- as.integer(str_extract(basename(path), "\\d{4}"))
  df$Year  <- year
  df$File  <- basename(path)
  df
}

all_data <- bind_rows(lapply(xlsx_files, read_one))

# Drop rows with NA, replace 0 with NA
all_data <- all_data |>
  filter(!is.na(Country), !is.na(DDD_per_1000_per_day)) |>
  mutate(DDD_per_1000_per_day = na_if(DDD_per_1000_per_day, 0))

cat("Years loaded  :", sort(unique(all_data$Year)), "\n")
cat("Countries     :", paste(sort(unique(all_data$Country)), collapse = ", "), "\n\n")

# --- 4. Helpers ----------------------------------------------------------------
country_order <- all_data |>
  group_by(Country) |>
  summarise(mean_ddd = mean(DDD_per_1000_per_day, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(mean_ddd)) |>
  pull(Country)

all_data <- all_data |>
  mutate(Country = factor(Country, levels = country_order))

# --- 5. Plot A – Line chart ----------------------------------------------------
p_lines <- ggplot(all_data,
                  aes(x = Year, y = DDD_per_1000_per_day,
                      colour = Country, group = Country)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = sort(unique(all_data$Year))) +
  scale_colour_viridis_d(option = "turbo") +
  labs(
    title    = "Antibiotic Consumption per Country Over Time",
    subtitle = "Antibacterials for systemic use (ATC group J01)",
    x = "Year", y = "DDD per 1 000 inhabitants per day", colour = "Country",
    caption  = "Source: ECDC Surveillance Atlas of Infectious Diseases"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.key.size  = unit(0.5, "lines"),
    legend.text      = element_text(size = 8),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# ggsave("antibacterial_lines.png", p_lines, width = 14, height = 7, dpi = 150)
# cat("Saved: antibacterial_lines.png\n")

# --- 6. Plot B – Small multiples -----------------------------------------------
p_facet <- ggplot(all_data, aes(x = Year, y = DDD_per_1000_per_day)) +
  geom_line(colour = "#1a6faf", linewidth = 0.7) +
  geom_point(colour = "#1a6faf", size = 1.5) +
  facet_wrap(~ Country, scales = "free_y") +
  scale_x_continuous(breaks = sort(unique(all_data$Year)),
                     labels = function(x) str_sub(x, 3, 4)) +
  labs(
    title    = "Antibiotic Consumption Trend by Country",
    subtitle = "Each panel uses its own y-axis scale",
    x = "Year", y = NULL,
    caption  = "Source: ECDC Surveillance Atlas of Infectious Diseases"
  ) +
  theme_bw(base_size = 10) +
  theme(
    strip.text       = element_text(face = "bold", size = 8),
    axis.text.x      = element_text(size = 7),
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("antibacterial_facets.png", p_facet, width = 16, height = 10, dpi = 150)
cat("Saved: antibacterial_facets.png\n")


all_data <- all_data[complete.cases(all_data),]

# --- 7. Plot C – Heatmap -------------------------------------------------------
p_heat <- ggplot(all_data,
                 aes(x = factor(Year), y = fct_rev(Country),
                     fill = DDD_per_1000_per_day)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  geom_text(aes(label = round(DDD_per_1000_per_day, 1)),
            size = 2.5, colour = "white") +
  scale_fill_viridis_c(option = "plasma", direction = -1,
                       name = "DDD / 1 000\ninhab. / day") +
  labs(
    x = "Year", y = NULL  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold"),
    panel.grid   = element_blank()
  )

ggsave("antibacterial_heatmap.png", p_heat, width = 14, height = 8, dpi = 150)
cat("Saved: antibacterial_heatmap.png\n")

# --- 8. Summary table ----------------------------------------------------------
summary_tbl <- all_data |>
  group_by(Country) |>
  summarise(
    Min   = round(min(DDD_per_1000_per_day,  na.rm = TRUE), 2),
    Max   = round(max(DDD_per_1000_per_day,  na.rm = TRUE), 2),
    Mean  = round(mean(DDD_per_1000_per_day, na.rm = TRUE), 2),
    Trend = {
      fit   <- lm(DDD_per_1000_per_day ~ Year, data = pick(everything()))
      slope <- round(coef(fit)[["Year"]], 3)
      ifelse(slope > 0, paste0("+", slope, "/yr"), paste0(slope, "/yr"))
    },
    .groups = "drop"
  ) |>
  arrange(desc(Mean))

cat("\n--- Summary (ordered by mean consumption) ---\n")
print(summary_tbl, n = Inf)

write.csv(summary_tbl, "antibacterial_summary.csv", row.names = FALSE)
cat("\nSaved: antibacterial_summary.csv\n")





# ============================================================
# EU-level trend plot (average across all countries per year)
# ============================================================

# --- EU average per year -------------------------------------------------------
eu_avg <- all_data |>
  group_by(Year) |>
  summarise(EU_avg = mean(DDD_per_1000_per_day, na.rm = TRUE), .groups = "drop")

# --- Plot: country lines (grey) + EU average (green) --------------------------
p_eu <- ggplot() +
  # Shaded COVID period
  annotate("rect",
           xmin = 2019.5, xmax = 2021.5,
           ymin = -Inf,   ymax = Inf,
           fill = "#E24B4A", alpha = 0.06) +
  annotate("text",
           x = 2020.5, y = Inf, vjust = 1.5,
           label = "COVID-19", size = 3,
           colour = "#A32D2D") +
  # Individual country lines (light grey, thin)
  geom_line(data = all_data,
            aes(x = Year, y = DDD_per_1000_per_day, group = Country),
            colour = "#B4B2A9", linewidth = 0.4, alpha = 0.6, na.rm = TRUE) +
  # EU average ribbon (light fill)
  geom_ribbon(data = eu_avg,
              aes(x = Year,
                  ymin = EU_avg - sd(EU_avg),
                  ymax = EU_avg + sd(EU_avg)),
              fill = "#1D9E75", alpha = 0.12) +
  # EU average line
  geom_line(data = eu_avg,
            aes(x = Year, y = EU_avg),
            colour = "#1D9E75", linewidth = 1.6) +
  geom_point(data = eu_avg,
             aes(x = Year, y = EU_avg),
             colour = "#1D9E75", fill = "white",
             shape = 21, size = 3.5, stroke = 2) +
  # Value labels on EU average points
  geom_text(data = eu_avg,
            aes(x = Year, y = EU_avg,
                label = round(EU_avg, 1)),
            vjust = -1.2, size = 3,
            colour = "#0F6E56", fontface = "bold") +
  scale_x_continuous(breaks = sort(unique(all_data$Year))) +
  scale_y_continuous(limits = c(6, 38),
                     breaks = seq(5, 35, by = 5)) +
  labs(
    x        = "Year",
    y        = "DDD per 1 000 inhabitants per day"  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92")
  )

ggsave("eu_average_trend.png", p_eu,
       width = 12, height = 6, dpi = 150)
cat("Saved: eu_average_trend.png\n")