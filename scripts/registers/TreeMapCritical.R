# Treemap of critical APIs by ATC Level 1 chapter
# Input: critical.csv with one column "ATC level 5" (blank rows allowed)

library(readr)
library(dplyr)
library(ggplot2)
library(treemapify)   # install.packages("treemapify")

# ---- Data -------------------------------------------------------------------
atc <- read_csv("Data/critical.csv", col_types = cols(.default = "c")) |>
  rename(code = `ATC level 5`) |>
  mutate(code = trimws(code)) |>
  filter(!is.na(code), code != "") |>
  distinct(code)                       # count unique ATC codes

chapter_names <- c(
  A = "Alimentary & metabolism",
  B = "Blood & blood forming organs",
  C = "Cardiovascular system",
  G = "Genito-urinary system",
  H = "Systemic hormonal preparations",
  J = "Antiinfectives (systemic)",
  L = "Antineoplastic & immunomodulating",
  M = "Musculo-skeletal system",
  N = "Nervous system",
  P = "Antiparasitic products",
  R = "Respiratory system",
  S = "Sensory organs",
  V = "Various"
)

chapter_cols <- c(
  A = "#A6CBE8", B = "#2F5A8C", C = "#A8D8B5", G = "#4FA65A",
  H = "#E0857A", J = "#A33228", L = "#DFA670", M = "#D06C2E",
  N = "#C6A9D8", P = "#4B2470", R = "#D3B53F", S = "#5C3A1A",
  V = "#9A9A9A"
)

tm <- atc |>
  mutate(chapter = substr(code, 1, 1)) |>
  count(chapter, name = "n") |>
  mutate(
    pct   = n / sum(n),
    name  = chapter_names[chapter],
    label = sprintf("%s\n–\n%s\n%d\n(%s)",
                    chapter, name, n, scales::percent(pct, accuracy = 1)),
    legend_lab = paste(chapter, "-", name)
  ) |>
  arrange(desc(n))

print(tm |> select(chapter, name, n, pct))

# ---- Plot -------------------------------------------------------------------
p <- ggplot(tm, aes(area = n, fill = chapter, label = label)) +
  geom_treemap(colour = "white", size = 1.5) +
  geom_treemap_text(
    colour = "white", fontface = "bold", place = "centre",
    grow = TRUE, reflow = TRUE, min.size = 3, padding.x = grid::unit(2, "mm"),
    padding.y = grid::unit(2, "mm")
  ) +
  scale_fill_manual(
    values = chapter_cols,
    labels = setNames(tm$legend_lab, tm$chapter),
    breaks = names(chapter_cols),
    name   = "ATC chapter"
  ) +
  theme_void(base_size = 11) +
  theme(
    legend.position  = "right",
    legend.key.size  = unit(0.45, "cm"),
    legend.title     = element_text(face = "plain", size = 11),
    legend.text      = element_text(size = 8.5),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

ggsave("atc_treemap.png", p, width = 10, height = 7, dpi = 300)
ggsave("atc_treemap.pdf", p, width = 10, height = 7)
p