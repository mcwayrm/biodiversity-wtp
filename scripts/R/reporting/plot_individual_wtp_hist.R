# scripts/R/plot_individual_wtp_hist.R
# ===========================================================================
# INDIVIDUAL WTP HISTOGRAM
# ===========================================================================
# Overlaid histogram of the per-user_id conditional WTP estimates produced
# by scripts/python/12_individual_wtp.py (via run_all.R Stage 12), across
# the Full / Above-Median / Below-Median samples -- pure post-processing/
# plotting, no computation of WTP happens here.
#
# Each sample also gets a vertical reference line at its own POPULATION WTP
# estimate (from that sample's *_wtp.csv), so each distribution's spread
# around its own population point is visible.

library(dplyr)
library(ggplot2)
library(arrow)

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
MODELS_DIR <- file.path("output", "models")

MODEL_ER <- "ER_indxyear-sitexseason-hour_choiceset10"
MODEL_TYPE <- "Mixed"

# One entry per sample shown in the overlay -- ADD/REMOVE samples here.
SAMPLES <- list(
  list(sample = "full_c5km_v5km_r50km_mInf", label = "Full"),
  list(sample = "above_median_c5km_v5km_r50km_mInf", label = "Above Median"),
  list(sample = "below_median_c5km_v5km_r50km_mInf", label = "Below Median")
)
SAMPLE_LEVELS <- vapply(SAMPLES, function(s) s$label, character(1))

SAMPLE_COLORS <- c(
  "Full" = "#2c7fb8",
  "Above Median" = "#d95f0e",
  "Below Median" = "#31a354"
)

OUTPUT_PATH <- file.path("output", "figures", "individual_wtp_histogram.png")
SHOW_TITLES <- FALSE
HISTOGRAM_BINS <- 30
HISTOGRAM_ALPHA <- 0.5

# ---------------------------------------------------------------------------
# Load individual + population WTP for each sample
# ---------------------------------------------------------------------------
load_sample <- function(sample_spec) {
  output_prefix <- sprintf("%s_%s_%s", MODEL_ER, MODEL_TYPE, sample_spec$sample)

  individual_path <- file.path(MODELS_DIR, sprintf("%s_individual_wtp.parquet", output_prefix))
  if (!file.exists(individual_path)) {
    message("NOTE: individual WTP not found, skipping sample '", sample_spec$label, "': ", individual_path,
            "\n(run scripts/python/12_individual_wtp.py for this scenario first)")
    return(NULL)
  }
  individual_wtp <- read_parquet(individual_path) %>%
    as.data.frame() %>%
    mutate(sample = sample_spec$label)

  population_wtp <- NA_real_
  population_path <- file.path(MODELS_DIR, sprintf("%s_wtp.csv", output_prefix))
  if (file.exists(population_path)) {
    pop_df <- read.csv(population_path, stringsAsFactors = FALSE)
    pop_row <- pop_df[pop_df$Variable == "expected_richness_dm", ]
    if (nrow(pop_row) == 1) population_wtp <- pop_row$WTP[1]
  } else {
    message("NOTE: population WTP file not found for sample '", sample_spec$label, "': ", population_path)
  }

  list(individual_wtp = individual_wtp,
       population_wtp = data.frame(sample = sample_spec$label, wtp = population_wtp))
}

loaded <- lapply(SAMPLES, load_sample)
loaded <- Filter(Negate(is.null), loaded)

if (length(loaded) == 0) {
  stop("No individual WTP data found for any configured sample -- nothing to plot.")
}

individual_wtp <- bind_rows(lapply(loaded, `[[`, "individual_wtp")) %>%
  mutate(sample = factor(sample, levels = SAMPLE_LEVELS))

population_wtp <- bind_rows(lapply(loaded, `[[`, "population_wtp")) %>%
  mutate(sample = factor(sample, levels = SAMPLE_LEVELS)) %>%
  filter(!is.na(wtp))

# ---------------------------------------------------------------------------
# Base theme (matches plot_er-hist_figure.R / plot_core_wtp_figure.R)
# ---------------------------------------------------------------------------
base_theme <- theme_minimal(base_size = 24) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 24),
    axis.text.y = element_text(size = 24),
    plot.title = element_text(face = "bold", size = 24),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.8),
    legend.justification = c("right", "top"),
    legend.text = element_text(size = 24),
    legend.title = element_text(face = "bold")
  )

panel_title <- function(text) if (SHOW_TITLES) text else NULL

# ---------------------------------------------------------------------------
# Overlaid histogram of individual WTP, by sample
# ---------------------------------------------------------------------------
hist <- ggplot(individual_wtp, aes(x = wtp_richness_2021inr, fill = sample)) +
  geom_histogram(bins = HISTOGRAM_BINS, alpha = HISTOGRAM_ALPHA, color = NA, position = "identity") +
  scale_fill_manual(values = SAMPLE_COLORS)

if (nrow(population_wtp) > 0) {
  hist <- hist +
    geom_vline(data = population_wtp, aes(xintercept = wtp, color = sample),
               linetype = "dashed", linewidth = 0.8, show.legend = FALSE) +
    scale_color_manual(values = SAMPLE_COLORS)
}

hist <- hist +
  labs(
    title = panel_title("Individual WTP for Species Richness"),
    x = "WTP (2021 INR)",
    y = "Number of individuals",
    fill = "Sample"
  ) +
  base_theme

if (!dir.exists(dirname(OUTPUT_PATH))) dir.create(dirname(OUTPUT_PATH), recursive = TRUE)
ggsave(OUTPUT_PATH, hist, width = 12, height = 8, dpi = 300)
message(sprintf("Saved: %s", OUTPUT_PATH))
