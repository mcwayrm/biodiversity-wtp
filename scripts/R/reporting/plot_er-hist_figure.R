# scripts/R/plot_core_wtp_figure.R
# ===========================================================================
# CORE WTP FIGURE
# ===========================================================================
# [... existing header comments ...]

library(dplyr)
library(ggplot2)
library(patchwork)
library(data.table)  # ADD THIS for choice-set histograms
library(arrow)       # ADD THIS for reading parquet files

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
SCENARIO <- "full_c5km_v5km_r50km_mInf"
CHOICESET_DATA_DIR <- file.path("data", "intermediate", "scenarios", SCENARIO)

MODEL_ER                <- "ER_indxyear-sitexseason-hour_choiceset10"
MODEL_ER_CONDITIONAL    <- "ERconditional_indxyear-sitexseason-hour_choiceset10"
MODEL_ER_MIGRANT        <- "ERmigrant_indxyear-sitexseason-hour_choiceset10"
MODEL_ER_MIGRANT_SEASON <- "ERmigrantseason_indxyear-sitexseason-hour_choiceset10"

OUTPUT_PATH <- file.path("output", "figures", "er_histogram_figure.png")
SHOW_TITLES <- FALSE

# ADD: Histogram-specific config
HISTOGRAM_BINS <- 30
HISTOGRAM_ALPHA <- c(
  "Expected Richness" = 1.0,
  "Migrant Richness" = 0.5,
  "Resident Richness" = 0.5
)
HISTOGRAM_COLORS <- c(
  "Expected Richness" = "#000000",
  "Migrant Richness" = "#ff7f0e",
  "Resident Richness" = "#2ca02c"
)

# ---------------------------------------------------------------------------
# Load choice-set histogram data
# ---------------------------------------------------------------------------
load_choiceset_data <- function(scenario_dir) {
  parquet_file <- file.path(scenario_dir, "master_data_with_attributes.parquet")
  
  if (length(parquet_file) == 0) {
    stop("No choice-set parquet file found in ", scenario_dir)
  }
  
  # Read the first matching file
  read_parquet(parquet_file, col_select = c("user_id", "expected_richness", "migrant_richness", "resident_richness")) %>%
    as.data.frame()
}

choiceset_data <- load_choiceset_data(CHOICESET_DATA_DIR)

# ---------------------------------------------------------------------------
# Base theme (existing, unchanged)
# ---------------------------------------------------------------------------
base_theme <- theme_minimal(base_size = 24) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 24),
    axis.text.y = element_text(size = 24),
    plot.title = element_text(face = "bold", size = 24),
      # 1. Tell ggplot the legend goes inside the panel
    legend.position = "inside",
    # 2. Place it at the maximum X and Y coordinates (top-right)
    legend.position.inside = c(0.8, 0.8),
    # 3. Anchor the top-right corner of the legend box to that spot
    legend.justification = c("right", "top"),
    legend.text = element_text(size = 24),
    legend.box = "vertical",
    legend.title.position = "top",
    legend.title = element_text(face = "bold")
  )

panel_title <- function(text) if (SHOW_TITLES) text else NULL

# ---------------------------------------------------------------------------
# Histogram of choice-set variation
# ---------------------------------------------------------------------------
hist <- NULL

if (!is.null(choiceset_data)) {
  metric_labels <- c(
    expected_richness = "Expected Richness",
    migrant_richness = "Migrant Richness",
    resident_richness = "Resident Richness"
  )
  
  metrics_present <- intersect(names(metric_labels), names(choiceset_data))
  
  if (length(metrics_present) > 0) {
    # Compute per-user SD across full candidate choice sets
    setDT(choiceset_data)
    sd_by_user <- choiceset_data[, 
      lapply(.SD, function(x) sd(x, na.rm = TRUE)), 
      by = user_id, 
      .SDcols = metrics_present
    ]
    
    # Convert to long format
    long_rows <- lapply(metrics_present, function(m) {
      data.frame(
        value = sd_by_user[[m]],
        metric = metric_labels[[m]]
      )
    })
    long_data <- do.call(rbind, long_rows)
    long_data <- long_data[!is.na(long_data$value), ]
    long_data$metric <- factor(long_data$metric, 
                               levels = unname(metric_labels[metrics_present]))
    
    # Build overlaid histogram
    hist <- ggplot(long_data, aes(x = value, fill = metric, alpha = metric)) +
      geom_histogram(bins = HISTOGRAM_BINS, color = NA, position = "identity") +
      scale_fill_manual(values = HISTOGRAM_COLORS) +
      scale_alpha_manual(values = HISTOGRAM_ALPHA) +
      labs(
        title = panel_title("Within-Individual Richness Variation Across Choice Sets"),
        x = "SD of Expected Richness",
        y = "Number of individuals",
        fill = "Richness Metric",
        alpha = "Richness Metric"
      ) +
      base_theme +
      guides(color = guide_legend(ncol = 1), fill = guide_legend(ncol = 1))
  }
}

if (!dir.exists(dirname(OUTPUT_PATH))) dir.create(dirname(OUTPUT_PATH), recursive = TRUE)
ggsave(OUTPUT_PATH, hist, width = 12, height = 13, dpi = 300)
message(sprintf("Saved: %s", OUTPUT_PATH))
