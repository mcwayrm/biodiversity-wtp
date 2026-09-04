# scripts/R/plot_core_wtp_figure.R
# ===========================================================================
# CORE WTP FIGURE
# ===========================================================================
# Publication-style 3-panel WTP figure, standalone from the interactive
# summary_report.qmd (this is a controlled, exportable output for slides/
# papers, not another chart embedded in the HTML report).
#
#   Top-left:     ER model        -- Expected Richness
#   Top-right:    ERmigrant model -- Migrant Richness, Resident Richness
#   Bottom (full width): ERmigrantseason model -- Migrant and Resident
#                 richness broken out by Indian climatic season, grouped BY
#                 SEASON (Resident/Migrant paired within each season) rather
#                 than by species group.
#
# Reads the *_wtp.csv files already produced by estimate_rum_xlogit.py --
# no re-estimation, this is pure post-processing/plotting.
#
# NOTE: uses the patchwork package for the (top-left | top-right) / bottom
# layout. Install if needed: install.packages("patchwork")

library(dplyr)
library(ggplot2)
library(patchwork)

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
SCENARIO <- "above_median_c5km_v5km_r50km_mInf"
MODELS_DIR <- file.path("output", "models")

MODEL_ER                <- "ER_indxyear-sitexseason-hour_choiceset10"
MODEL_ER_CONDITIONAL    <- "ERconditional_indxyear-sitexseason-hour_choiceset10"
MODEL_ER_MIGRANT        <- "ERmigrant_indxyear-sitexseason-hour_choiceset10"
MODEL_ER_MIGRANT_SEASON <- "ERmigrantseason_indxyear-sitexseason-hour_choiceset10"

OUTPUT_PATH <- file.path("output", "figures", "core_wtp_figure.png")

# Set TRUE to include panel titles (A./B./C.) and the overall title/subtitle
# (scenario label). Default FALSE so titles can be added in LaTeX instead.
# Axis titles (y = "WTP (INR)") and category labels are shown either way --
# this toggle only affects the descriptive titles, not the axes.
SHOW_TITLES <- FALSE

# Set TRUE for one shared, 0-centered y-axis range across ALL THREE panels.
# Set FALSE for each panel to get its own 0-centered range sized to its own
# data. NOTE: this flag is currently unused pending clarification on the
# dual-axis request (see bottom of file) -- left in place since the
# non-dual-axis fallback still uses it.
SHARED_Y_RANGE <- TRUE

# Indian climatic seasons don't map onto the code's Western 4-season labels
# (Winter/Spring/Summer/Fall) the way the names suggest -- this is display-
# only, the underlying data/model still uses the original column suffixes.
SEASON_DISPLAY <- c(
  Winter = "Winter\n(Dec-Feb)",
  Spring = "Summer\n(Mar-May)",
  Summer = "Monsoon\n(Jun-Aug)",
  Fall   = "Post-Monsoon\n(Sep-Nov)"
)
SEASON_ORDER <- c("Winter\n(Dec-Feb)", "Summer\n(Mar-May)", "Monsoon\n(Jun-Aug)", "Post-Monsoon\n(Sep-Nov)")

# ---------------------------------------------------------------------------
# Load
# ---------------------------------------------------------------------------
load_wtp <- function(model_name, model_type = "Mixed") {
  path <- file.path(MODELS_DIR, sprintf("%s_%s_%s_wtp.csv", model_name, model_type, SCENARIO))
  if (!file.exists(path)) {
    stop(sprintf("WTP file not found: %s\n(check MODEL_* names in this script match models.yml)", path))
  }
  read.csv(path, stringsAsFactors = FALSE) %>%
    rename(variable = Variable, estimate = WTP, se = Std.Err) %>%
    mutate(
      variable = gsub("_dm$", "", variable),
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se
    )
}

# Optional: doesn't stop() on a missing file -- lets the figure render with
# just the Mixed-logit point if the conditional-logit companion model
# hasn't been estimated yet, rather than failing the whole script.
load_wtp_optional <- function(model_name, model_type) {
  path <- file.path(MODELS_DIR, sprintf("%s_%s_%s_wtp.csv", model_name, model_type, SCENARIO))
  if (!file.exists(path)) {
    message(sprintf("NOTE: %s not found yet -- Panel A will show Mixed Logit only until this model is estimated", basename(path)))
    return(NULL)
  }
  load_wtp(model_name, model_type)
}

wtp_er <- load_wtp(MODEL_ER)
wtp_er_conditional <- load_wtp_optional(MODEL_ER_CONDITIONAL, "Conditional")
wtp_migrant <- load_wtp(MODEL_ER_MIGRANT)
wtp_migrant_season <- load_wtp(MODEL_ER_MIGRANT_SEASON)

# Separate width per panel, not one shared value -- ggplot's discrete axis
# allocates each category an equal DATA-UNIT slot regardless of how many
# total categories share the panel, so the same numeric width maps to a
# different absolute pixel size depending on category count AND panel
# physical width. Panel A (1 category, half-width panel) and Panel C (4
# season-groups further halved by the Migrant/Resident dodge, full-width
# panel) both end up with effectively denser slots than Panel B (2
# categories, half-width panel) -- hence both roughly half of B's value.
# These are reasoned starting points based on the panel dimensions below,
# not verified against an actual render -- nudge by eye if still off.
ERRORBAR_WIDTH_A <- 0.03
ERRORBAR_WIDTH_B <- 0.05
ERRORBAR_WIDTH_C <- 0.09
base_theme <- theme_minimal(base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 20),
    axis.text.y = element_text(size = 20),
    plot.title = element_text(face = "bold", size = 20),
    legend.position = "none",
    legend.text = element_text(size = 20),
  )

panel_title <- function(text) if (SHOW_TITLES) text else NULL

# Point + capped error bar + value label, in one layer set (item 2 and 5)
wtp_point_layers <- function(width) {
  list(
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4),
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = width, linewidth = 0.7),
    geom_point(size = 2.6),
    geom_text(aes(y = ci_upper, label = sprintf("%.3f", estimate)),
              vjust = -0.7, size = 5.2, show.legend = FALSE)
  )
}

# ---------------------------------------------------------------------------
# Panel A (top-left): ER model -- Expected Richness. Shows Mixed Logit vs.
# Conditional Logit once both are available; falls back to Mixed Logit only
# if the conditional-logit companion model hasn't been estimated yet.
# ---------------------------------------------------------------------------
if (!is.null(wtp_er_conditional)) {
  panel_a_data <- bind_rows(
    wtp_er %>% filter(variable == "expected_richness") %>% mutate(group = "Mixed Logit"),
    wtp_er_conditional %>% filter(variable == "expected_richness") %>% mutate(group = "Conditional Logit")
  ) %>%
    mutate(
      label = recode(group, "Mixed Logit" = "Mixed\nLogit", "Conditional Logit" = "Conditional\nLogit"),
      label = factor(label, levels = c("Conditional\nLogit", "Mixed\nLogit"))
    )

  panel_a <- ggplot(panel_a_data, aes(x = label, y = estimate, color = group)) +
    wtp_point_layers(ERRORBAR_WIDTH_B) +  # same 2-category geometry as panel B now
    scale_color_manual(values = c("Mixed Logit" = "#2c7fb8", "Conditional Logit" = "#756bb1")) +
    labs(title = panel_title("A. Overall Richness: Mixed vs. Conditional Logit"), x = NULL, y = "WTP (INR)") +
    base_theme
} else {
  panel_a_data <- wtp_er %>%
    filter(variable == "expected_richness") %>%
    mutate(label = "Species\nRichness")

  panel_a <- ggplot(panel_a_data, aes(x = label, y = estimate)) +
    wtp_point_layers(ERRORBAR_WIDTH_A) +
    labs(title = panel_title("A. Overall Richness"), x = NULL, y = "WTP (INR)") +
    base_theme
}

# ---------------------------------------------------------------------------
# Panel B (top-right): ERmigrant model -- Migrant, Resident
# ---------------------------------------------------------------------------
panel_b_data <- wtp_migrant %>%
  filter(variable %in% c("migrant_richness", "resident_richness")) %>%
  mutate(
    label = recode(variable,
                    migrant_richness = "Migrant\nRichness",
                    resident_richness = "Resident\nRichness"),
    label = factor(label, levels = c("Resident\nRichness", "Migrant\nRichness")),
    group = recode(variable,
                    migrant_richness = "Migrant",
                    resident_richness = "Resident")
  )

panel_b <- ggplot(panel_b_data, aes(x = label, y = estimate, color = group)) +
  wtp_point_layers(ERRORBAR_WIDTH_B) +
  scale_color_manual(values = c("Migrant" = "#d95f0e", "Resident" = "#31a354")) +
  labs(title = panel_title("B. Migrant vs. Resident Richness"), x = NULL, y = "WTP (INR)") +
  base_theme

# ---------------------------------------------------------------------------
# Panel C (bottom, full width): ERmigrantseason -- grouped BY SEASON
# (Resident/Migrant paired within each season, not all-Migrant-then-all-
# Resident). Needs a legend now, since the x-axis no longer distinguishes
# Migrant vs. Resident on its own.
# ---------------------------------------------------------------------------
season_var_map <- c(
  migrant_richness = "Summer", migrant_richness_Fall = "Fall",
  migrant_richness_Winter = "Winter", migrant_richness_Spring = "Spring",
  resident_richness = "Summer", resident_richness_Fall = "Fall",
  resident_richness_Winter = "Winter", resident_richness_Spring = "Spring"
)

panel_c_data <- wtp_migrant_season %>%
  filter(variable %in% names(season_var_map)) %>%
  mutate(
    season_code = season_var_map[variable],
    season = SEASON_DISPLAY[season_code],
    season = factor(season, levels = SEASON_ORDER),
    # Resident first in factor levels -> dodges to the left within each
    # season pair, Migrant second -> dodges to the right
    group = ifelse(grepl("^migrant", variable), "Migrant", "Resident"),
    group = factor(group, levels = c("Resident", "Migrant"))
  )

panel_c <- ggplot(panel_c_data, aes(x = season, y = estimate, color = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = ERRORBAR_WIDTH_C, linewidth = 0.7,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 2.6, position = position_dodge(width = 0.5)) +
  geom_text(aes(y = ci_upper, label = sprintf("%.3f", estimate)),
            position = position_dodge(width = 0.5), vjust = -0.7, size = 5.2, show.legend = FALSE) +
  scale_color_manual(values = c("Migrant" = "#d95f0e", "Resident" = "#31a354")) +
  labs(title = panel_title("C. Migrant vs. Resident Richness by Season (ref: Monsoon)"),
       x = NULL, y = "WTP (INR)", color = NULL) +
  base_theme +
  theme(legend.position = "top")

# ---------------------------------------------------------------------------
# Axis ranges: ONE shared, symmetric, 0-centered LHS range across panels A,
# B, and Panel C's Resident points. Panel C additionally gets a RHS axis
# (via sec_axis -- an alternate LABELING of the same coordinate space, not a
# truly independent scale) sized to Migrant's actual range, since Migrant's
# seasonal swings are much wider than everything else in the figure.
#
# Mechanism: Migrant's points in panel C are rescaled by (lhs_max/rhs_max)
# so they plot in the same visual space as Resident/A/B; sec_axis()'s
# inverse transform makes the RHS tick labels show migrant's TRUE values at
# those rescaled positions. Point-label TEXT always shows the true,
# un-rescaled estimate -- only the plotted Y position is transformed.
# ---------------------------------------------------------------------------
panel_y_max <- function(...) {
  dfs <- list(...)
  vals <- unlist(lapply(dfs, function(d) c(d$ci_lower, d$ci_upper)))
  max(abs(vals), na.rm = TRUE)
}

USE_DUAL_AXIS <- TRUE  # FALSE falls back to one shared range across everything, no rescaling

if (USE_DUAL_AXIS) {
  panel_c_resident <- panel_c_data %>% filter(group == "Resident")
  panel_c_migrant  <- panel_c_data %>% filter(group == "Migrant")

  lhs_max <- panel_y_max(panel_a_data, panel_b_data, panel_c_resident) * 1.15  # a little headroom for labels
  rhs_max <- panel_y_max(panel_c_migrant) * 1.15
  rescale_factor <- lhs_max / rhs_max

  panel_a <- panel_a + coord_cartesian(ylim = c(-lhs_max, lhs_max))
  panel_b <- panel_b + coord_cartesian(ylim = c(-lhs_max, lhs_max))

  # Rebuild panel C with Migrant's Y values rescaled into the LHS coordinate
  # space; Resident stays untransformed (matches "Resident -> left axis,
  # Migrant -> right axis")
  panel_c_data <- panel_c_data %>%
    mutate(
      plot_estimate = ifelse(group == "Migrant", estimate * rescale_factor, estimate),
      plot_ci_lower = ifelse(group == "Migrant", ci_lower * rescale_factor, ci_lower),
      plot_ci_upper = ifelse(group == "Migrant", ci_upper * rescale_factor, ci_upper)
    )

  panel_c <- ggplot(panel_c_data, aes(x = season, y = plot_estimate, color = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
    geom_errorbar(aes(ymin = plot_ci_lower, ymax = plot_ci_upper), width = ERRORBAR_WIDTH_C, linewidth = 0.7,
                  position = position_dodge(width = 0.5)) +
    geom_point(size = 2.6, position = position_dodge(width = 0.5)) +
    geom_text(aes(y = plot_ci_upper, label = sprintf("%.3f", estimate)),
              position = position_dodge(width = 0.5), vjust = -0.7, size = 5.2, show.legend = FALSE) +
    scale_color_manual(values = c("Migrant" = "#d95f0e", "Resident" = "#31a354")) +
    scale_y_continuous(
      limits = c(-lhs_max, lhs_max),
      sec.axis = sec_axis(~ . / rescale_factor, name = "WTP (INR): Migrant")
    ) +
    labs(title = panel_title("C. Migrant vs. Resident Richness by Season (ref: Monsoon)"),
         x = NULL, y = "WTP (INR): Resident", color = NULL) +
    base_theme +
    theme(legend.position = "bottom")

} else if (SHARED_Y_RANGE) {
  y_max_all <- panel_y_max(panel_a_data, panel_b_data, panel_c_data)
  panel_a <- panel_a + coord_cartesian(ylim = c(-y_max_all, y_max_all))
  panel_b <- panel_b + coord_cartesian(ylim = c(-y_max_all, y_max_all))
  panel_c <- panel_c + coord_cartesian(ylim = c(-y_max_all, y_max_all))
} else {
  panel_a <- panel_a + coord_cartesian(ylim = c(-panel_y_max(panel_a_data), panel_y_max(panel_a_data)))
  panel_b <- panel_b + coord_cartesian(ylim = c(-panel_y_max(panel_b_data), panel_y_max(panel_b_data)))
  panel_c <- panel_c + coord_cartesian(ylim = c(-panel_y_max(panel_c_data), panel_y_max(panel_c_data)))
}

# ---------------------------------------------------------------------------
# Combine: (A | B) / C
# ---------------------------------------------------------------------------
final_figure <- (panel_a | panel_b) / panel_c +
  plot_layout(heights = c(1, 1.1))

if (SHOW_TITLES) {
  final_figure <- final_figure +
    plot_annotation(
      title = "Willingness to Pay for Biodiversity",
      subtitle = sprintf("Scenario: %s | Point estimates with 95%% confidence intervals", SCENARIO),
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )
}

if (!dir.exists(dirname(OUTPUT_PATH))) dir.create(dirname(OUTPUT_PATH), recursive = TRUE)
ggsave(OUTPUT_PATH, final_figure, width = 12, height = 10, dpi = 300)
message(sprintf("Saved: %s", OUTPUT_PATH))