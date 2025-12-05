#!/usr/bin/env Rscript
# scripts/R/11_generate_scenario_outputs.R
#############################################
#  - Generates scenario-specific diagnostic outputs
#  - Creates Voronoi polygon map
#  - Generates WTP comparison table across all models
#  - Creates data summary statistics
#  - Saves: outputs$voronoi_plot, outputs$wtp_table, outputs$data_summary
#############################################

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------

message("Loading scenario data...")

# Load Voronoi polygons
voronoi_unlimited <- st_read(inputs$voronoi_shp,
                             layer = "cluster_voronoi_unlimited",
                             quiet = TRUE)
voronoi_limited <- st_read(inputs$voronoi_shp,
                           layer = "cluster_voronoi_limited",
                           quiet = TRUE)

# Load clustered hotspots
hotspots <- read_parquet(inputs$hotspots_clustered)

# Load cleaned data
data_clean <- read_parquet(inputs$master_data_final)
setDT(data_clean)

message("Loaded ", nrow(voronoi_limited), " Voronoi polygons")
message("Loaded ", nrow(hotspots), " clustered hotspots")
message("Loaded ", nrow(data_clean), " clean observations")

# -----------------------------------------------------------------------------
# Generate Voronoi Map
# -----------------------------------------------------------------------------

message("\n--- Creating Voronoi map ---")

# Load India boundaries for context
india_boundary <- st_read(inputs$district_shp, quiet = TRUE) %>%
  st_union() %>%
  st_transform(crs = 4326)

# Create map
p <- ggplot() +
  # India boundary
  geom_sf(data = india_boundary, fill = NA, color = "gray30", linewidth = 0.8) +
  # Voronoi polygons
  geom_sf(data = st_transform(voronoi_limited, 4326), 
          fill = "lightblue", alpha = 0.3, color = "blue", linewidth = 0.3) +
  # Hotspot points
  geom_point(data = hotspots, aes(x = lon, y = lat), 
            color = "red", size = 0.8, alpha = 0.6) +
  labs(
    title = "Voronoi Clusters and Hotspot Locations",
    subtitle = paste0("Scenario: ", basename(dirname(dirname(inputs$voronoi_shp))),
                    " | ", nrow(voronoi_limited), " clusters, ",
                    nrow(hotspots), " hotspots"),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# Save plot
ggsave(outputs$voronoi_plot, p, width = 10, height = 12, dpi = 300)
message("Voronoi map saved to: ", outputs$voronoi_plot)

# -----------------------------------------------------------------------------
# Load Model Results
# -----------------------------------------------------------------------------

message("\n--- Loading model results ---")

model_basic <- readRDS(inputs$model_basic)
model_fe <- readRDS(inputs$model_fe)
model_mixed <- readRDS(inputs$model_mixed)

# -----------------------------------------------------------------------------
# Generate WTP Comparison Table
# -----------------------------------------------------------------------------

message("\n--- Creating WTP comparison table ---")

extract_wtp <- function(model_obj, model_name) {
  empty_df <- data.frame(
    variable = character(),
    wtp = numeric(),
    se = numeric(),
    model = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(model_obj$wtp)) return(empty_df)
  
  wtp_raw <- model_obj$wtp
  
  # handle manual-style list with Estimate element
  if (is.list(wtp_raw) && "method" %in% names(wtp_raw) && wtp_raw$method == "manual") {
    est_raw <- wtp_raw$Estimate
    if (is.null(est_raw)) return(empty_df)
    if (is.matrix(est_raw) || is.data.frame(est_raw)) {
      est <- as.numeric(est_raw[,1])
      vars <- rownames(est_raw)
    } else {
      est <- as.numeric(est_raw)
      vars <- names(est_raw)
    }
    se <- rep(NA_real_, length(est))
  } else {
    # standard: could be data.frame/matrix with Estimate and optionally StdError,
    # or a named numeric vector
    if (is.data.frame(wtp_raw) || is.matrix(wtp_raw)) {
      wtp_df <- as.data.frame(wtp_raw, stringsAsFactors = FALSE)
      if ("Estimate" %in% colnames(wtp_df)) {
        est <- as.numeric(wtp_df$Estimate)
      } else {
        # fall back to first numeric column
        numcols <- which(sapply(wtp_df, is.numeric))
        if (length(numcols) == 0) return(empty_df)
        est <- as.numeric(wtp_df[[numcols[1]]])
      }
      if ("StdError" %in% colnames(wtp_df)) {
        se <- as.numeric(wtp_df$StdError)
      } else {
        se <- rep(NA_real_, length(est))
      }
      vars <- rownames(wtp_df)
    } else if (is.numeric(wtp_raw) && !is.null(names(wtp_raw))) {
      est <- as.numeric(wtp_raw)
      se <- rep(NA_real_, length(est))
      vars <- names(wtp_raw)
    } else {
      return(empty_df)
    }
  }
  
  # ensure lengths align
  n <- length(est)
  if (is.null(vars) || length(vars) != n) vars <- paste0("V", seq_len(n))
  if (length(se) == 1 && n > 1) se <- rep(se, n)
  if (length(se) != n) se <- rep(NA_real_, n)
  
  data.frame(
    variable = vars,
    wtp = est,
    se = se,
    model = model_name,
    stringsAsFactors = FALSE
  )
}


# WTP Models
wtp_basic_df <- extract_wtp(model_basic, "Basic")
wtp_fe_df <- extract_wtp(model_fe, "Fixed Effects")
wtp_mixed_df <- extract_wtp(model_mixed, "Mixed Logit")

# Combine all WTP estimates
wtp_all <- rbind(wtp_basic_df, wtp_fe_df, wtp_mixed_df)

# Pivot wider for comparison table
wtp_comparison <- wtp_all %>%
  select(variable, wtp, model) %>%
  pivot_wider(names_from = model, values_from = wtp) %>%
  arrange(variable)

# Add standard errors in separate columns if available
if (any(!is.na(wtp_all$se))) {
  wtp_se <- wtp_all %>%
    filter(!is.na(se)) %>%
    select(variable, se, model) %>%
    pivot_wider(names_from = model, values_from = se, names_prefix = "SE_")
  
  wtp_comparison <- left_join(wtp_comparison, wtp_se, by = "variable")
}

# Save WTP table
write.csv(wtp_comparison, outputs$wtp_table, row.names = FALSE)
message("WTP comparison table saved to: ", outputs$wtp_table)

print(wtp_comparison)

# -----------------------------------------------------------------------------
# Generate Data Summary Statistics
# -----------------------------------------------------------------------------

message("\n--- Creating data summary ---")

# Variable list
summary_vars <- c("expected_richness", "expected_congestion", "precip", "temp",
                  "trees", "travel_cost_combined", "dist_to_pa_km", "geo_dist")

# Calculate summary statistics
summary_stats <- data_clean[, lapply(.SD, function(x) {
  list(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    n_missing = sum(is.na(x)),
    pct_missing = 100 * sum(is.na(x)) / length(x)
  )
}), .SDcols = intersect(summary_vars, names(data_clean))]

# Convert to long format
summary_df <- data.frame(
  variable = rep(names(summary_stats), each = 7),
  statistic = rep(c("mean", "sd", "min", "median", "max", "n_missing", "pct_missing"),
                  length(summary_stats)),
  value = unlist(summary_stats)
)

# Pivot wider
summary_table <- summary_df %>%
  pivot_wider(names_from = statistic, values_from = value)

# Add sample size info
summary_table <- rbind(
  data.frame(
    variable = "Sample Size",
    mean = nrow(data_clean),
    sd = NA, min = NA, median = NA, max = NA,
    n_missing = NA, pct_missing = NA
  ),
  data.frame(
    variable = "Number of Trips",
    mean = data_clean[, uniqueN(trip_id)],
    sd = NA, min = NA, median = NA, max = NA,
    n_missing = NA, pct_missing = NA
  ),
  data.frame(
    variable = "Number of Users",
    mean = data_clean[, uniqueN(user_id)],
    sd = NA, min = NA, median = NA, max = NA,
    n_missing = NA, pct_missing = NA
  ),
  summary_table
)

# Save summary table
write.csv(summary_table, outputs$data_summary, row.names = FALSE)
message("Data summary saved to: ", outputs$data_summary)

print(summary_table)

# -----------------------------------------------------------------------------
# Model Fit Statistics
# -----------------------------------------------------------------------------

message("\n--- Model fit statistics ---")

model_fit <- data.frame(
  model = c("Basic", "Fixed Effects", "Mixed Logit"),
  log_likelihood = c(
    model_basic$model$logLik,
    model_fe$model$logLik,
    model_mixed$model$logLik
  ),
  n_parameters = c(
    length(coef(model_basic$model)),
    length(coef(model_fe$model)),
    length(coef(model_mixed$model))
  )
)

model_fit$AIC <- -2 * model_fit$log_likelihood + 2 * model_fit$n_parameters
model_fit$BIC <- -2 * model_fit$log_likelihood + log(nrow(data_clean)) * model_fit$n_parameters

print(model_fit)

# Save model fit table
model_fit_path <- file.path(dirname(outputs$wtp_table), "model_fit.csv")
write.csv(model_fit, model_fit_path, row.names = FALSE)
message("Model fit statistics saved to: ", model_fit_path)