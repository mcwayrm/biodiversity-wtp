#!/usr/bin/env Rscript
# scripts/R/10_estimate_rum_models.R
#############################################
#  - Loads final master dataset with all attributes and costs
#  - Filters to analysis years and cleans problematic trips
#  - Estimates three models: basic, fixed effects, and mixed logit
#  - Saves: outputs$data_clean (cleaned data), outputs$model_basic,
#           outputs$model_fe, outputs$model_mixed
#
#  Required params:
#    - analysis_start_year: First year to include (e.g., 2019)
#    - analysis_end_year: Last year to include (e.g., 2023)
#    - model_vars: Vector of variable names for RUM estimation
#    - fe_vars: Vector of fixed effect variable names (for FE model)
#    - mixed_vars: Vector of variables to treat as random (for mixed logit)
#############################################

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------

message("Loading master dataset...")
cs <- read_parquet(inputs$master_data_with_travel_cost)
setDT(cs)
cs[, observation_date := as.IDate(observation_date)]

message("Initial dataset: ", nrow(cs), " rows, ", cs[, uniqueN(trip_id)], " trips")

# -----------------------------------------------------------------------------
# Filter to Analysis Years
# -----------------------------------------------------------------------------

# Start and End years
start_year <- params$analysis_start_year
end_year <- params$analysis_end_year

message("Filtering to years ", start_year, "-", end_year)
before_rows <- nrow(cs)
before_trips <- cs[, uniqueN(trip_id)]

# Filter
cs <- cs[!is.na(year) & year >= start_year & year <= end_year]

after_rows <- nrow(cs)
after_trips <- cs[, uniqueN(trip_id)]

message(sprintf("Filtered: %d -> %d rows ; %d -> %d trips",
                before_rows, after_rows, before_trips, after_trips))

# -----------------------------------------------------------------------------
# Clean Missing Data and Problematic Trips
# -----------------------------------------------------------------------------

message("\n--- Cleaning problematic trips ---")

# Find trips where chosen alternative is missing expected_richness
missing_choice_trips <- cs[choice == 1 & is.na(expected_richness), unique(trip_id)]
message("Trips with chosen alternative missing expected_richness: ", length(missing_choice_trips))

if (length(missing_choice_trips) > 0) {
  before_rows <- nrow(cs)
  before_trips <- cs[, uniqueN(trip_id)]
  cs <- cs[!(trip_id %in% missing_choice_trips)]
  message(sprintf("Dropped trips with missing chosen richness: %d -> %d rows ; %d -> %d trips",
                  before_rows, nrow(cs), before_trips, cs[, uniqueN(trip_id)]))
}

# Drop counterfactual alternatives with missing expected_richness
before_cc <- nrow(cs)
cs <- cs[!(choice == 0 & is.na(expected_richness))]
message(sprintf("Dropped %d counterfactual rows with missing expected_richness",
                before_cc - nrow(cs)))

# Check trip statistics
trip_stats <- cs[, .(n_alts = .N, n_chosen = sum(as.integer(choice), na.rm = TRUE)),
                by = trip_id]

message("\nTrip statistics:")
print(trip_stats[, .N, by = .(n_chosen, n_alts)][order(n_chosen, n_alts)])

# Identify problematic trips
no_choice_trips <- trip_stats[n_chosen == 0, trip_id]
few_alts_trips <- trip_stats[n_alts < 2, trip_id]

message("Trips with no chosen alternative: ", length(no_choice_trips))
message("Trips with <2 alternatives: ", length(few_alts_trips))

# Drop problematic trips
drop_trips <- unique(c(no_choice_trips, few_alts_trips))
if (length(drop_trips) > 0) {
  before_rows <- nrow(cs)
  before_trips <- cs[, uniqueN(trip_id)]
  cs <- cs[!(trip_id %in% drop_trips)]
  message(sprintf("Dropped problematic trips: %d -> %d rows ; %d -> %d trips",
                  before_rows, nrow(cs), before_trips, cs[, uniqueN(trip_id)]))
}

# Create sequential observation ID
cs[, obs_id_num := .GRP, by = .(trip_id)]

# Order data
setorder(cs, obs_id_num)

message("\nFinal cleaned dataset: ", nrow(cs), " rows, ", cs[, uniqueN(trip_id)], " trips")

# Save cleaned data
write_parquet(cs, outputs$master_data_final)
message("Cleaned data saved")

# -----------------------------------------------------------------------------
# Estimate Basic Model
# -----------------------------------------------------------------------------

message("\n=== ESTIMATING BASIC MODEL ===")

model_vars <- params$model_vars

message("Model variables: ", paste(model_vars, collapse = ", "))

# Basic Logit Model
ebird_basic <- logitr(
  data = as.data.frame(cs),
  outcome = "choice",
  obsID = "obs_id_num",
  pars = model_vars,
  robust = TRUE
)

message("Basic model estimated")
print(summary(ebird_basic))

# Calculate WTP with error handling
wtp_basic <- tryCatch({
  wtp(ebird_basic, scalePar = "travel_cost_combined")
}, error = function(e) {
  message("Warning: WTP calculation failed for basic model: ", e$message)
  message("This often indicates numerical issues. Attempting manual WTP calculation...")
  
  # Manual WTP calculation: -coef / price_coef
  coefs <- coef(ebird_basic)
  price_coef <- coefs["travel_cost_combined"]
  
  if (is.na(price_coef) || abs(price_coef) < 1e-10) {
    message("Price coefficient too small or NA, cannot calculate WTP")
    return(NULL)
  }
  
  wtp_manual <- -coefs / price_coef
  wtp_manual <- wtp_manual[names(wtp_manual) != "travel_cost_combined"]
  
  message("Manual WTP estimates calculated")
  return(list(Estimate = wtp_manual, method = "manual"))
})

if (!is.null(wtp_basic)) {
  message("\nWTP estimates (basic model):")
  print(wtp_basic)
}

# Save model
saveRDS(list(
  model = ebird_basic,
  wtp = wtp_basic,
  summary = summary(ebird_basic)
), outputs$model_basic)

message("Basic model saved")

# -----------------------------------------------------------------------------
# Estimate Fixed Effects Model
# -----------------------------------------------------------------------------

message("\n=== ESTIMATING FIXED EFFECTS MODEL ===")

fe_vars <- params$fe_vars
message("Fixed effects: ", paste(fe_vars, collapse = ", "))

# Demean variables
cs_demeaned <- demean(
  X = cs[, ..model_vars],
  f = cs[, ..fe_vars],
  na.rm = FALSE
)

# Add demeaned variables to dataset
cs[, (paste0(model_vars, "_dm")) := as.data.table(cs_demeaned)]

# Estimate FE model
ebird_fe <- logitr(
  data = as.data.frame(cs),
  outcome = "choice",
  obsID = "obs_id_num",
  pars = paste0(model_vars, "_dm"),
  robust = TRUE
)

message("Fixed effects model estimated")
print(summary(ebird_fe))

# Calculate WTP with error handling
wtp_fe <- tryCatch({
  wtp(ebird_fe, scalePar = "travel_cost_combined_dm")
}, error = function(e) {
  message("Warning: WTP calculation failed for FE model: ", e$message)
  message("Attempting manual WTP calculation...")
  
  coefs <- coef(ebird_fe)
  price_coef <- coefs["travel_cost_combined_dm"]
  
  if (is.na(price_coef) || abs(price_coef) < 1e-10) {
    message("Price coefficient too small or NA, cannot calculate WTP")
    return(NULL)
  }
  
  wtp_manual <- -coefs / price_coef
  wtp_manual <- wtp_manual[names(wtp_manual) != "travel_cost_combined_dm"]
  
  message("Manual WTP estimates calculated")
  return(list(Estimate = wtp_manual, method = "manual"))
})

if (!is.null(wtp_fe)) {
  message("\nWTP estimates (FE model):")
  print(wtp_fe)
}

# Save model
saveRDS(list(
  model = ebird_fe,
  wtp = wtp_fe,
  summary = summary(ebird_fe)
), outputs$model_fe)

message("Fixed effects model saved")

# -----------------------------------------------------------------------------
# Estimate Mixed Logit Model
# -----------------------------------------------------------------------------

message("\n=== ESTIMATING MIXED LOGIT MODEL ===")

mixed_vars <- params$mixed_vars
message("Random parameters: ", paste(mixed_vars, collapse = ", "))

# Create randPars specification
rand_pars <- setNames(rep("n", length(mixed_vars)), mixed_vars)

# Mixed Logit Model
ebird_mixed <- logitr(
  data = as.data.frame(cs),
  outcome = "choice",
  obsID = "obs_id_num",
  pars = model_vars,
  randPars = rand_pars,
  numMultiStarts = 5,
  robust = TRUE
)

message("Mixed logit model estimated")
print(summary(ebird_mixed))

# Calculate WTP with error handling
wtp_mixed <- tryCatch({
  wtp(ebird_mixed, scalePar = "travel_cost_combined")
}, error = function(e) {
  message("Warning: WTP calculation failed for mixed logit: ", e$message)
  message("Attempting manual WTP calculation...")
  
  coefs <- coef(ebird_mixed)
  price_coef <- coefs["travel_cost_combined"]
  
  if (is.na(price_coef) || abs(price_coef) < 1e-10) {
    message("Price coefficient too small or NA, cannot calculate WTP")
    return(NULL)
  }
  
  wtp_manual <- -coefs / price_coef
  wtp_manual <- wtp_manual[!grepl("travel_cost_combined|sd_", names(wtp_manual))]
  
  message("Manual WTP estimates calculated")
  return(list(Estimate = wtp_manual, method = "manual"))
})

if (!is.null(wtp_mixed)) {
  message("\nWTP estimates (mixed logit):")
  print(wtp_mixed)
}

# Save model
saveRDS(list(
  model = ebird_mixed,
  wtp = wtp_mixed,
  summary = summary(ebird_mixed)
), outputs$model_mixed)

message("Mixed logit model saved")

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

message("\n=== MODEL ESTIMATION COMPLETE ===")
message("Basic model log-likelihood: ", round(ebird_basic$logLik, 2))
message("FE model log-likelihood: ", round(ebird_fe$logLik, 2))
message("Mixed logit log-likelihood: ", round(ebird_mixed$logLik, 2))
message("\nAll models saved to outputs directory")