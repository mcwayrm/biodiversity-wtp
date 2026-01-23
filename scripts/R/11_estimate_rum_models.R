#!/usr/bin/env Rscript
# scripts/R/11_estimate_rum_models.R
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

if (file.exists(outputs$master_data_final)) {
  message("Cleaned data already exists at: ", outputs$master_data_final, " -- skipping data cleaning.")
  cs <- read_parquet(outputs$master_data_final)
  setDT(cs)
  cs[, observation_date := as.IDate(observation_date)]
} else {
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

  # -----------------------------------------------------------------------------
  # Create expected_richness interaction variables by month and season
  # -----------------------------------------------------------------------------
  cs[, month := month(observation_date)]
  cs[, season := "Fall"]
  cs[month %in% c(12, 1, 2), season := "Winter"]
  cs[month %in% c(3, 4, 5), season := "Spring"]
  cs[month %in% c(6, 7, 8), season := "Summer"]

  # By month (expected_richness_Jan, ..., expected_richness_Dec)
  month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  for (i in 1:12) {
    varname <- paste0("expected_richness_", month_names[i])
    cs[, (varname) := ifelse(month == i, expected_richness, 0)]
  }

  # By season (expected_richness_Winter, etc)
  season_names <- c("Winter", "Spring", "Summer", "Fall")
  for (s in season_names) {
    varname <- paste0("expected_richness_", s)
    cs[, (varname) := ifelse(season == s, expected_richness, 0)]
  }

  message("\nFinal cleaned dataset: ", nrow(cs), " rows, ", cs[, uniqueN(trip_id)], " trips")

  # Save cleaned data
  write_parquet(cs, outputs$master_data_final)
  message("Cleaned data saved")
}

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
# -----------------------------------------------------------------------------
# Utility: Calculate WTP with error handling
# -----------------------------------------------------------------------------
calc_wtp_with_error_handling <- function(model, scalePar, exclude_pattern = NULL) {
  tryCatch({
    wtp(model, scalePar = scalePar)
  }, error = function(e) {
    message("Warning: WTP calculation failed for model: ", e$message)
    message("This often indicates numerical issues. Attempting manual WTP calculation...")
    coefs <- coef(model)
    price_coef <- coefs[scalePar]
    if (is.na(price_coef) || abs(price_coef) < 1e-10) {
      message("Price coefficient too small or NA, cannot calculate WTP")
      return(NULL)
    }
    wtp_manual <- -coefs / price_coef
    if (!is.null(exclude_pattern)) {
      wtp_manual <- wtp_manual[!grepl(exclude_pattern, names(wtp_manual))]
    } else {
      wtp_manual <- wtp_manual[names(wtp_manual) != scalePar]
    }
    message("Manual WTP estimates calculated")
    return(list(Estimate = wtp_manual, method = "manual"))
  })
}
# Calculate WTP for basic model
wtp_basic <- calc_wtp_with_error_handling(ebird_basic, scalePar = "travel_cost_combined")

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

# Save WTP estimates separately
wtp_basic_path <- sub("model_basic.rds$", "wtp_basic.rds", outputs$model_basic)
saveRDS(wtp_basic, wtp_basic_path)

message("Basic model and WTP saved")

# -----------------------------------------------------------------------------
# Estimate Fixed Effects Model (optional)
# -----------------------------------------------------------------------------

fe_vars <- params$fe_vars
if (!is.null(fe_vars) && !identical(fe_vars, "skip") && length(fe_vars) > 0) {
  message("\n=== ESTIMATING FIXED EFFECTS MODEL ===")
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
  # Calculate WTP for FE model
  wtp_fe <- calc_wtp_with_error_handling(ebird_fe, scalePar = "travel_cost_combined_dm")
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

  # Save WTP estimates separately
  wtp_fe_path <- sub("model_fe.rds$", "wtp_fe.rds", outputs$model_fe)
  saveRDS(wtp_fe, wtp_fe_path)

  message("Fixed effects model and WTP saved")
} else {
  message("\nSkipping fixed effects model estimation (fe_vars not provided or set to 'skip').")
}

# -----------------------------------------------------------------------------
# Estimate Mixed Logit Model (optional)
# -----------------------------------------------------------------------------

mixed_vars <- params$mixed_vars
if (!is.null(mixed_vars) && !identical(mixed_vars, "skip") && length(mixed_vars) > 0) {
  message("\n=== ESTIMATING MIXED LOGIT MODEL ===")
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
  # Calculate WTP for mixed logit model
  wtp_mixed <- calc_wtp_with_error_handling(
    ebird_mixed,
    scalePar = "travel_cost_combined",
    exclude_pattern = "travel_cost_combined|sd_"
  )
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

  # Save WTP estimates separately
  wtp_mixed_path <- sub("model_mixed.rds$", "wtp_mixed.rds", outputs$model_mixed)
  saveRDS(wtp_mixed, wtp_mixed_path)

  message("Mixed logit model and WTP saved")
} else {
  message("\nSkipping mixed logit model estimation (mixed_vars not provided or set to 'skip').")
}

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

message("\n=== MODEL ESTIMATION COMPLETE ===")
message("\nAll models saved to outputs directory")