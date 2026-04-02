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

# -----------------------------------------------------------------------------
# Model selection logic: run only the model type(s) implied by config
# -----------------------------------------------------------------------------
model_vars <- params$model_vars
fe_vars <- params$fe_vars
mixed_vars <- params$mixed_vars


# Helper to check if a variable is set (not skip/empty/NULL)
is_set <- function(x) {
  !is.null(x) && !identical(x, "skip") && length(x) > 0 && !(length(x) == 1 && x == "skip")
}

# Subsample data by trip_id if sample param is present and <1
if (!is.null(params$sample) && is.numeric(params$sample) && params$sample < 1 && params$sample > 0) {
  set.seed(42) # for reproducibility
  trip_ids <- unique(cs$trip_id)
  n_trips <- ceiling(length(trip_ids) * params$sample)
  sampled_trips <- sample(trip_ids, n_trips)
  cs <- cs[trip_id %in% sampled_trips]
  message(sprintf("Subsampled data to %.2f%% of trips: %d trips, %d rows", params$sample * 100, length(sampled_trips), nrow(cs)))
}

model_type <- "basic"
if (is_set(mixed_vars)) {
  model_type <- "mixed"
} else if (is_set(fe_vars)) {
  model_type <- "fe"
}

model_name <- if (!is.null(params$model_name)) params$model_name else "model"
out_prefix <- paste0(model_name, "_", model_type)

if (model_type == "basic") {
  message("\n=== ESTIMATING BASIC MODEL ===")
  message("Model variables: ", paste(model_vars, collapse = ", "))
  ebird_basic <- logitr(
    data = as.data.frame(cs),
    outcome = "choice",
    obsID = "obs_id_num",
    pars = model_vars,
    robust = TRUE
  )
  message("Basic model estimated")
  print(summary(ebird_basic))
  wtp_basic <- calc_wtp_with_error_handling(ebird_basic, scalePar = "travel_cost_combined")
  if (!is.null(wtp_basic)) {
    message("\nWTP estimates (basic model):")
    print(wtp_basic)
  }
  saveRDS(list(
    model = ebird_basic,
    wtp = wtp_basic,
    summary = summary(ebird_basic)
  ), file = outputs$model_rds)
  saveRDS(wtp_basic, file = outputs$wtp_rds)
  message("Basic model and WTP saved with model name in filename")
}

# -----------------------------------------------------------------------------
# Estimate Fixed Effects Model
# -----------------------------------------------------------------------------

if (model_type == "fe") {
  # Print info about observation_date for debugging
  message("\n--- observation_date variable info ---")
  message("Class: ", paste(class(cs$date), collapse=", "))
  message("First 5 values:")
  print(head(cs$date, 5))
  message("Summary:")
  print(summary(cs$date))
  message("\n=== ESTIMATING FIXED EFFECTS MODEL ===")
  message("Fixed effects (raw): ", paste(capture.output(str(fe_vars)), collapse = " "))

  # Helper to create interaction variables
  create_interaction <- function(dt, vars) {
    vname <- paste(vars, collapse = "_")
    dt[, (vname) := do.call(paste, c(.SD, sep = "_")), .SDcols = vars]
    return(vname)
  }

  # Parse and create all FE variables (single, interaction, hour_of_day)
  fe_names <- c()
  for (fe in fe_vars) {
    if (is.list(fe) || (is.atomic(fe) && length(fe) > 1)) {
      # Interaction FE (e.g., [user_id, year])
      vname <- create_interaction(cs, unlist(fe))
      fe_names <- c(fe_names, vname)
    } else if (fe == "hour_of_day") {
      # Extract hour from observation_date (assume POSIXct or character with time)
      if (!"hour_of_day" %in% names(cs)) {
        if (inherits(cs$observation_date, "POSIXct") || inherits(cs$observation_date, "POSIXt")) {
          cs[, hour_of_day := as.integer(format(observation_date, "%H"))]
        } else if (inherits(cs$observation_date, "IDate")) {
          stop("observation_date does not contain time information for hour_of_day FE.")
        } else {
          # Try to parse as character
          cs[, hour_of_day := as.integer(substr(format(observation_date), 12, 13))]
        }
      }
      fe_names <- c(fe_names, "hour_of_day")
    } else {
      # Single FE
      fe_names <- c(fe_names, as.character(fe))
    }
  }

  message("Fixed effects (parsed): ", paste(fe_names, collapse = ", "))

  # Check all FE columns exist
  missing_fes <- setdiff(fe_names, names(cs))
  if (length(missing_fes) > 0) {
    stop(paste("ERROR: The following fixed effect variables are missing in the data:", paste(missing_fes, collapse = ", ")))
  }

  # Remove rows with NA in model_vars or fe_names
  all_vars <- unique(c(model_vars, fe_names))
  n_before <- nrow(cs)
  cs <- cs[complete.cases(cs[, ..all_vars])]
  n_after <- nrow(cs)
  if (n_after < n_before) {
    message(sprintf("Removed %d rows with NA in model_vars or FE vars before demean.", n_before - n_after))
  }

  # Remove trips with no or multiple chosen alternatives (must have exactly one choice==1 per obs_id_num)
  trip_choice_counts <- cs[, .(n_chosen = sum(choice == 1, na.rm = TRUE)), by = obs_id_num]
  bad_trips <- trip_choice_counts[n_chosen != 1, obs_id_num]
  n_bad <- length(bad_trips)
  if (n_bad > 0) {
    cs <- cs[!obs_id_num %in% bad_trips]
    message(sprintf("Removed %d trips with no or multiple chosen alternatives after filtering.", n_bad))
  }

  # Demean variables, stop on error
  tryCatch({
    cs_demeaned <- demean(
      X = cs[, ..model_vars],
      f = cs[, ..fe_names],
      na.rm = FALSE
    )
    cs[, (paste0(model_vars, "_dm")) := as.data.table(cs_demeaned)]
  }, error = function(e) {
    stop(paste("ERROR during demean step:", e$message))
  })

  # Build FE spec string for filenames
  fe_spec_str <- paste(fe_names, collapse = "-")
  model_name <- if (!is.null(params$model_name)) params$model_name else "fe_model"
  out_prefix <- paste0(model_name, "_fe_", fe_spec_str)

  # Estimate FE model and save with FE spec in filename
  ebird_fe <- logitr(
    data = as.data.frame(cs),
    outcome = "choice",
    obsID = "obs_id_num",
    pars = paste0(model_vars, "_dm"),
    robust = TRUE
  )
  message("Fixed effects model estimated")
  print(summary(ebird_fe))
  wtp_fe <- calc_wtp_with_error_handling(ebird_fe, scalePar = "travel_cost_combined_dm")
  if (!is.null(wtp_fe)) {
    message("\nWTP estimates (FE model):")
    print(wtp_fe)
  }
  saveRDS(list(
    model = ebird_fe,
    wtp = wtp_fe,
    summary = summary(ebird_fe)
  ), file = outputs$model_rds)
  saveRDS(wtp_fe, file = outputs$wtp_rds)
  message("Fixed effects model and WTP saved with model name in filename")
  
} else {
  message("\nSkipping fixed effects model estimation (fe_vars not provided or set to 'skip').")
}

# -----------------------------------------------------------------------------
# Estimate Mixed Logit Model (optional)
# -----------------------------------------------------------------------------

if (model_type == "mixed") {
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
  ), file = outputs$model_rds)
  saveRDS(wtp_mixed, file = outputs$wtp_rds)
  message("Mixed logit model and WTP saved with model name in filename")
} else {
  message("\nSkipping mixed logit model estimation (mixed_vars not provided or set to 'skip').")
}

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

message("\n=== MODEL ESTIMATION COMPLETE ===")
message("\nAll models saved to outputs directory")