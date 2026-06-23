# scripts/R/11a_model_data_prep.R
# ===========================================================================
# MODEL DATA PREPARATION PIPELINE
# ===========================================================================
# Purpose: Filter, clean, and prepare raw choice set data for xlogit estimation
# Computes global means by FE structure for later demeaning
#
# Expected inputs:
#   inputs$master_data_with_iv - IV-enriched choice set parquet
#   inputs$master_data_with_travel_cost - Fallback raw choice set parquet
#
# Expected outputs:
#   outputs$model_data - Filtered/prepped parquet
#   outputs$global_means_dir - Directory with JSON files by FE structure

cat(paste0("=", strrep("=", 70), "\n"))
cat("11A: MODEL DATA PREPARATION\n")
cat(paste0("=", strrep("=", 70), "\n"))
cat(sprintf("Scenario: %s\n", scenario_name))

# Load raw data
input_data_path <- if (!is.null(inputs$master_data_with_iv)) {
  inputs$master_data_with_iv
} else {
  inputs$master_data_with_travel_cost
}

if (!file.exists(input_data_path)) {
  stop(sprintf("Input file not found: %s", input_data_path))
}

cat(sprintf("Loading: %s\n", basename(input_data_path)))
cs <- read_parquet(input_data_path)
setDT(cs)
cat(sprintf("  %.0fM rows\n", nrow(cs) / 1e6))

# Stage 1: Filter by year and basic criteria
cs <- cs[!is.na(choice) & !is.na(trip_id)]
cs[, year := suppressWarnings(as.numeric(year))]
cs <- cs[
  !is.na(year) &
    year >= params$analysis_start_year &
    year <= params$analysis_end_year
]

# Stage 2: Temporal variables
cs[, observation_date := as.Date(observation_date)]
cs[, month := lubridate::month(observation_date)]
cs[, season := fifelse(
  month %in% c(12, 1, 2), "Winter",
  fifelse(
    month %in% c(3, 4, 5), "Spring",
    fifelse(month %in% c(6, 7, 8), "Summer", "Fall")
  )
)]
cs[, hour_of_day := suppressWarnings(as.numeric(substr(as.character(time_observations_started), 1, 2)))]
cs[is.na(hour_of_day), hour_of_day := 12]

# Stage 3: Log travel cost
if ("travel_cost_combined" %in% colnames(cs)) {
  cs[, log_travel_cost := log(pmax(travel_cost_combined, 0.01))]
}

# Stage 4: Clean richness data
missing_chosen_trips <- unique(cs[choice == 1 & is.na(expected_richness), .(trip_id)])
if (nrow(missing_chosen_trips) > 0) {
  setkey(missing_chosen_trips, trip_id)
  setkey(cs, trip_id)
  cs <- cs[!missing_chosen_trips]
}
cs <- cs[!(choice == 0 & is.na(expected_richness))]

# Stage 5: Validate choice structure
trip_stats <- cs[, .(n_chosen = sum(choice), n_alts = .N), by = trip_id]
bad_trips <- trip_stats[n_chosen != 1 | n_alts < 2, .(trip_id)]
if (nrow(bad_trips) > 0) {
  setkey(bad_trips, trip_id)
  setkey(cs, trip_id)
  cs <- cs[!bad_trips]
}

cat(sprintf("After cleaning: %.1fM rows, %.0fK trips\n", nrow(cs) / 1e6, uniqueN(cs$trip_id) / 1000))

# Stage 6: Create seasonal richness variants
for (richness_var in c("expected_richness", "migrant_richness", "resident_richness")) {
  if (richness_var %in% colnames(cs)) {
    for (season_name in c("Winter", "Spring", "Summer")) {
      new_var <- sprintf("%s_%s", richness_var, season_name)
      cs[, (new_var) := fifelse(season == season_name, .SD[[1]], 0), .SDcols = richness_var]
    }
  }
}

# Stage 7: Compute global means by FE structure
cat("\nComputing global means...\n")

all_vars <- c(
  "expected_richness", "migrant_richness", "resident_richness",
  "expected_congestion", "precip", "temp", "trees", "log_travel_cost",
  "expected_richness_Winter", "expected_richness_Spring", "expected_richness_Summer",
  "migrant_richness_Winter", "migrant_richness_Spring", "migrant_richness_Summer",
  "resident_richness_Winter", "resident_richness_Spring", "resident_richness_Summer"
)

models_config <- read_yaml("models.yml")
fe_structures <- models_config %>%
  map(~unique(unlist(.x$fe_vars))) %>%
  unique()

dir.create(dirname(outputs$model_data), recursive = TRUE, showWarnings = FALSE)

for (fe_spec in fe_structures) {
  if (!all(fe_spec %in% colnames(cs))) next
  
  fe_key <- paste(sort(fe_spec), collapse = "-")
  cat(sprintf("  FE: %s\n", paste(fe_spec, collapse = ", ")))
  mean_vars <- intersect(all_vars, colnames(cs))
  
  global_means <- cs[
    ,
    lapply(.SD, function(x) mean(x, na.rm = TRUE)),
    by = fe_spec,
    .SDcols = mean_vars
  ]
  setnames(global_means, mean_vars, paste0(mean_vars, "_mean"))
  
  means_file <- file.path(dirname(outputs$model_data), 
                          sprintf("global_means_%s_%s.json", fe_key, scenario_name))
  write_json(as.list(global_means), means_file, pretty = TRUE)
}

# Stage 8: Save prepared data
trip_lookup <- unique(cs[, .(trip_id)])
trip_lookup[, obs_id_num := .I - 1L]
setkey(trip_lookup, trip_id)
setkey(cs, trip_id)
cs <- trip_lookup[cs]
setcolorder(cs, c("obs_id_num", setdiff(names(cs), "obs_id_num")))
write_parquet(cs, outputs$model_data)

cat(sprintf("\nSaved: %s\n", basename(outputs$model_data)))
cat(sprintf("  %.1fM rows, %.0fK trips\n", nrow(cs) / 1e6, uniqueN(cs$trip_id) / 1000))
cat(paste0("=", strrep("=", 70), "\n"))