# scripts/R/11a_model_data_prep.R
# ===========================================================================
# MODEL DATA PREPARATION PIPELINE
# ===========================================================================
# Purpose: Filter, clean, and prepare raw choice set data for xlogit estimation
#
# Expected inputs:
#   inputs$master_data_with_iv - IV-enriched choice set parquet
#   inputs$master_data_with_travel_cost - Fallback raw choice set parquet
#
# Expected outputs:
#   outputs$model_data - Filtered/prepped parquet
#
# NOTE: FE demeaning (previously computed here and exported as global-means
# JSON for the Python subprocess to read) has moved to estimate_rum_xlogit.py,
# which now runs in-process via reticulate and computes group means directly
# with pandas on this same prepped file. No JSON hand-off needed anymore.
#
# NOTE: cat()/message() calls after each stage are deliberate checkpoints --
# this script has hit multiple memory crashes (OS-level "killed", not R's own
# catchable errors) with no output between Stage 1 and Stage 5's completion,
# making it hard to tell which stage actually died. Keep these even if they
# feel verbose; they're cheap and the alternative is debugging blind.

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

# Read lazily via arrow's Dataset API and push the choice/trip_id/year
# filters down to Arrow BEFORE materializing into R memory. Only the
# post-filter rows ever get pulled into a data.table, instead of loading
# the full file and filtering afterward -- meaningfully reduces peak
# memory on this step.
cs <- arrow::open_dataset(input_data_path) %>%
  dplyr::mutate(year_num = as.numeric(year)) %>%
  dplyr::filter(
    !is.na(choice), !is.na(trip_id), !is.na(year_num),
    year_num >= params$analysis_start_year,
    year_num <= params$analysis_end_year
  ) %>%
  dplyr::collect()
setDT(cs)
cs[, year := year_num]
cs[, year_num := NULL]
cat(sprintf("  %.1fM rows after filtering\n", nrow(cs) / 1e6))
gc()

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
cat("  Stage 2 done: temporal variables\n")
gc()

# Stage 3: Log travel cost
if ("travel_cost_combined" %in% colnames(cs)) {
  cs[, log_travel_cost := log(pmax(travel_cost_combined, 0.01))]
}
cat("  Stage 3 done: log travel cost\n")
gc()

# Stage 4: Clean richness data
missing_chosen_trips <- unique(cs[choice == 1 & is.na(expected_richness), .(trip_id)])
if (nrow(missing_chosen_trips) > 0) {
  setkey(missing_chosen_trips, trip_id)
  setkey(cs, trip_id)
  cs <- cs[!missing_chosen_trips]
}
cs <- cs[!(choice == 0 & is.na(expected_richness))]
cat(sprintf("  Stage 4 done: richness cleaning (%.1fM rows remain)\n", nrow(cs) / 1e6))
gc()

# Stage 5: Validate choice structure
trip_stats <- cs[, .(n_chosen = sum(choice), n_alts = .N), by = trip_id]
bad_trips <- trip_stats[n_chosen != 1 | n_alts < 2, .(trip_id)]
if (nrow(bad_trips) > 0) {
  setkey(bad_trips, trip_id)
  setkey(cs, trip_id)
  cs <- cs[!bad_trips]
}
rm(trip_stats, bad_trips)
gc()

cat(sprintf("After cleaning: %.1fM rows, %.0fK trips\n", nrow(cs) / 1e6, uniqueN(cs$trip_id) / 1000))

# Stage 6: Create seasonal richness variants
# All FOUR seasons get their own interaction column (not just three) -- which
# season serves as the reference/excluded category is now purely a modeling
# choice made in models.yml (via which three of the four you list as
# model_vars), not something baked into this script. Previously hardcoded to
# Winter/Spring/Summer only, which silently forced Fall as the only possible
# reference category.
#
# fifelse() allocates a full temporary vector on every call before it gets
# assigned via := -- with 4 seasons x 3 richness vars (12 calls total) that's
# more transient memory pressure than the previous 9-call version. gc()
# between richness variables reclaims those temporaries promptly rather than
# letting them pile up unreclaimed.
for (richness_var in c("expected_richness", "migrant_richness", "resident_richness")) {
  if (richness_var %in% colnames(cs)) {
    for (season_name in c("Winter", "Spring", "Summer", "Fall")) {
      new_var <- sprintf("%s_%s", richness_var, season_name)
      cs[, (new_var) := fifelse(season == season_name, .SD[[1]], 0), .SDcols = richness_var]
    }
    gc()
  }
}
cat("  Stage 6 done: seasonal richness variants\n")

# Stage 7: Save prepared data
trip_lookup <- unique(cs[, .(trip_id)])
trip_lookup[, obs_id_num := .I - 1L]
setkey(trip_lookup, trip_id)
setkey(cs, trip_id)
cs <- trip_lookup[cs]
setcolorder(cs, c("obs_id_num", setdiff(names(cs), "obs_id_num")))
cat("  Stage 7: writing output...\n")
write_parquet(cs, outputs$model_data)

cat(sprintf("\nSaved: %s\n", basename(outputs$model_data)))
cat(sprintf("  %.1fM rows, %.0fK trips\n", nrow(cs) / 1e6, uniqueN(cs$trip_id) / 1000))
cat(paste0("=", strrep("=", 70), "\n"))