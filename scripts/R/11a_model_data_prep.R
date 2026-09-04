# scripts/R/11a_model_data_prep.R
# ===========================================================================
# MODEL DATA PREPARATION PIPELINE
# ===========================================================================
# Purpose: Filter, clean, and prepare raw choice set data for xlogit estimation
#
# Expected inputs:
#   inputs$master_data_with_iv - directory of year-partitioned parquet files
#     from 09 (or fallback master_data_with_travel_cost, same shape)
#
# Expected outputs:
#   outputs$model_data - a DIRECTORY of year-partitioned parquet files, with
#     a "_SUCCESS" marker file inside for run_task()'s skip-check -- NOT a
#     single .parquet file anymore. See the memory-architecture note below.
#
# MEMORY ARCHITECTURE (mirrors 09's restructuring, same root cause): this
# script processes ONE YEAR AT A TIME -- loads only that year's rows via
# arrow filter pushdown, applies all cleaning stages, writes immediately,
# discards before moving to the next year. Never holds the full multi-year
# dataset in memory simultaneously. This became necessary once 09 stopped
# dropping any years (2023/2024 forward-filled instead of dropped), since
# that meant this script's year-window filter started letting through
# ~36M rows in one collect() instead of the ~23-24M it used to see.
#
# Trip-level operations (Stages 4/5) are safe to do per-year independently:
# a trip_id is always confined to a single year (one observation_date), so
# there's no cross-year leakage from doing this work per-year rather than
# on the full dataset at once.
#
# obs_id_num is now assigned as (year * 10,000,000 + a per-year sequence
# number) rather than a single globally-sequential integer -- this
# guarantees global uniqueness without needing a separate cross-year
# coordination pass (which would reintroduce the "hold everything at once"
# problem this whole restructuring exists to avoid). 10,000,000 is larger
# than any single year's trip count by a wide margin, so no collision risk.
#
# NOTE: downstream (11b/estimate_rum_xlogit.py), pandas' read_parquet()
# transparently reads a directory of parquet files the same way it reads a
# single file -- no Python-side code change needed, just point
# input_data_path at the directory instead of the old single filename.
# Files/dirs prefixed with "_" (like "_SUCCESS") are ignored by both
# arrow's and pyarrow's directory scanning by standard big-data convention
# -- already confirmed working on the R/arrow side reading 09's output.
#
# NOTE: FE demeaning happens in estimate_rum_xlogit.py, computed directly
# with pandas on this prepped data. No JSON hand-off.

cat(paste0("=", strrep("=", 70), "\n"))
cat("11A: MODEL DATA PREPARATION\n")
cat(paste0("=", strrep("=", 70), "\n"))
cat(sprintf("Scenario: %s\n", scenario_name))

input_data_path <- if (!is.null(inputs$master_data_with_iv)) {
  inputs$master_data_with_iv
} else {
  inputs$master_data_with_travel_cost
}

if (!file.exists(input_data_path) && !dir.exists(input_data_path)) {
  stop(sprintf("Input file not found: %s", input_data_path))
}

cat(sprintf("Loading: %s\n", basename(input_data_path)))

# NOTE: baseline_2015_users_only filtering happens upstream now, in
# 02_filter_users.R, applied against raw trip records before choice-set
# expansion. Every downstream task (including this one) already operates
# on the pre-filtered sample -- nothing else needed here.

# Determine which years to actually process -- only years within the
# analysis window, and only years present in the input data. Cheap: just a
# distinct-years query, not full rows.
available_years <- arrow::open_dataset(input_data_path) %>%
  dplyr::mutate(year_num = as.integer(year)) %>%
  dplyr::distinct(year_num) %>%
  dplyr::collect() %>%
  dplyr::pull(year_num) %>%
  sort()

years_to_process <- available_years[
  available_years >= params$analysis_start_year & available_years <= params$analysis_end_year
]
cat(sprintf("Years to process: %s\n", paste(years_to_process, collapse = ", ")))

output_dir <- outputs$model_data
if (basename(output_dir) == "_SUCCESS") output_dir <- dirname(output_dir)  # tolerate either being passed
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

total_rows_before <- 0L
total_rows_after <- 0L
total_trips <- 0L

for (yr_int in years_to_process) {
  cat(sprintf("\n--- Year %d ---\n", yr_int))

  # Stage 1 (per-year): load only this year's rows via arrow filter pushdown
  cs <- arrow::open_dataset(input_data_path) %>%
    dplyr::mutate(year_num = as.integer(year)) %>%
    dplyr::filter(!is.na(choice), !is.na(trip_id), year_num == yr_int) %>%
    dplyr::collect()
  setDT(cs)
  cs[, year := year_num]
  cs[, year_num := NULL]
  n_before <- nrow(cs)
  total_rows_before <- total_rows_before + n_before
  cat(sprintf("  Loaded %.2fM rows\n", n_before / 1e6))

  if (nrow(cs) == 0) {
    cat("  No rows remain for this year, skipping\n")
    rm(cs); gc()
    next
  }

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

  # Stage 4: Clean richness data (trip-level, safe per-year -- a trip_id
  # never spans multiple years)
  missing_chosen_trips <- unique(cs[choice == 1 & is.na(expected_richness), .(trip_id)])
  if (nrow(missing_chosen_trips) > 0) {
    setkey(missing_chosen_trips, trip_id)
    setkey(cs, trip_id)
    cs <- cs[!missing_chosen_trips]
  }
  cs <- cs[!(choice == 0 & is.na(expected_richness))]

  # Stage 5: Validate choice structure (trip-level, safe per-year)
  trip_stats <- cs[, .(n_chosen = sum(choice), n_alts = .N), by = trip_id]
  bad_trips <- trip_stats[n_chosen != 1 | n_alts < 2, .(trip_id)]
  if (nrow(bad_trips) > 0) {
    setkey(bad_trips, trip_id)
    setkey(cs, trip_id)
    cs <- cs[!bad_trips]
  }
  rm(trip_stats, bad_trips, missing_chosen_trips)

  n_after <- nrow(cs)
  total_rows_after <- total_rows_after + n_after
  cat(sprintf("  After cleaning: %.2fM rows, %dK trips\n", n_after / 1e6, round(uniqueN(cs$trip_id) / 1000)))

  if (nrow(cs) == 0) {
    cat("  No rows remain after cleaning, skipping\n")
    rm(cs); gc()
    next
  }

  # Stage 6: Seasonal richness variants -- all FOUR seasons, so which one
  # is the reference/excluded category is a models.yml choice, not baked
  # in here
  for (richness_var in c("expected_richness", "migrant_richness", "resident_richness")) {
    if (richness_var %in% colnames(cs)) {
      for (season_name in c("Winter", "Spring", "Summer", "Fall")) {
        new_var <- sprintf("%s_%s", richness_var, season_name)
        cs[, (new_var) := fifelse(season == season_name, .SD[[1]], 0), .SDcols = richness_var]
      }
      gc()
    }
  }

  # Stage 7: Assign obs_id_num -- year-prefixed for global uniqueness
  # without needing a cross-year synchronization pass. IMPORTANT: no L
  # suffix on 10000000 -- R's plain integer type is 32-bit (caps ~2.1
  # billion), and yr_int * 10000000 alone already exceeds that for every
  # year in this panel, silently producing NA via integer overflow if done
  # as integer arithmetic. Dropping the L forces double/numeric arithmetic,
  # which exactly represents integers up to 2^53 -- far more than enough
  # for these magnitudes (~20 billion at most).
  trip_lookup <- unique(cs[, .(trip_id)])
  trip_lookup[, obs_id_num := yr_int * 10000000 + (.I - 1L)]
  setkey(trip_lookup, trip_id)
  setkey(cs, trip_id)
  cs <- trip_lookup[cs]
  setcolorder(cs, c("obs_id_num", setdiff(names(cs), "obs_id_num")))
  rm(trip_lookup)

  total_trips <- total_trips + uniqueN(cs$trip_id)

  write_parquet(cs, file.path(output_dir, sprintf("year=%d.parquet", yr_int)))
  cat(sprintf("  Wrote year=%d.parquet\n", yr_int))

  rm(cs)
  gc()
}

cat(sprintf("\nTotal: %.1fM rows before cleaning -> %.1fM rows after, %dK trips across %d year(s)\n",
            total_rows_before / 1e6, total_rows_after / 1e6, round(total_trips / 1000), length(years_to_process)))

file.create(file.path(output_dir, "_SUCCESS"))
cat(sprintf("\nSaved: %s\n", output_dir))
cat(paste0("=", strrep("=", 70), "\n"))