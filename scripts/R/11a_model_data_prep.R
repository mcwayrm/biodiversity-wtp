# scripts/R/11a_model_data_prep.R
# ===========================================================================
# MODEL DATA PREPARATION PIPELINE
# ===========================================================================
# Purpose: Filter, clean, and prepare raw choice set data for xlogit estimation
# Computes global means by FE structure for later demeaning
#
# Expected inputs:
#   inputs$master_data_with_travel_cost - Raw choice set parquet
#
# Expected outputs:
#   outputs$model_data - Filtered/prepped parquet
#   outputs$global_means_dir - Directory with JSON files by FE structure

cat(paste0("=", strrep("=", 70), "\n"))
cat("11A: MODEL DATA PREPARATION\n")
cat(paste0("=", strrep("=", 70), "\n"))
cat(sprintf("Scenario: %s\n", scenario_name))

# Load raw data
if (!file.exists(inputs$master_data_with_travel_cost)) {
  stop(sprintf("Input file not found: %s", inputs$master_data_with_travel_cost))
}

cat(sprintf("Loading: %s\n", basename(inputs$master_data_with_travel_cost)))
cs <- read_parquet(inputs$master_data_with_travel_cost)
cat(sprintf("  %.0fM rows\n", nrow(cs) / 1e6))

# Stage 1: Filter by year and basic criteria
cs <- cs %>%
  filter(!is.na(choice), !is.na(trip_id)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(year), year >= params$analysis_start_year, year <= params$analysis_end_year)

# Stage 2: Temporal variables
cs <- cs %>%
  mutate(
    observation_date = as.Date(observation_date),
    month = month(observation_date),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Fall"
    ),
    hour_of_day = as.numeric(str_sub(as.character(time_observations_started), 1, 2)),
    hour_of_day = replace_na(hour_of_day, 12)
  )

# Stage 3: Log travel cost
if ("travel_cost_combined" %in% colnames(cs)) {
  cs <- cs %>% mutate(log_travel_cost = log(pmax(travel_cost_combined, 0.01)))
}

# Stage 4: Clean richness data
chosen_missing <- cs %>%
  filter(choice == 1) %>%
  group_by(trip_id) %>%
  filter(any(is.na(expected_richness))) %>%
  distinct(trip_id) %>% pull(trip_id)

cs <- cs %>%
  filter(!(trip_id %in% chosen_missing)) %>%
  filter(!(choice == 0 & is.na(expected_richness)))

# Stage 5: Validate choice structure
trip_stats <- cs %>%
  group_by(trip_id) %>%
  summarise(n_chosen = sum(choice), n_alts = n(), .groups = "drop")

bad_trips <- trip_stats %>% filter(n_chosen != 1 | n_alts < 2) %>% pull(trip_id)
cs <- cs %>% filter(!(trip_id %in% bad_trips))

cat(sprintf("After cleaning: %.1fM rows, %.0fK trips\n", nrow(cs) / 1e6, n_distinct(cs$trip_id) / 1000))

# Stage 6: Create seasonal richness variants
for (richness_var in c("expected_richness", "migrant_richness", "resident_richness")) {
  if (richness_var %in% colnames(cs)) {
    for (season in c("Winter", "Spring", "Summer")) {
      new_var <- sprintf("%s_%s", richness_var, season)
      cs <- cs %>%
        mutate(!!sym(new_var) := ifelse(season == !!season, !!sym(richness_var), 0))
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
  
  global_means <- cs %>%
    group_by(across(all_of(fe_spec))) %>%
    summarise(across(all_of(intersect(all_vars, colnames(cs))),
                     list(mean = ~mean(., na.rm = TRUE))),
              .groups = "drop")
  
  means_file <- file.path(dirname(outputs$model_data), 
                          sprintf("global_means_%s_%s.json", fe_key, scenario_name))
  write_json(as.list(global_means), means_file, pretty = TRUE)
}

# Stage 8: Save prepared data
cs <- cs %>% mutate(obs_id_num = as.integer(factor(trip_id)) - 1, .before = 1)
write_parquet(cs, outputs$model_data)

cat(sprintf("\nSaved: %s\n", basename(outputs$model_data)))
cat(sprintf("  %.1fM rows, %.0fK trips\n", nrow(cs) / 1e6, n_distinct(cs$trip_id) / 1000))
cat(paste0("=", strrep("=", 70), "\n"))