#!/usr/bin/env Rscript
# scripts/R/9_compute_travel_cost.R
#############################################
#  - Loads master dataset with attributes
#  - Computes travel cost based on GDP-derived wages and travel parameters
#  - Saves: outputs$master_data_with_travel_cost (a DIRECTORY of year-
#           partitioned parquet files, not a single file -- see note below)
#
#  Required params:
#    - projection_crs: CRS for spatial operations
#    - time_value_fraction: Value of time as fraction of wage (default: 1/3)
#    - work_hours_per_year: Annual work hours (default: 2000)
#    - travel_speed_kmph: Average travel speed in km/h (default: 30)
#  Required inputs:
#    - master_data_with_attributes: Output from stage 8
#    - district_shp: District boundaries for home location centroids
#    - gdp_csv: GDP data file (panel 2015-2024; predicted_GCP_current_USD in billions USD)
#    - cpi_xlsx: FRED INDCPIALLAINMEI CPI index, "Annual" sheet (base 2015=100),
#                for deflating to real 2021 INR
#    - driving_cost_rds: Year-specific driving cost in real 2021 INR/km
#
#  MEMORY ARCHITECTURE (changed after two real OOM crashes at ~37.6M rows):
#  This script processes ONE YEAR AT A TIME -- computes travel cost for that
#  year's slice, writes it to disk immediately, discards it, moves to the
#  next year. It never holds the full multi-year dataset in memory
#  simultaneously. Peak memory now scales with the largest single year
#  (~4-7M rows) rather than all years combined (~37.6M rows).
#
#  This means outputs$master_data_with_travel_cost must be a DIRECTORY path
#  now, not a single .parquet file path -- e.g.
#    file.path(scenario_dir, "master_data_with_travel_cost")
#  not
#    file.path(scenario_dir, "master_data_with_travel_cost.parquet")
#  A "_SUCCESS" marker file is written inside that directory on completion,
#  and run_all.R's declared output for this task should point at that
#  marker file (file.path(scenario_dir, "master_data_with_travel_cost",
#  "_SUCCESS")) so the existing file.exists()-based skip-check keeps
#  working unchanged. Downstream (11a), arrow::open_dataset() reads a
#  directory of parquet files exactly the same way it reads a single file --
#  no change needed on the reading side beyond pointing at the directory
#  instead of the old single filename.
#
#  GDP coverage: forward-filled per grid cell from the most recent available
#  year where an analysis year has no direct GDP data -- no rows are dropped
#  for this reason. See gdp_year_used column in the output.
#############################################

# -----------------------------------------------------------------------------
# Load Parameters (with defaults)
# -----------------------------------------------------------------------------

time_value_fraction <- if (!is.null(params$time_value_fraction)) params$time_value_fraction else 1/3
work_hours <- if (!is.null(params$work_hours_per_year)) params$work_hours_per_year else 2000
travel_speed_kmph <- if (!is.null(params$travel_speed_kmph)) params$travel_speed_kmph else 30

exchange_df <- tibble(
  year = 2015:2024,
  usd_to_inr = c(64.15, 67.19, 65.12, 68.43, 70.41, 74.10, 73.93, 77.44, 82.57, 83.50)
)

message("Travel cost parameters:")
message("  Time value fraction: ", time_value_fraction)
message("  Work hours/year: ", work_hours)
message("  Travel speed: ", travel_speed_kmph, " km/h")

# -----------------------------------------------------------------------------
# Load District Centroids (for home locations without coordinates)
# -----------------------------------------------------------------------------

dist <- st_read(inputs$district_shp, quiet = TRUE) %>%
  select(c_code_11) %>%
  rename(c_code_2011 = c_code_11) %>%
  st_transform(crs = params$projection_crs)

dist_centroids <- dist %>%
  st_centroid() %>%
  mutate(lon_dist = st_coordinates(geometry)[, 1], lat_dist = st_coordinates(geometry)[, 2]) %>%
  st_drop_geometry()
rm(dist)

# -----------------------------------------------------------------------------
# Load CPI Data (small -- fine to keep in memory whole)
# -----------------------------------------------------------------------------

message("Loading CPI data...")
cpi_raw <- readxl::read_excel(inputs$cpi_xlsx, sheet = "Annual")
cpi_df <- cpi_raw %>%
  mutate(year = lubridate::year(as.Date(observation_date))) %>%
  rename(cpi = INDCPIALLAINMEI) %>%
  select(year, cpi) %>%
  group_by(year) %>%
  summarise(cpi = mean(cpi, na.rm = TRUE), .groups = "drop")
rm(cpi_raw)

cpi_2021 <- cpi_df$cpi[cpi_df$year == 2021]
if (length(cpi_2021) == 0) stop("ERROR: CPI value for 2021 not found — cannot deflate to real 2021 INR!")
if (length(cpi_2021) > 1) stop("ERROR: Multiple CPI values found for 2021 after aggregation — check the Annual sheet!")
message("CPI base value (2021): ", cpi_2021)

# -----------------------------------------------------------------------------
# Load Year-Specific Driving Cost (small)
# -----------------------------------------------------------------------------

message("Loading driving cost data...")
driving_cost_df <- readRDS(inputs$driving_cost_rds) %>%
  mutate(year = as.integer(year), driving_cost = as.numeric(driving_cost)) %>%
  filter(year >= 2015 & year <= 2024)
message("Driving cost years available: ", paste(sort(driving_cost_df$year), collapse = ", "))

# -----------------------------------------------------------------------------
# Load and Process GDP Data (small -- one row per grid cell per year, not
# per observation, so this stays in memory whole)
# -----------------------------------------------------------------------------

message("Loading GDP data...")
gdp_all <- fread(inputs$gdp)
if (!("predicted_GCP_current_USD" %in% names(gdp_all))) {
  stop("Cannot proceed: 'predicted_GCP_current_USD' column not found. ",
       "Available columns: ", paste(names(gdp_all), collapse = ", "))
}

gdp_india <- gdp_all[iso == "IND" & year >= 2015 & year <= 2024]
rm(gdp_all)
years_in_india <- sort(unique(gdp_india$year))
message("Available years for India (2015-2024): ", paste(years_in_india, collapse = ", "))
if (length(years_in_india) == 0) stop("ERROR: No India GDP data found for 2015-2024!")
missing_gdp_years <- setdiff(2015:2024, years_in_india)
if (length(missing_gdp_years) > 0) {
  message("NOTE: GDP data missing for year(s): ", paste(missing_gdp_years, collapse = ", "),
          " -- forward-filled per grid cell from the most recent available prior year")
}

gdp_india <- gdp_india[pop_cell > 0]
gdp_india <- as.data.frame(gdp_india) %>%
  left_join(exchange_df, by = "year") %>%
  mutate(
    gdppc_usd = (predicted_GCP_current_USD * 1e9) / pop_cell,
    gdppc_nominal_INR = gdppc_usd * usd_to_inr
  ) %>%
  left_join(cpi_df, by = "year") %>%
  mutate(gdppc_real_2021_INR = round(gdppc_nominal_INR * (cpi_2021 / cpi), 2)) %>%
  select(year, longitude, latitude, gdppc_real_2021_INR)

message("\nGDP per capita (real 2021 INR) across panel years:")
gdp_india %>%
  group_by(year) %>%
  summarise(mean_gdppc = round(mean(gdppc_real_2021_INR, na.rm = TRUE), 0), .groups = "drop") %>%
  { message(paste(sprintf("  %d: ₹%s", .$year, format(.$mean_gdppc, big.mark = ",")), collapse = "\n")) }

gdp_india_sf <- st_as_sf(gdp_india, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = params$projection_crs)
rm(gdp_india)
available_gdp_years <- sort(unique(gdp_india_sf$year))
gc()

# -----------------------------------------------------------------------------
# Determine years present in the input data, WITHOUT loading full rows yet
# -----------------------------------------------------------------------------

analysis_years <- arrow::open_dataset(inputs$master_data_with_attributes) %>%
  dplyr::distinct(year) %>%
  dplyr::collect() %>%
  dplyr::pull(year) %>%
  as.integer() %>%
  sort()
message("\nYears present in input data: ", paste(analysis_years, collapse = ", "))

# -----------------------------------------------------------------------------
# Set up output directory + per-year loop
# -----------------------------------------------------------------------------

output_dir <- outputs$master_data_with_travel_cost
if (basename(output_dir) == "_SUCCESS") output_dir <- dirname(output_dir)  # tolerate either being passed
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

coverage_rows <- list()
n_forward_filled_total <- 0L
n_rows_total <- 0L

for (yr_int in analysis_years) {
  message("\n--- Year ", yr_int, " ---")

  # Load ONLY this year's rows via arrow filter pushdown -- never loads
  # other years' rows into memory
  master_year <- arrow::open_dataset(inputs$master_data_with_attributes) %>%
    dplyr::mutate(year_num = as.integer(year)) %>%
    dplyr::filter(year_num == yr_int) %>%
    dplyr::collect()
  setDT(master_year)
  master_year[, year := year_num]
  master_year[, year_num := NULL]
  message("  Loaded ", nrow(master_year), " rows")

  if (!all(c("lon_home", "lat_home") %in% names(master_year))) {
    master_year <- master_year %>% left_join(dist_centroids, by = "c_code_2011")
    setDT(master_year)
  }

  # Forward-fill: most recent available GDP year at or before this year
  candidate_years <- available_gdp_years[available_gdp_years <= yr_int]
  match_year <- if (length(candidate_years) > 0) max(candidate_years) else min(available_gdp_years)
  if (match_year != yr_int) message("  No direct GDP data, forward-filling from ", match_year)
  gdp_year <- gdp_india_sf %>% filter(year == match_year)

  homes_sf <- st_as_sf(as.data.frame(master_year), coords = c("lon_home", "lat_home"), crs = 4326) %>%
    st_transform(crs = params$projection_crs)
  nearest_indices <- st_nearest_feature(homes_sf, gdp_year)
  master_year[, gdppc_real_2021_INR := gdp_year$gdppc_real_2021_INR[nearest_indices]]
  master_year[, gdp_year_used := match_year]
  master_year[, hourly_wage := gdppc_real_2021_INR / work_hours]
  rm(homes_sf, nearest_indices, gdp_year)

  # Driving cost for this year
  dc_row <- driving_cost_df[driving_cost_df$year == yr_int, ]
  master_year[, driving_cost := if (nrow(dc_row) > 0) dc_row$driving_cost[1] else NA_real_]
  if (nrow(dc_row) == 0) message("  WARNING: no driving_cost for year ", yr_int)

  master_year[, geo_dist := as.numeric(geo_dist)]
  master_year[, `:=`(
    travel_time_hours = geo_dist / travel_speed_kmph,
    time_cost = 2 * time_value_fraction * hourly_wage * (geo_dist / travel_speed_kmph),
    fuel_cost = geo_dist * driving_cost
  )]
  master_year[, travel_cost_combined := time_cost + fuel_cost]
  master_year[, log_travel_cost := log1p(travel_cost_combined)]

  coverage_rows[[as.character(yr_int)]] <- data.table(
    year = yr_int,
    n_rows = nrow(master_year),
    n_missing = sum(is.na(master_year$travel_cost_combined)),
    mean_cost = mean(master_year$travel_cost_combined, na.rm = TRUE),
    n_forward_filled = sum(master_year$gdp_year_used != master_year$year)
  )
  n_forward_filled_total <- n_forward_filled_total + sum(master_year$gdp_year_used != master_year$year)
  n_rows_total <- n_rows_total + nrow(master_year)

  write_parquet(master_year, file.path(output_dir, sprintf("year=%d.parquet", yr_int)))
  message("  Wrote year=", yr_int, ".parquet (", nrow(master_year), " rows)")

  rm(master_year, dc_row)
  gc()
}

# -----------------------------------------------------------------------------
# Summary diagnostics (from the small per-year rows collected above -- no
# need to re-read the full written dataset back into memory for this)
# -----------------------------------------------------------------------------

coverage_by_year <- rbindlist(coverage_rows)[order(year)]
message("\n=== TRAVEL COST COVERAGE BY YEAR ===")
print(coverage_by_year)
message(sprintf("\nTotal rows: %s | Forward-filled GDP: %s (%.1f%%)",
                 format(n_rows_total, big.mark = ","),
                 format(n_forward_filled_total, big.mark = ","),
                 100 * n_forward_filled_total / n_rows_total))

# -----------------------------------------------------------------------------
# Marker file -- signals completion for run_task()'s skip-check
# -----------------------------------------------------------------------------
file.create(file.path(output_dir, "_SUCCESS"))
message("\nDone: ", output_dir)