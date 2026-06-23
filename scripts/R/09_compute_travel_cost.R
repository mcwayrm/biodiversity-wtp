#!/usr/bin/env Rscript
# scripts/R/9_compute_travel_cost.R
#############################################
#  - Loads master dataset with attributes
#  - Computes travel cost based on GDP-derived wages and travel parameters
#  - Saves: outputs$master_data_with_travel_cost
#
#  Required params:
#    - projection_crs: CRS for spatial operations
#    - time_value_fraction: Value of time as fraction of wage (default: 1/3)
#    - work_hours_per_year: Annual work hours (default: 2000)
#    - travel_speed_kmph: Average travel speed in km/h (default: 30)
#  Required inputs:
#    - master_data_with_attributes: Output from stage 8
#    - district_shp: District boundaries for home location centroids
#    - gdp_csv: GDP data file (panel 2015-2021; predicted_GCP_current_USD in billions USD)
#    - cpi_csv: FRED INDCPIALLAINMEI CPI index (base 2015=100), for deflating to real 2021 INR
#    - driving_cost_rds: Year-specific driving cost in real 2021 INR/km
#############################################

# -----------------------------------------------------------------------------
# Load Parameters (with defaults)
# -----------------------------------------------------------------------------

# Value of time from wage
time_value_fraction <- if (!is.null(params$time_value_fraction)) {
  params$time_value_fraction
} else {
  1/3  # Value of time assumed as 1/3 of wage (Jayalath et al., 2023)
}

# Annual work hours
work_hours <- if (!is.null(params$work_hours_per_year)) {
  params$work_hours_per_year
} else {
  2000  # 250 working days x 8 hours/day
}

# Average travel speed
travel_speed_kmph <- if (!is.null(params$travel_speed_kmph)) {
  params$travel_speed_kmph
} else {
  30  # Average rural/urban travel speed (MoRTH road studies)
}

# Year-specific exchange rates (USD to INR), used to convert nominal USD GDP to nominal INR
# Driving cost is now loaded from driving_cost.rds (year-specific, real 2021 INR/km)
exchange_df <- tibble(
  year = 2015:2021,
  usd_to_inr = c(64.15, 67.19, 65.12, 68.43, 70.41, 74.10, 73.93)
)

message("Travel cost parameters:")
message("  Time value fraction: ", time_value_fraction)
message("  Work hours/year: ", work_hours)
message("  Travel speed: ", travel_speed_kmph, " km/h")
message("  Driving cost: year-specific from driving_cost.rds (real 2021 INR/km)")

# -----------------------------------------------------------------------------
# Load Master Dataset
# -----------------------------------------------------------------------------

master_data <- read_parquet(inputs$master_data_with_attributes)
setDT(master_data)

message("Loaded master dataset: ", nrow(master_data), " rows")

# -----------------------------------------------------------------------------
# Load District Centroids (for home locations without coordinates)
# -----------------------------------------------------------------------------

dist <- st_read(inputs$district_shp, quiet = TRUE) %>%
  select(c_code_11) %>%
  rename(c_code_2011 = c_code_11) %>%
  st_transform(crs = params$projection_crs)

dist_centroids <- dist %>%
  st_centroid() %>%
  mutate(
    lon_dist = st_coordinates(geometry)[, 1],
    lat_dist = st_coordinates(geometry)[, 2]
  ) %>%
  st_drop_geometry()

# Merge district centroids if needed (for users without home coordinates)
if (!all(c("lon_home", "lat_home") %in% names(master_data))) {
  master_data <- master_data %>%
    left_join(dist_centroids, by = "c_code_2011")
}

# -----------------------------------------------------------------------------
# Load CPI Data  (FRED — INDCPIALLAINMEI, base 2015=100)
# -----------------------------------------------------------------------------
#
# Used to deflate nominal INR GDP per capita to real 2021 INR so that wages are
# comparable in constant-purchasing-power terms across the panel.
# Deflation formula: real_t = nominal_t x (CPI_2021 / CPI_t)

message("Loading CPI data...")
cpi_raw <- read_csv(inputs$cpi_csv, show_col_types = FALSE)

cpi_df <- cpi_raw %>%
  mutate(year = lubridate::year(as.Date(observation_date))) %>%
  rename(cpi = INDCPIALLAINMEI) %>%
  select(year, cpi)

cpi_2021 <- cpi_df$cpi[cpi_df$year == 2021]
if (length(cpi_2021) == 0) stop("ERROR: CPI value for 2021 not found — cannot deflate to real 2021 INR!")
message("CPI base value (2021): ", cpi_2021)

# -----------------------------------------------------------------------------
# Load Year-Specific Driving Cost
# -----------------------------------------------------------------------------
#
# Driving cost per km varies by year and is measured in real 2021 INR/km.
# Replaces the earlier fixed assumption of INR 7.5/km.

message("Loading driving cost data...")
driving_cost_df <- readRDS(inputs$driving_cost_rds) %>%
  mutate(
    year = as.integer(year),
    driving_cost = as.numeric(driving_cost)
  ) %>%
  filter(year >= 2015 & year <= 2021)

message("Driving cost years available: ", paste(sort(driving_cost_df$year), collapse = ", "))

# -----------------------------------------------------------------------------
# Load and Process GDP Data (panel: 2015-2021)
# -----------------------------------------------------------------------------
#
# Processing steps per year:
#   1. GDP per capita (USD) = (predicted_GCP_current_USD * 1e9) / pop_cell
#   2. Nominal INR          = GDP per capita (USD) x year-specific USD/INR rate
#   3. Real 2021 INR        = Nominal INR x (CPI_2021 / CPI_t)

message("Loading GDP data...")
gdp_all <- fread(inputs$gdp)

# Verify GDP column exists
if (!("predicted_GCP_current_USD" %in% names(gdp_all))) {
  stop("Cannot proceed: 'predicted_GCP_current_USD' column not found. ",
       "Available columns: ", paste(names(gdp_all), collapse = ", "))
}

# Filter for India across the full panel range
gdp_india <- gdp_all[iso == "IND" & year >= 2015 & year <= 2021]
years_in_india <- sort(unique(gdp_india$year))
message("Available years for India (2015-2021): ", paste(years_in_india, collapse = ", "))

if (length(years_in_india) == 0) stop("ERROR: No India GDP data found for 2015-2021!")

# Remove zero-population cells (unreliable)
gdp_india <- gdp_india[pop_cell > 0]
message("GDP records with pop > 0: ", nrow(gdp_india))

# Convert to data.frame for dplyr joins
gdp_india <- as.data.frame(gdp_india)

# Step 1 & 2: GDP per capita in USD -> nominal INR (year-specific exchange rate)
gdp_india <- gdp_india %>%
  left_join(exchange_df, by = "year") %>%
  mutate(
    gdppc_usd          = (predicted_GCP_current_USD * 1e9) / pop_cell,
    gdppc_nominal_INR  = gdppc_usd * usd_to_inr
  )

# Step 3: Deflate nominal INR to real 2021 INR using CPI
gdp_india <- gdp_india %>%
  left_join(cpi_df, by = "year") %>%
  mutate(
    gdppc_real_2021_INR = round(gdppc_nominal_INR * (cpi_2021 / cpi), 2)
  )

message("\nGDP per capita (real 2021 INR) across panel years:")
gdp_india %>%
  group_by(year) %>%
  summarise(mean_gdppc = round(mean(gdppc_real_2021_INR, na.rm = TRUE), 0), .groups = "drop") %>%
  { message(paste(sprintf("  %d: ₹%s", .$year, format(.$mean_gdppc, big.mark = ",")), collapse = "\n")) }

# Convert to sf for spatial matching (keep year column for per-year join below)
gdp_india_sf <- st_as_sf(gdp_india, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = params$projection_crs)

# -----------------------------------------------------------------------------
# Match Home Locations to Nearest GDP Centroid — Per Year
# -----------------------------------------------------------------------------
#
# For each year in the panel, match each user's home location to the nearest
# GDP grid cell for *that year*, then compute hourly wage from real 2021 INR GDP.

message("Matching home locations to GDP data (per year)...")

master_data <- as.data.frame(master_data)
master_data$year <- as.integer(master_data$year)

master_data_list <- split(master_data, master_data$year)

matched_list <- lapply(names(master_data_list), function(yr) {

  yr_int <- as.integer(yr)
  master_year <- master_data_list[[yr]]

  gdp_year <- gdp_india_sf %>% filter(year == yr_int)

  if (nrow(gdp_year) == 0) {
    message("WARNING: No GDP data for year ", yr_int, " — gdppc_real_2021_INR will be NA")
    master_year$gdppc_real_2021_INR <- NA_real_
    return(master_year)
  }

  homes_sf <- st_as_sf(master_year, coords = c("lon_home", "lat_home"), crs = 4326) %>%
    st_transform(crs = params$projection_crs)

  nearest_indices <- st_nearest_feature(homes_sf, gdp_year)
  master_year$gdppc_real_2021_INR <- gdp_year$gdppc_real_2021_INR[nearest_indices]

  message("  Year ", yr_int, ": matched ", nrow(master_year), " rows")
  master_year
})

master_data <- bind_rows(matched_list)
setDT(master_data)

# Hourly wage from real 2021 INR GDP per capita
master_data[, hourly_wage := gdppc_real_2021_INR / work_hours]

users_with_gdp <- master_data[!is.na(gdppc_real_2021_INR), uniqueN(user_id)]
users_total    <- master_data[, uniqueN(user_id)]
message("GDP data matched for ", users_with_gdp, " / ", users_total, " users")

if (users_with_gdp < users_total) {
  message("WARNING: ", users_total - users_with_gdp, " users missing GDP data!")
}

# Diagnostic: Check GDP and wage ranges
message("\n=== GDP DATA DIAGNOSTICS ===")
message("GDP per capita (annual, real 2021 INR):")
message("  Min: ₹",    round(min(master_data$gdppc_real_2021_INR, na.rm = TRUE), 2))
message("  Mean: ₹",   round(mean(master_data$gdppc_real_2021_INR, na.rm = TRUE), 2))
message("  Median: ₹", round(median(master_data$gdppc_real_2021_INR, na.rm = TRUE), 2))
message("  Max: ₹",    round(max(master_data$gdppc_real_2021_INR, na.rm = TRUE), 2))
message("Hourly wage (real 2021 INR/hour):")
message("  Min: ₹",    round(min(master_data$hourly_wage, na.rm = TRUE), 2))
message("  Mean: ₹",   round(mean(master_data$hourly_wage, na.rm = TRUE), 2))
message("  Median: ₹", round(median(master_data$hourly_wage, na.rm = TRUE), 2))
message("  Max: ₹",    round(max(master_data$hourly_wage, na.rm = TRUE), 2))

# -----------------------------------------------------------------------------
# Calculate Travel Costs
# -----------------------------------------------------------------------------

message("\nComputing travel costs (all values in real 2021 INR)...")

# Ensure distance is numeric and year is integer
master_data[, geo_dist := as.numeric(geo_dist)]
master_data[, year := as.integer(year)]

# Diagnostic: Check distance range BEFORE calculations
message("\n=== DISTANCE DIAGNOSTICS ===")
message("Distance (km):")
message("  Min: ",    round(min(master_data$geo_dist, na.rm = TRUE), 2), " km")
message("  Mean: ",   round(mean(master_data$geo_dist, na.rm = TRUE), 2), " km")
message("  Median: ", round(median(master_data$geo_dist, na.rm = TRUE), 2), " km")
message("  Max: ",    round(max(master_data$geo_dist, na.rm = TRUE), 2), " km")
message("  NA count: ", sum(is.na(master_data$geo_dist)))
if (sum(master_data$geo_dist == 0, na.rm = TRUE) > 0) {
  message("  WARNING: ", sum(master_data$geo_dist == 0, na.rm = TRUE), " observations with 0 km distance!")
}

# Join year-specific driving cost (real 2021 INR/km)
master_data <- master_data[as.data.table(driving_cost_df), on = "year", nomatch = NA]

missing_driving_cost <- sum(is.na(master_data$driving_cost))
if (missing_driving_cost > 0) {
  message("WARNING: ", missing_driving_cost, " rows missing driving_cost after join (check year coverage)")
}

# Calculate travel cost components (all in real 2021 INR)
master_data[, `:=`(
  # One-way travel time (hours)
  travel_time_hours = geo_dist / travel_speed_kmph,
  # Round-trip time cost = 2 x (time value fraction) x wage x time (real 2021 INR)
  time_cost = 2 * time_value_fraction * hourly_wage * (geo_dist / travel_speed_kmph),
  # One-way fuel cost using year-specific driving cost (real 2021 INR)
  fuel_cost = geo_dist * driving_cost
)]

# Total travel cost in INR
master_data[, travel_cost_combined := time_cost + fuel_cost]

# Add log transformation (avoiding log(0))
master_data[, log_travel_cost := log1p(travel_cost_combined)]

# Diagnostic: Check intermediate calculations
message("\n=== PRE-SUMMARY DIAGNOSTICS ===")
message("Time cost (INR):")
message("  NA count: ", sum(is.na(master_data$time_cost)))
message("  Min: ₹", round(min(master_data$time_cost, na.rm = TRUE), 2))
message("  Mean: ₹", round(mean(master_data$time_cost, na.rm = TRUE), 2))
message("  Max: ₹", round(max(master_data$time_cost, na.rm = TRUE), 2))
message("Fuel cost (INR):")
message("  NA count: ", sum(is.na(master_data$fuel_cost)))
message("  Min: ₹", round(min(master_data$fuel_cost, na.rm = TRUE), 2))
message("  Mean: ₹", round(mean(master_data$fuel_cost, na.rm = TRUE), 2))
message("  Max: ₹", round(max(master_data$fuel_cost, na.rm = TRUE), 2))

message("\n=== TRAVEL COST SUMMARY (real 2021 INR) ===")
message("Total observations: ", nrow(master_data))
message("NA count in travel_cost_combined: ", sum(is.na(master_data$travel_cost_combined)))
message("\nTravel cost statistics (INR):")
message("  Mean: ₹", round(mean(master_data$travel_cost_combined, na.rm = TRUE), 2))
message("  Median: ₹", round(median(master_data$travel_cost_combined, na.rm = TRUE), 2))
message("  Min: ₹", round(min(master_data$travel_cost_combined, na.rm = TRUE), 2))
message("  Max: ₹", round(max(master_data$travel_cost_combined, na.rm = TRUE), 2))
message("  SD: ₹", round(sd(master_data$travel_cost_combined, na.rm = TRUE), 2))

# Sanity check: Time cost vs Fuel cost ratio
time_cost_mean <- mean(master_data$time_cost, na.rm = TRUE)
fuel_cost_mean <- mean(master_data$fuel_cost, na.rm = TRUE)
message("\nTime cost vs Fuel cost (INR):")
message("  Time cost (mean): ₹", round(time_cost_mean, 2))
message("  Fuel cost (mean): ₹", round(fuel_cost_mean, 2))
message("  Split: ", round(100 * time_cost_mean / (time_cost_mean + fuel_cost_mean), 1), "% time, ",
        round(100 * fuel_cost_mean / (time_cost_mean + fuel_cost_mean), 1), "% fuel")

# -----------------------------------------------------------------------------
# Save Output
# -----------------------------------------------------------------------------
write_parquet(master_data, outputs$master_data_with_travel_cost)