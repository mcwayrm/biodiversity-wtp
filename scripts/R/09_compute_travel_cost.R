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
#    - driving_cost_per_km: Vehicle operating cost in INR/km (default: 7.5)
#  Required inputs:
#    - master_data_with_attributes: Output from stage 8
#    - district_shp: District boundaries for home location centroids
#    - gdp_csv: GDP data file
#############################################

# -----------------------------------------------------------------------------
# Load Parameters (with defaults)
# -----------------------------------------------------------------------------

time_value_fraction <- if (!is.null(params$time_value_fraction)) {
  params$time_value_fraction
} else {
  1/3  # Value of time assumed as 1/3 of wage (Jayalath et al., 2023)
}

work_hours <- if (!is.null(params$work_hours_per_year)) {
  params$work_hours_per_year
} else {
  2000  # 250 working days x 8 hours/day
}

travel_speed_kmph <- if (!is.null(params$travel_speed_kmph)) {
  params$travel_speed_kmph
} else {
  30  # Average rural/urban travel speed (MoRTH road studies)
}

driving_cost_per_km <- if (!is.null(params$driving_cost_per_km)) {
  params$driving_cost_per_km
} else {
  7.5  # INR/km vehicle running cost (Times of India, 2024)
}

message("Travel cost parameters:")
message("  Time value fraction: ", time_value_fraction)
message("  Work hours/year: ", work_hours)
message("  Travel speed: ", travel_speed_kmph, " km/h")
message("  Driving cost: ", driving_cost_per_km, " INR/km")

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
# Load and Process GDP Data
# -----------------------------------------------------------------------------

message("Loading GDP data...")
gdp_all <- fread(inputs$gdp)

gdp_india <- gdp_all[iso == "IND" & year >= 2014 & year <= 2024]

# Convert GDP data to spatial features
gdp_india_sf <- st_as_sf(gdp_india, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = params$projection_crs)

# Keep one point per grid cell for year 2020
gdp_summary <- gdp_india_sf %>%
  filter(year == 2020) %>%
  group_by(cell_id) %>%
  summarize(geometry = st_centroid(st_union(geometry)), .groups = "drop")

# Merge GDP per capita values
gdp_centroids <- left_join(
  gdp_summary,
  as.data.frame(gdp_india)[gdp_india$year == 2020, c("cell_id", "predicted_GCP_current_USD")],
  by = "cell_id"
) %>%
  rename(gdppc = predicted_GCP_current_USD)

message("GDP centroids: ", nrow(gdp_centroids), " locations")

# -----------------------------------------------------------------------------
# Match Home Locations to Nearest GDP Centroid
# -----------------------------------------------------------------------------

message("Matching home locations to GDP data...")

# Get unique home locations
unique_homes <- master_data[, .(lon_home = first(lon_home), 
                                lat_home = first(lat_home)), 
                            by = .(user_id)]

# Convert to sf
homes_sf <- st_as_sf(unique_homes, coords = c("lon_home", "lat_home"), crs = 4326) %>%
  st_transform(crs = params$projection_crs)

# Find nearest GDP centroid for each home
nearest_indices <- st_nearest_feature(homes_sf, gdp_centroids)
unique_homes[, gdppc := gdp_centroids$gdppc[nearest_indices]]
unique_homes[, hourly_wage := gdppc / work_hours]

# Merge back to master data
master_data <- master_data[unique_homes[, .(user_id, gdppc, hourly_wage)], 
                          on = "user_id"]

message("GDP data matched for ", master_data[!is.na(gdppc), uniqueN(user_id)], " users")

# -----------------------------------------------------------------------------
# Calculate Travel Costs
# -----------------------------------------------------------------------------

message("Computing travel costs...")

# Ensure distance is numeric
master_data[, geo_dist := as.numeric(geo_dist)]

# Calculate travel cost components
master_data[, `:=`(
  # One-way travel time (hours)
  travel_time_hours = geo_dist / travel_speed_kmph,
  # Round-trip time cost = 2 x (time value fraction) x wage x time
  time_cost = 2 * time_value_fraction * hourly_wage * (geo_dist / travel_speed_kmph),
  # One-way fuel cost
  fuel_cost = geo_dist * driving_cost_per_km * 
)]

# Total travel cost
master_data[, travel_cost_combined := time_cost + fuel_cost]

# Add log transformation (avoiding log(0))
master_data[, log_travel_cost := log1p(travel_cost_combined)]

# -----------------------------------------------------------------------------
# Summary Statistics
# -----------------------------------------------------------------------------

message("\n=== TRAVEL COST SUMMARY ===")
message("Total observations: ", nrow(master_data))
message("Travel cost statistics (INR):")
message("  Mean: ", round(mean(master_data$travel_cost_combined, na.rm = TRUE), 2))
message("  Median: ", round(median(master_data$travel_cost_combined, na.rm = TRUE), 2))
message("  Min: ", round(min(master_data$travel_cost_combined, na.rm = TRUE), 2))
message("  Max: ", round(max(master_data$travel_cost_combined, na.rm = TRUE), 2))
message("  SD: ", round(sd(master_data$travel_cost_combined, na.rm = TRUE), 2))

message("\nDistance statistics (km):")
message("  Mean: ", round(mean(master_data$geo_dist, na.rm = TRUE), 2))
message("  Median: ", round(median(master_data$geo_dist, na.rm = TRUE), 2))

message("\nHourly wage statistics (INR/hour):")
message("  Mean: ", round(mean(master_data$hourly_wage, na.rm = TRUE), 2))
message("  Median: ", round(median(master_data$hourly_wage, na.rm = TRUE), 2))

# -----------------------------------------------------------------------------
# Save Output
# -----------------------------------------------------------------------------
write_parquet(master_data, outputs$master_data_with_travel_cost)