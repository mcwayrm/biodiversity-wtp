#!/usr/bin/env Rscript
# scripts/R/7_compute_biodiversity_metrics.R
#############################################
#  - Loads eBird data and Voronoi polygons
#  - Computes species richness and congestion metrics at multiple temporal scales
#  - Saves: outputs$monthly_richness, outputs$weekly_richness, outputs$seasonal_richness,
#           outputs$monthly_congestion, outputs$weekly_congestion, outputs$seasonal_congestion
#
#  Required params:
#    - projection_crs: CRS for spatial operations
#############################################

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------

message("Loading eBird data...")
ebird <- fread(
  inputs$ebird_basic,
  select = c('LATITUDE', 'LONGITUDE', 'OBSERVATION DATE',
             'OBSERVER ID', 'SAMPLING EVENT IDENTIFIER',
             'PROTOCOL TYPE', 'DURATION MINUTES',
             'EFFORT DISTANCE KM', 'ALL SPECIES REPORTED',
             'LOCALITY', 'LOCALITY TYPE', 'COMMON NAME', 'CATEGORY'),
  quote = ""
)

# Clean column names
setnames(ebird, gsub('\\.', '_', tolower(make.names(names(ebird)))))

# Filter and clean data
ebird[, observation_date := as.Date(observation_date)]
dt <- ebird[
  locality_type == "H" & category == "species" &
    !is.na(observation_date) & !is.na(common_name),
  .(lat = latitude, lon = longitude, observation_date, common_name, 
    sampling_event_identifier, observer_id, protocol_type, 
    duration_minutes, effort_distance_km, all_species_reported)
]

message("Filtered eBird data: ", nrow(dt), " species observations")

# -----------------------------------------------------------------------------
# Create Trip-Level Data
# -----------------------------------------------------------------------------

trip_data <- dt[, .(
  species_richness = uniqueN(common_name),
  lat = first(lat),
  lon = first(lon)
), by = .(sampling_event_identifier, observation_date, observer_id, protocol_type, 
          duration_minutes, effort_distance_km, all_species_reported)]

message("Created trip-level data: ", nrow(trip_data), " trips")

# -----------------------------------------------------------------------------
# Assign Cluster IDs via Spatial Join
# -----------------------------------------------------------------------------

# Load Voronoi polygons
voronoi_polygons <- st_read(inputs$voronoi_shp, 
                           layer = "cluster_voronoi_limited",
                           quiet = TRUE)

# Convert trips to sf and transform
trip_df <- as.data.frame(trip_data)
trip_sf <- st_as_sf(trip_df, coords = c('lon', 'lat'), crs = 4326) %>% 
  st_transform(crs = params$projection_crs)

# Transform voronoi to match
voronoi_proj <- st_transform(voronoi_polygons, crs = params$projection_crs)

# Spatial join
trip_sf <- st_join(trip_sf, voronoi_proj[, c("cluster_id")], left = TRUE)

# Convert back to data.table
trip_richness <- as.data.table(st_drop_geometry(trip_sf))

# Remove trips without cluster assignments
trip_richness <- trip_richness[!is.na(cluster_id)]
message("Trips with cluster assignments: ", nrow(trip_richness))

# -----------------------------------------------------------------------------
# Add Temporal Variables
# -----------------------------------------------------------------------------

trip_richness[, cluster_id := as.character(cluster_id)]
trip_richness[, year := year(observation_date)]
trip_richness[, month := month(observation_date)]
trip_richness[, week := isoweek(observation_date)]
trip_richness[, year_week := sprintf("%04d-%02d", isoyear(observation_date), isoweek(observation_date))]
trip_richness[, year_month := format(observation_date, "%Y-%m")]

# Create season variable
trip_richness[, season := "Fall"]
trip_richness[month %in% c(12, 1, 2), season := "Winter"]
trip_richness[month %in% c(3, 4, 5), season := "Spring"]
trip_richness[month %in% c(6, 7, 8), season := "Summer"]
trip_richness[, year_season := paste0(year, "-", season)]

# -----------------------------------------------------------------------------
# Compute Species Richness Metrics
# -----------------------------------------------------------------------------

message("\nComputing species richness metrics...")

# Weekly
weekly_richness <- trip_richness[, .(
  richness = mean(species_richness, na.rm = TRUE),
  n_trips = .N
), by = .(cluster_id, year_week)]

write_parquet(weekly_richness, outputs$weekly_richness)
message("Weekly richness: ", nrow(weekly_richness), " records")

# Monthly
monthly_richness <- trip_richness[, .(
  richness = mean(species_richness, na.rm = TRUE),
  n_trips = .N
), by = .(cluster_id, year_month)]

write_parquet(monthly_richness, outputs$monthly_richness)
message("Monthly richness: ", nrow(monthly_richness), " records")

# Seasonal
seasonal_richness <- trip_richness[, .(
  richness = mean(species_richness, na.rm = TRUE),
  n_trips = .N
), by = .(cluster_id, year_season)]

write_parquet(seasonal_richness, outputs$seasonal_richness)
message("Seasonal richness: ", nrow(seasonal_richness), " records")

# -----------------------------------------------------------------------------
# Compute Congestion Metrics
# -----------------------------------------------------------------------------

message("\nComputing congestion metrics...")

# Weekly
weekly_congestion <- trip_richness[, .(
  n_users = uniqueN(observer_id),
  n_trips = .N
), by = .(cluster_id, year_week)]

write_parquet(weekly_congestion, outputs$weekly_congestion)
message("Weekly congestion: ", nrow(weekly_congestion), " records")

# Monthly
monthly_congestion <- trip_richness[, .(
  n_users = uniqueN(observer_id),
  n_trips = .N
), by = .(cluster_id, year_month)]

write_parquet(monthly_congestion, outputs$monthly_congestion)
message("Monthly congestion: ", nrow(monthly_congestion), " records")

# Seasonal
seasonal_congestion <- trip_richness[, .(
  n_users = uniqueN(observer_id),
  n_trips = .N
), by = .(cluster_id, year_season)]

write_parquet(seasonal_congestion, outputs$seasonal_congestion)
message("Seasonal congestion: ", nrow(seasonal_congestion), " records")

message("\n=== BIODIVERSITY METRICS COMPUTATION COMPLETE ===")