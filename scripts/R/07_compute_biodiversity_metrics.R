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
              'LOCALITY', 'LOCALITY TYPE', 'COMMON NAME', 'CATEGORY',
              'OBSERVATION COUNT'),
  quote = ""
)

# Clean column names
setnames(ebird, gsub('\\.', '_', tolower(make.names(names(ebird)))))
ebird[, observation_count := as.numeric(observation_count)]


# Filter and clean data
ebird[, observation_date := as.Date(observation_date)]
dt <- ebird[
  locality_type == "H" & category == "species" &
    !is.na(observation_date) & !is.na(common_name),
  .(lat = latitude, lon = longitude, observation_date, common_name,
    sampling_event_identifier, observer_id, protocol_type,
    duration_minutes, effort_distance_km, all_species_reported, observation_count)
]

message("Filtered eBird data: ", nrow(dt), " species observations")

# -----------------------------------------------------------------------------
# Create Trip-Level Data
# -----------------------------------------------------------------------------

# Load species categorization for migrant/resident classification
species_info <- fread(inputs$migrant_species)

# Clean and standardize migratory_status and species names
# 1. Recode migratory_status to standardized labels
species_info[, migratory_status := fifelse(
  grepl("resident|local|india|altitudinal|nomadic", migratory_status, ignore.case = TRUE), "resident",
  fifelse(grepl("passage migrant|winter migrant|summer migrant", migratory_status, ignore.case = TRUE), "migrant", migratory_status)
)]
# 2. Clean species name: remove extra spaces, lowercase, capitalize genus
species_info[, species := gsub("\\s+", " ", trimws(tolower(species)))]
species_info[, genus := tools::toTitleCase(sub(" .*", "", species))]
species_info[, species_epithet := sub("^[^ ]+ ", "", species)]
species_info[, species := paste(genus, species_epithet)]
# 3. Deduplicate by species name
species_info <- species_info[!duplicated(species)]

# Lowercase common_name for consistency
dt[, common_name := tolower(common_name)]
species_info[, species := tolower(species)]

# Create a species matching log: for each species in species_info, indicate if it matched any dt$common_name
species_info[, match := ifelse(species %in% dt$common_name, TRUE, FALSE)]
species_matching_log <- species_info[, .(species, migratory_status, match)]
write.csv(species_matching_log, outputs$species_matching_log, row.names = FALSE)
message("Species matching log written to: ", outputs$species_matching_log)

dt <- merge(dt, species_info[, .(common_name = species, migratory_status)], by = "common_name", all.x = TRUE)

# Compute indices and richness per trip
trip_data <- dt[, .(
  species_richness = uniqueN(common_name),
  migrant_richness = uniqueN(common_name[migratory_status %like% "migrant"]),
  resident_richness = uniqueN(common_name[migratory_status %like% "resident"]),
  shannon_index = {
    counts <- as.numeric(tapply(observation_count, common_name, sum, na.rm = TRUE))
    counts <- counts[!is.na(counts) & counts > 0]
    if (length(counts) == 0) NA_real_ else {
      p <- counts / sum(counts)
      -sum(p * log(p))
    }
  },
  simpson_index = {
    counts <- as.numeric(tapply(observation_count, common_name, sum, na.rm = TRUE))
    counts <- counts[!is.na(counts) & counts > 0]
    if (length(counts) == 0) NA_real_ else {
      p <- counts / sum(counts)
      1 - sum(p^2)
    }
  },
  # shannon_index = {
  #   p <- table(common_name) / .N
  #   -sum(p * log(p))
  # },
  # simpson_index = {
  #   p <- table(common_name) / .N
  #   1 - sum(p^2)
  # },
  lat = first(lat),
  lon = first(lon)
), by = .(sampling_event_identifier, observation_date, observer_id, protocol_type, 
          duration_minutes, effort_distance_km, all_species_reported)]

print("Rows with NA Shannon index: ")
print(sum(is.na(trip_data$shannon_index)))
print("Rows with NA Simpson index: ")
print(sum(is.na(trip_data$simpson_index)))
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
# Compute Indices and Migrant/Resident Richness
# -----------------------------------------------------------------------------
message("\nComputing Shannon/Simpson indices and migrant/resident richness...")
# Weekly
weekly_shannon <- trip_richness[, .(
  shannon_index = mean(shannon_index, na.rm = TRUE),
  n_trips = .N
), by = .(cluster_id, year_week)]
weekly_simpson <- trip_richness[, .(
  simpson_index = mean(simpson_index, na.rm = TRUE),
  n_trips = .N
), by = .(cluster_id, year_week)]
weekly_migrant <- trip_richness[, .(
  migrant_richness = mean(migrant_richness, na.rm = TRUE),
  n_trips = .N
), by = .(cluster_id, year_week)]
weekly_resident <- trip_richness[, .(
  resident_richness = mean(resident_richness, na.rm = TRUE),
  n_trips = .N
), by = .(cluster_id, year_week)]
write_parquet(weekly_shannon, outputs$weekly_shannon)
write_parquet(weekly_simpson, outputs$weekly_simpson)
write_parquet(weekly_migrant, outputs$weekly_migrant)
write_parquet(weekly_resident, outputs$weekly_resident)

# Monthly
monthly_shannon <- trip_richness[, .(
  shannon_index = mean(shannon_index, na.rm = TRUE),
  n_trips = .N
), by = .(cluster_id, year_month)]
monthly_simpson <- trip_richness[, .(
  simpson_index = mean(simpson_index, na.rm = TRUE),
  n_trips = .N
), by = .(cluster_id, year_month)]
monthly_migrant <- trip_richness[, .(
  migrant_richness = mean(migrant_richness, na.rm = TRUE),
  n_trips = .N
), by = .(cluster_id, year_month)]
monthly_resident <- trip_richness[, .(
  resident_richness = mean(resident_richness, na.rm = TRUE),
  n_trips = .N
), by = .(cluster_id, year_month)]
write_parquet(monthly_shannon, outputs$monthly_shannon)
write_parquet(monthly_simpson, outputs$monthly_simpson)
write_parquet(monthly_migrant, outputs$monthly_migrant)
write_parquet(monthly_resident, outputs$monthly_resident)

# Seasonal
seasonal_shannon <- trip_richness[, .(
  shannon_index = mean(shannon_index, na.rm = TRUE),
  n_trips = .N
), by = .(cluster_id, year_season)]
seasonal_simpson <- trip_richness[, .(
  simpson_index = mean(simpson_index, na.rm = TRUE),
  n_trips = .N
), by = .(cluster_id, year_season)]
seasonal_migrant <- trip_richness[, .(
  migrant_richness = mean(migrant_richness, na.rm = TRUE),
  n_trips = .N
), by = .(cluster_id, year_season)]
seasonal_resident <- trip_richness[, .(
  resident_richness = mean(resident_richness, na.rm = TRUE),
  n_trips = .N
), by = .(cluster_id, year_season)]
write_parquet(seasonal_shannon, outputs$seasonal_shannon)
write_parquet(seasonal_simpson, outputs$seasonal_simpson)
write_parquet(seasonal_migrant, outputs$seasonal_migrant)
write_parquet(seasonal_resident, outputs$seasonal_resident)
message("\nShannon/Simpson indices and migrant/resident richness computed.")

# -----------------------------------------------------------------------------
# Compute Species Rarity Metrics
# -----------------------------------------------------------------------------

message("\nComputing species rarity metrics...")

# Use checklist counts where available; otherwise treat presence as one observation.
dt[, obs_weight := fifelse(is.na(observation_count) | observation_count <= 0, 1, observation_count)]

# Attach hotspot/time identifiers to species-level observations.
hotspot_species_obs <- merge(
  dt[, .(sampling_event_identifier, observer_id, common_name, obs_weight)],
  trip_richness[, .(sampling_event_identifier, cluster_id, year_week, year_month, year_season)],
  by = "sampling_event_identifier",
  all.x = TRUE
)
hotspot_species_obs <- hotspot_species_obs[!is.na(cluster_id)]

# User-specific species frequencies and rarity weights.
user_species_obs <- hotspot_species_obs[, .(
  user_species_obs = sum(obs_weight, na.rm = TRUE)
), by = .(observer_id, common_name)]
user_total_obs <- user_species_obs[, .(
  user_total_obs = sum(user_species_obs, na.rm = TRUE)
), by = observer_id]
user_species_obs <- merge(user_species_obs, user_total_obs, by = "observer_id", all.x = TRUE)
user_species_obs[, p_i := pmax(user_species_obs / user_total_obs, .Machine$double.eps)]
user_species_obs[, rarity_weight := -log(p_i)]

compute_rarity_metrics <- function(period_col) {
  species_counts <- hotspot_species_obs[, .(
    c_hi = sum(obs_weight, na.rm = TRUE)
  ), by = c("cluster_id", period_col, "observer_id", "common_name")]

  species_counts <- merge(
    species_counts,
    user_species_obs[, .(observer_id, common_name, p_i, rarity_weight)],
    by = c("observer_id", "common_name"),
    all.x = TRUE
  )

  effort <- species_counts[, .(
    N_h = sum(c_hi, na.rm = TRUE)
  ), by = c("cluster_id", period_col, "observer_id")]

  species_counts <- merge(
    species_counts,
    effort,
    by = c("cluster_id", period_col, "observer_id"),
    all.x = TRUE
  )

  user_rarity <- species_counts[, .(
    rarity_index = sum(log1p(c_hi) * rarity_weight, na.rm = TRUE),
    rarity_enrichment_index = sum(rarity_weight * log((c_hi + 1) / (N_h * p_i + 1)), na.rm = TRUE),
    n_species_user = uniqueN(common_name),
    total_observations_user = sum(c_hi, na.rm = TRUE)
  ), by = c("cluster_id", period_col, "observer_id")]

  rarity_metrics <- user_rarity[, .(
    rarity_index = mean(rarity_index, na.rm = TRUE),
    rarity_enrichment_index = mean(rarity_enrichment_index, na.rm = TRUE),
    n_species = mean(n_species_user, na.rm = TRUE),
    n_users = uniqueN(observer_id),
    total_observations = sum(total_observations_user, na.rm = TRUE)
  ), by = c("cluster_id", period_col)]

  trip_counts <- trip_richness[, .(n_trips = .N), by = c("cluster_id", period_col)]
  rarity_metrics <- merge(rarity_metrics, trip_counts, by = c("cluster_id", period_col), all.x = TRUE)

  rarity_metrics
}

# Weekly
weekly_rarity <- compute_rarity_metrics("year_week")
write_parquet(weekly_rarity, outputs$weekly_rarity)
message("Weekly rarity: ", nrow(weekly_rarity), " records")

# Monthly
monthly_rarity <- compute_rarity_metrics("year_month")
write_parquet(monthly_rarity, outputs$monthly_rarity)
message("Monthly rarity: ", nrow(monthly_rarity), " records")

# Seasonal
seasonal_rarity <- compute_rarity_metrics("year_season")
write_parquet(seasonal_rarity, outputs$seasonal_rarity)
message("Seasonal rarity: ", nrow(seasonal_rarity), " records")

message("\nSpecies rarity metrics computed.")

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