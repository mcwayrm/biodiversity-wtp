#!/usr/bin/env Rscript
# scripts/R/5_create_choice_set.R
#############################################
#  - Loads clustered hotspots and trip data
#  - Generates choice sets for each user based on Voronoi clusters
#  - Creates stacked choice table (trips + counterfactuals)
#  - Saves: outputs$master_data
#
#  Required params:
#    - projection_crs: CRS for distance calculations (e.g., 32643 for India UTM)
#    - choice_radius_km: Buffer radius around home for available choices
#    - max_alternatives: Maximum number of alternatives per user (default 30)
#############################################

# -----------------------------------------------------------------------------
# Generate Choice Set for Each User
# -----------------------------------------------------------------------------
generate_choice_sets <- function(clustered_hotspots, trip_df, radius_km, max_alts = 30) {
  
  # Convert to data.table for efficiency
  td <- as.data.table(trip_df)
  hd <- as.data.table(clustered_hotspots)
  
  # Get unique users within radius
  print("Debug: Generating users_in_radius")
  users_in_radius <- unique(td[geo_dist <= radius_km, .(user_id, lon_home, lat_home)])
  
  message("Finding available choices for ", nrow(users_in_radius), " users within ", radius_km, "km radius")
  
  # Transform to projected coordinates for accurate distance calculations
  user_coords <- st_as_sf(users_in_radius, coords = c('lon_home', 'lat_home'), crs = 4326) %>%
    st_transform(params$projection_crs)
  
  hotspot_coords <- st_as_sf(hd, coords = c('lon', 'lat'), crs = 4326) %>%
    st_transform(params$projection_crs)
  
  # Create buffers around user homes (in meters)
  user_buffers <- st_buffer(user_coords, radius_km * 1000)
  
  # Find hotspots within each user's buffer
  choices_within_buffer <- st_join(user_buffers, hotspot_coords, join = st_intersects) %>%
    filter(!is.na(cluster_id)) %>%
    st_drop_geometry() %>%
    as.data.table()
  
  # Get unique clusters (one representative location per cluster - using centroid)
  cluster_centroids <- hd[, .(
    lat = mean(lat, na.rm = TRUE),
    lon = mean(lon, na.rm = TRUE)
  ), by = cluster_id]
  
  # Add back the original coordinates
  choices_with_coords <- choices_within_buffer[
    users_in_radius[, .(user_id, lon_home, lat_home)], 
    on = "user_id"
  ][
    cluster_centroids, 
    on = "cluster_id"
  ]
  
  # Calculate precise distances
  choices_with_coords[, geo_dist := {
    mapply(function(lon_h, lat_h, lon_c, lat_c) {
      as.numeric(st_distance(
        st_sfc(st_point(c(lon_h, lat_h)), crs = 4326) %>% st_transform(params$projection_crs),
        st_sfc(st_point(c(lon_c, lat_c)), crs = 4326) %>% st_transform(params$projection_crs)
      )) / 1000
    }, lon_home, lat_home, lon, lat)
  }]
  
  # Remove duplicates: keep the entry with smallest distance for each (user_id, cluster_id) pair
  choices_unique <- choices_with_coords[order(geo_dist)][
    , .SD[1], by = .(user_id, cluster_id)
  ]
  
  # Limit choice sets to maximum alternatives (keep closest)
  n_before <- nrow(choices_unique)
  choices_unique[, rank := frank(geo_dist), by = user_id]
  choice_sets_sampled <- choices_unique[rank <= max_alts][, rank := NULL]
  
  message("Choice set size - Before: ", n_before, " After: ", nrow(choice_sets_sampled))
  
  return(choice_sets_sampled)
}

# -----------------------------------------------------------------------------
# Stack Choice Sets for Individual Trips
# -----------------------------------------------------------------------------
create_trip_choice_stacks <- function(trip_data, choice_data) {
  
  td <- as.data.table(trip_data)
  cd <- as.data.table(choice_data)
  
  if (!"user_id" %in% names(td) || !"user_id" %in% names(cd)) {
    stop("user_id must be present in both trip_data and choice_data")
  }
  
  # Unique trip_id check
  trip_id_counts <- td[, .N, by = trip_id]
  if (any(trip_id_counts$N > 1)) stop("Duplicate trip_id values found in trip_data")
  
  # Unique choice check
  choice_counts <- cd[, .N, by = c("user_id", "cluster_id")]
  if (any(choice_counts$N > 1)) stop("Duplicate choice values found in choice_data")
  
  # Remove duplicate columns before merge
  duplicate_cols <- intersect(names(td), names(cd))
  duplicate_cols <- setdiff(duplicate_cols, c("user_id", "cluster_id"))
  if (length(duplicate_cols) > 0) {
    td[, (duplicate_cols) := NULL]
  }
  
  # Cartesian join
  merged <- merge(td, cd, by = "user_id", allow.cartesian = TRUE, all.x = TRUE, 
                  suffixes = c("_trip", "_choice"))
  
  # Create choice indicator
  md <- as.data.table(merged)
  md[, choice := as.integer(cluster_id_trip == cluster_id_choice)]
  
  # Remove duplicate cluster_id columns
  md[, cluster_id := cluster_id_choice]
  md[, cluster_id_choice := NULL]
  md[, cluster_id_trip := NULL]
  
  return(md)
}

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------

# Load clustered hotspots (created in step 4)
hotspots_clustered <- read_parquet(inputs$hotspots_clustered)

# Load trip data with distances to hotspots
trip_data <- read_parquet(inputs$ebird_trips_hotspots) %>%
  filter(geo_dist <= params$choice_radius_km)

message("Loaded ", nrow(trip_data), " trips within ", params$choice_radius_km, "km radius")

# -----------------------------------------------------------------------------
# Generate Choice Sets
# -----------------------------------------------------------------------------

# Create choice set
choice_sets <- generate_choice_sets(
  clustered_hotspots = hotspots_clustered,
  trip_df = trip_data,
  radius_km = params$choice_radius_km,
  max_alts = params$max_alternatives
)

# Get users who have choice sets available
users_with_choices <- distinct(choice_sets, user_id)
final_trips <- inner_join(trip_data, users_with_choices, by = 'user_id')

# Add cluster IDs to trips by finding nearest cluster
trip_coords <- st_as_sf(final_trips, coords = c('lon', 'lat'), crs = 4326) %>%
  st_transform(params$projection_crs)

choice_coords <- st_as_sf(choice_sets, coords = c('lon', 'lat'), crs = 4326) %>%
  st_transform(params$projection_crs)

nearest_clusters <- st_nearest_feature(trip_coords, choice_coords)
final_trips$cluster_id <- choice_sets$cluster_id[nearest_clusters]

# -----------------------------------------------------------------------------
# Create Stacked Choice Table
# -----------------------------------------------------------------------------

message("Building stacked choice table...")
master_data_dt <- create_trip_choice_stacks(final_trips, choice_sets)
master_data <- as.data.frame(master_data_dt)

# -----------------------------------------------------------------------------
# Save Results
# -----------------------------------------------------------------------------

write_parquet(master_data, outputs$master_data)
message("Final dataset: ", nrow(master_data), " rows")
message("Unique trips: ", length(unique(master_data$trip_id[!is.na(master_data$trip_id)])))
message("Average choices per trip: ", 
        round(nrow(master_data) / length(unique(master_data$trip_id[!is.na(master_data$trip_id)])), 1))