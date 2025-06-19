# ==============================================================================
# PROJECT: RUM Model - Clean and Reorganized
# PURPOSE: Create hotspot clusters and generate choice sets for counterfactuals
# AUTHOR: Raahil Madhok (reorganized)
# ==============================================================================

# Load required libraries and config
config_path <- file.path("scripts", "R", "0.load_config.R")
source(config_path)


# ==============================================================================
# FUNCTION DEFINITIONS
# ==============================================================================

#' Create Recreation Area Clusters from Hotspots
#' 
#' @param df Dataframe of hotspots with lat, lon, name columns
#' @param module Either 'all' (keep all hotspots) or 'cluster' (create centroids)
#' @param clust_size Cluster size in kilometers
#' @return Dataframe with cluster_id assigned to each hotspot/centroid
create_hotspot_clusters <- function(df, module = 'cluster', clust_size = 10) {
  
  writeLines(paste("Creating hotspot clusters with", clust_size, "km radius..."))
  
  # Load district boundaries
  india_dist <- st_read(file.path("data", "shp", "district-2011", "district-2011.shp"))
  
  # Add district codes to hotspots
  df_sf <- st_as_sf(df, coords = c('lon', 'lat'), crs = 4326)
  df$c_code_2011 <- st_join(df_sf, india_dist, join = st_intersects)$c_code_11
  
  # Remove off-coast hotspots
  df <- filter(df, !is.na(c_code_2011))
  
  # Remove duplicates
  df <- df %>%
    distinct(lat, lon, .keep_all = TRUE) %>%  # Remove duplicate coordinates
    distinct(c_code_2011, name, .keep_all = TRUE)  # Remove duplicate names within district
  
  writeLines(paste("After cleaning:", nrow(df), "unique hotspots"))
  
  # Hierarchical clustering using projected coordinates for accuracy
  writeLines("Computing distance matrix and clustering hotspots...")
  
  # Transform to projected CRS for accurate distance calculations
  india_crs <- 7755 # WGS 84 / India NSF LCC
  df_sf_projected <- st_as_sf(df, coords = c('lon', 'lat'), crs = 4326) %>%
    st_transform(india_crs)
  
  distm <- st_distance(df_sf_projected)
  hc <- hclust(as.dist(distm), method = "complete")
  
  # Assign cluster IDs
  df$cluster_id <- cutree(hc, h = clust_size * 1000)  # Convert km to meters
  
  writeLines(paste("Created", max(df$cluster_id), "clusters"))
  
  if (module == 'all') {
    # Keep all hotspots with cluster assignments
    result <- df
    save_path <- file.path("data", "intermediate", "hotspots", 
                          paste0("hotspots_all_", clust_size, "km.rds"))
    
  } else if (module == 'cluster') {
    # Create cluster centroids
    result <- df %>%
      group_by(cluster_id) %>%
      summarize(
        lat = mean(lat),
        lon = mean(lon),
        .groups = 'drop'
      )
    
    # Add district codes to centroids
    result_sf <- st_as_sf(result, coords = c('lon', 'lat'), crs = 4326)
    result$c_code_2011 <- st_join(result_sf, india_dist, join = st_intersects)$c_code_11
    
    # Remove clusters with off-coast centers
    result <- filter(result, !is.na(c_code_2011))
    
    save_path <- file.path("data", "intermediate", "hotspots", 
                          paste0("hotspots_clust_", clust_size, "km.rds"))
  }
  
  # Save intermediate results
  saveRDS(result, save_path)
  writeLines(paste("Hotspot clusters saved to:", save_path))
  
  return(result)
}

#' Generate Choice Set for Each User
#' 
#' @param choice_df Dataframe of hotspots/clusters to choose from
#' @param trip_df Dataframe of observed trips
#' @param module Either 'cluster' or 'all'
#' @param radius Buffer radius around home in kilometers
#' @param clust_size Cluster size in kilometers (for file naming)
#' @param sample Sample proportion of users
#' @return Dataframe of choice alternatives for each user
generate_choice_sets <- function(choice_df, trip_df, module = 'cluster', 
                                radius = 50, clust_size = 10, sample = 1) {
  
  # Define cache file path
  file_path <- if (module == 'cluster') {
    file.path("data", "intermediate", "choice_sets",
              paste0("choice_set_", radius, "km_clust", clust_size, "km_sample", sample, ".rds"))
  } else {
    file.path("data", "intermediate", "choice_sets",
              paste0("choice_set_", radius, "km.rds"))
  }
  
  # Check if cached version exists
  if (file.exists(file_path)) {
    writeLines(paste("Loading existing choice set from:", file_path))
    return(readRDS(file_path))
  }
  
  writeLines(paste("Generating new choice set with module:", module))
  
  # Process hotspots/clusters
  processed_hotspots <- create_hotspot_clusters(choice_df, module = module, clust_size = clust_size)
  
  # Get unique users within radius
  users_in_radius <- trip_df %>%
    filter(geo_dist <= radius) %>%
    distinct(user_id, .keep_all = TRUE) %>%
    select(user_id, lon_home, lat_home)
  
  writeLines(paste("Found", nrow(users_in_radius), "users within", radius, "km radius"))
  
  # Use appropriate projected CRS for India to get accurate distance calculations
  # UTM Zone 43N (EPSG:32643) covers most of India - adjust if your study area differs
  india_crs <- 32643
  
  writeLines("Finding available choices for each user...")
  
  # Transform to projected coordinates for accurate distance calculations
  user_coords <- st_as_sf(users_in_radius, coords = c('lon_home', 'lat_home'), crs = 4326) %>%
    st_transform(india_crs)
  
  hotspot_coords <- st_as_sf(processed_hotspots, coords = c('lon', 'lat'), crs = 4326) %>%
    st_transform(india_crs)
  
  # Create buffers around user homes (in meters)
  user_buffers <- st_buffer(user_coords, radius * 1000)
  
  # Find hotspots within each user's buffer
  choices_within_buffer <- st_join(user_buffers, hotspot_coords, join = st_intersects) %>%
    filter(!is.na(cluster_id)) %>%
    st_drop_geometry()
  
  # Add back the original coordinates and calculate precise distances
  choice_sets <- choices_within_buffer %>%
    left_join(
      users_in_radius %>% select(user_id, lon_home, lat_home),
      by = "user_id"
    ) %>%
    left_join(
      processed_hotspots %>% select(cluster_id, lat, lon),
      by = "cluster_id"
    ) %>%
    rowwise() %>%
    mutate(
      geo_dist = as.numeric(st_distance(
        st_sfc(st_point(c(lon_home, lat_home)), crs = 4326) %>% st_transform(india_crs),
        st_sfc(st_point(c(lon, lat)), crs = 4326) %>% st_transform(india_crs)
      )) / 1000  # Convert to km
    ) %>%
    ungroup()
  
  # Save results
  saveRDS(choice_sets, file_path)
  writeLines(paste("Choice sets saved to:", file_path))
  
  return(choice_sets)
}

#' Stack Choice Sets for Individual Trips
#' 
#' @param trip_data Dataframe of observed trips
#' @param choice_data Dataframe of choice alternatives
#' @param module Either 'cluster' or 'all'
#' @return List of dataframes, each containing one trip + its counterfactuals
create_trip_choice_stacks <- function(trip_data, choice_data, module = 'cluster') {
  
  writeLines("Creating choice stacks for each trip...")
  
  stack_single_trip <- function(trip_id) {
    # Get the observed trip
    trip_row <- filter(trip_data, trip_id == !!trip_id)
    user <- trip_row$user_id[1]
    
    # Get user's choice set
    user_choices <- filter(choice_data, user_id == user)
    
    # Remove the chosen alternative from counterfactuals
    if (module == "cluster") {
      chosen_cluster <- trip_row$cluster_id[1]
      counterfactuals <- filter(user_choices, cluster_id != chosen_cluster)
    } else {
      trip_lat <- trip_row$lat[1]
      trip_lon <- trip_row$lon[1]
      counterfactuals <- filter(user_choices, lat != trip_lat | lon != trip_lon)
    }
    
    # Handle edge case: no counterfactuals available
    if (nrow(counterfactuals) == 0) {
      counterfactuals <- trip_row[0, ]  # Empty dataframe with same structure
    }
    
    # Standardize data types
    trip_row <- mutate_all(trip_row, as.character)
    counterfactuals <- mutate_all(counterfactuals, as.character)
    
    # Combine observed choice with counterfactuals
    tryCatch({
      bind_rows(trip_row, counterfactuals)
    }, error = function(e) {
      writeLines(paste("Error processing trip_id:", trip_id))
      writeLines(paste("Trip dimensions:", paste(dim(trip_row), collapse = " x ")))
      writeLines(paste("Counterfactual dimensions:", paste(dim(counterfactuals), collapse = " x ")))
      stop(e)
    })
  }
  
  # Process all trips with progress bar
  trip_ids <- trip_data$trip_id
  choice_stacks <- pblapply(trip_ids, stack_single_trip)
  
  return(choice_stacks)
}

# ==============================================================================
# MAIN EXECUTION
# ==============================================================================

# ----------------------------------------------------
# 1. SET PARAMETERS
# ----------------------------------------------------

# Configuration
MODULE <- 'cluster'       # 'cluster' or 'all'
SAMPLE_PROP <- 1          # Proportion of users to sample (0-1)
RADIUS_KM <- 50           # Buffer radius around home
CLUSTER_SIZE_KM <- 10     # Recreation area cluster size

# Print configuration
writeLines("=== RUM MODEL CONFIGURATION ===")
writeLines(paste("Module:", MODULE))
writeLines(paste("Sample Proportion:", SAMPLE_PROP))
writeLines(paste("Radius:", RADIUS_KM, "km"))
writeLines(paste("Cluster Size:", CLUSTER_SIZE_KM, "km"))
writeLines("================================")

# ----------------------------------------------------
# 2. LOAD AND PREPARE DATA
# ----------------------------------------------------

# Load trip data
writeLines(paste("Loading trips from:", config$ebird_trip_hotspots_path))
all_trips <- readRDS(config$ebird_trip_hotspots_path) %>%
  filter(geo_dist <= RADIUS_KM)

# Verify data integrity
stopifnot("Expected 78,593 trips within radius" = nrow(all_trips) == 78593)

# Sample users if needed
set.seed(12345)
if (SAMPLE_PROP < 1) {
  sampled_users <- all_trips %>%
    distinct(user_id) %>%
    sample_frac(SAMPLE_PROP)
  
  sample_trips <- inner_join(all_trips, sampled_users, by = 'user_id')
  writeLines(paste("Sampled", nrow(sample_trips), "trips from", 
                  nrow(sampled_users), "users"))
} else {
  sample_trips <- all_trips
  writeLines(paste("Using all", nrow(sample_trips), "trips"))
}

# Load hotspot data
writeLines(paste("Loading hotspots from:", config$hotspots_path))
hotspot_cols <- c('loc_id', 'country', 'state', 'county', 'lat', 'lon', 'name', 'time', 'v9')
hotspots <- readRDS(config$hotspots_path) %>%
  setNames(hotspot_cols) %>%
  select(lat, lon, name)

# Verify hotspot data
stopifnot("Expected 12,622 hotspots" = nrow(hotspots) == 12622)

# ----------------------------------------------------
# 3. GENERATE CHOICE SETS
# ----------------------------------------------------

choice_sets <- generate_choice_sets(
  choice_df = hotspots,
  trip_df = sample_trips,
  module = MODULE,
  radius = RADIUS_KM,
  clust_size = CLUSTER_SIZE_KM,
  sample = SAMPLE_PROP
)

# Get users who have choice sets available
users_with_choices <- distinct(choice_sets, user_id)
final_trips <- inner_join(sample_trips, users_with_choices, by = 'user_id')

# Add cluster IDs to trips if using cluster module
if (MODULE == 'cluster') {
  trip_coords <- st_as_sf(final_trips, coords = c('lon', 'lat'), crs = 4326) %>%
    st_transform(32643)
  choice_coords <- st_as_sf(choice_sets, coords = c('lon', 'lat'), crs = 4326) %>%
    st_transform(32643)
  
  nearest_clusters <- st_nearest_feature(trip_coords, choice_coords)
  final_trips$cluster_id <- choice_sets$cluster_id[nearest_clusters]
} else {
  choice_sets <- rename(choice_sets, locality = name)
}

# ----------------------------------------------------
# 4. CREATE COUNTERFACTUAL STACKS
# ----------------------------------------------------

choice_stacks <- create_trip_choice_stacks(final_trips, choice_sets, MODULE)

# Convert to single dataframe
writeLines("Combining all choice stacks...")
master_data <- rbindlist(choice_stacks, fill = TRUE) %>%
  as.data.frame()

# ----------------------------------------------------
# 5. CLEAN AND SAVE RESULTS
# ----------------------------------------------------

# Add choice indicator and clean columns
master_clean <- master_data %>%
  mutate(choice = ifelse(is.na(trip_id), 0, 1)) %>%
  select(-starts_with('lat'), -starts_with('lon'), -duration, -distance, -complete) %>%
  fill(trip_id, date, year, month, yearmonth, c_code_2011_home, .direction = "down")

# Save final dataset
output_filename <- paste0("master_cs", RADIUS_KM, "km_clust", CLUSTER_SIZE_KM, 
                         "km_sample", SAMPLE_PROP, ".rds")
output_path <- do.call(file.path, c(as.list(config$clean_data_dir), output_filename))
saveRDS(master_clean, output_path)

# Print summary
writeLines("=== PROCESSING COMPLETE ===")
writeLines(paste("Final dataset:", nrow(master_clean), "rows"))
writeLines(paste("Unique trips:", length(unique(master_clean$trip_id[!is.na(master_clean$trip_id)]))))
writeLines(paste("Average choices per trip:", 
                round(nrow(master_clean) / length(unique(master_clean$trip_id[!is.na(master_clean$trip_id)])), 1)))
writeLines(paste("File saved to:", output_path))
writeLines("===========================")
