# PROJECT: RUM Model
# PURPOSE: Construct data on site attributes (pull factors)
####################################
# Sections:
# 1. Rainfall
# 2. Temperature
# 3. Tree cover
# 4. Species richness
# 5. Save updated hotspots with attributes
####################################


# ----------------------------------------------------
# Preliminary Setup
# ----------------------------------------------------
# Load config
config_path <- file.path("scripts", "R", "0.load_config.R")
source(config_path)

# Bring in hotspot cluster data (e.g., the sites we want to add attributes to)
hotspots <- readRDS("data/intermediate/hotspots/hotspots_clust_10km.rds")

# Straight-line dist from home to hotspot
cat("Generating Hotspot Buffers...\n")
# Convert to sf and make buffer
india_crs <- 7755 # WGS 84 / India NSF LCC
hotspots_buffer <- st_as_sf(hotspots, coords = c('lon', 'lat'), crs = 4326) %>% 
  st_transform(crs = india_crs) %>%
  st_buffer(dist = 10000)

# Transform back to 4326 to match rasters
hotspots_buffer_WGS84 <- st_transform(hotspots_buffer, crs = 4326)

# Save for debugging
# Save all three to a single GeoPackage with different layer names
gpkg_path <- "data/intermediate/hotspots/hotspots.gpkg"
hotspots_sf <- st_as_sf(hotspots, coords = c('lon', 'lat'), crs = 4326)
st_write(hotspots_sf, gpkg_path, layer = "hotspots", delete_layer = TRUE)
st_write(hotspots_buffer, gpkg_path, layer = "hotspots_buffer", delete_layer = TRUE)
st_write(hotspots_buffer_WGS84, gpkg_path, layer = "hotspots_buffer_WGS84", delete_layer = TRUE)



# ----------------------------------------------------
# 1. Extract Zonal Statistics: Precipitation, Temperature, Tree Cover
# ----------------------------------------------------
# Define function to extract zonal stats from dir of raster files
extract_zonal_stats <- function(
  variable_name,                # Name of variable (e.g., "precip", "temp", "trees")
  dir_path,                     # Directory containing raster files
  hotspots_data,                # Hotspots dataframe
  hotspots_buffer,              # Buffer polygons for extraction
  output_dir,                   # Output directory for results
  date_pattern,                 # Regex pattern to extract date from filename
  date_replacement,             # Replacement pattern for date extraction
  file_pattern = "\\.tif$",     # Pattern to match raster files
  exclude_pattern = NULL,       # Pattern to exclude files (e.g., "SD" for standard deviation)
  time_frequency = "monthly",   # "monthly", "annual", or custom
  extraction_fun = mean,        # Function for zonal statistics (mean, sum, etc.)
  na.rm = TRUE,                 # Remove NA values in extraction
  force_recompute = FALSE       # Force recomputation even if output exists
) {
  
  # Validate inputs
  if (!dir.exists(dir_path)) {
    stop("Directory path does not exist: ", dir_path)
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
  }
  
  # Create output filename
  output_filename <- paste0("hotspots_cluster_", time_frequency, "_", variable_name, ".rds")
  output_path <- file.path(output_dir, output_filename)
  
  # Check if output already exists
  if (file.exists(output_path) && !force_recompute) {
    cat("Output file already exists, loading existing data for", variable_name, "\n")
    return(readRDS(output_path))
  }
  
  # Get list of raster files
  raster_files <- list.files(dir_path, pattern = file_pattern, full.names = TRUE)
  
  # Exclude files if pattern provided
  if (!is.null(exclude_pattern)) {
    raster_files <- raster_files[!grepl(exclude_pattern, raster_files)]
  }
  
  # Sort files
  raster_files <- sort(raster_files)
  
  if (length(raster_files) == 0) {
    stop("No raster files found matching pattern in: ", dir_path)
  }
  
  cat("Running Zonal Stats for", variable_name, "...\n")
  cat("Processing", length(raster_files), "files\n")
  
  # Initialize storage
  variable_stats <- list()
  start_time <- Sys.time()
  
  # Loop through raster files
  for (i in seq_along(raster_files)) {
    f <- raster_files[i]
    
    # Progress indicator (every 25%)
    progress_points <- c(0.25, 0.5, 0.75, 1.0) * length(raster_files)
    if (i %in% ceiling(progress_points)) {
      percent <- round(100 * i / length(raster_files))
      cat("Processing file", i, "of", length(raster_files), "(", percent, "%)\n")
    }
    
    tryCatch({
      # Load raster
      r <- terra::rast(f)
      
      # Extract date from filename
      date_label <- sub(date_pattern, date_replacement, basename(f))
      
      # Validate date extraction
      if (date_label == basename(f)) {
        warning("Date pattern did not match for file: ", basename(f))
        next
      }
      
      # Extract zonal statistics
      stats <- terra::extract(r, terra::vect(hotspots_buffer_WGS84), 
                              fun = extraction_fun, na.rm = na.rm, touches = TRUE)
    # DEBUG 
    na_count <- sum(is.na(stats[[2]]))
    total_count <- length(stats[[2]])
    if (na_count > 0) {
      cat("File:", basename(f), "- NA values:", na_count, "/", total_count, "\n")
    }
      
      # Create results dataframe
      stats_df <- hotspots_data %>%
        dplyr::mutate(
          year_month = date_label,
          !!variable_name := stats[[2]]  # Use dynamic column naming
        )
      
      # Store results
      variable_stats[[date_label]] <- stats_df
      
    }, error = function(e) {
      warning("Error processing file ", basename(f), ": ", e$message)
    })
  }
  
  # Combine all results
  if (length(variable_stats) == 0) {
    stop("No data was successfully extracted")
  }
  
  result_data <- dplyr::bind_rows(variable_stats)
  
  # Final quality control summary
  total_observations <- nrow(result_data)
  missing_observations <- sum(is.na(result_data[[variable_name]]))
  missing_percentage <- round(100 * missing_observations / total_observations, 2)
  
  cat("\n=== EXTRACTION SUMMARY ===\n")
  cat("Variable:", variable_name, "\n")
  cat("Total observations:", total_observations, "\n")
  cat("Missing values:", missing_observations, "(", missing_percentage, "%)\n")
  
  # Save results
  saveRDS(result_data, file = output_path)
  
  # Print summary
  end_time <- Sys.time()
  cat("Completed zonal stats for", variable_name, "\n")
  cat("Processing time:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")
  cat("Output saved to:", output_path, "\n")
  cat("Total records:", nrow(result_data), "\n")
  
  return(result_data)
}

# i) Precipitation
hotspots_precip <- extract_zonal_stats(
  variable_name = "precip",
  dir_path = config$precip_basic_path,
  hotspots_data = hotspots,
  hotspots_buffer = hotspots_buffer,
  output_dir = file.path("data", "intermediate", "hotspots"),
  date_pattern = ".*_(\\d{4})_(\\d{2})\\.tif$",
  date_replacement = "\\1-\\2",
  time_frequency = "monthly"
)

# ii) Temperature
hotspots_temp <- extract_zonal_stats(
  variable_name = "temp",
  dir_path = config$temp_basic_path,
  hotspots_data = hotspots,
  hotspots_buffer = hotspots_buffer,
  output_dir = file.path("data", "intermediate", "hotspots"),
  date_pattern = ".*_(\\d{4})_(\\d{2})\\.tif$",
  date_replacement = "\\1-\\2",
  time_frequency = "monthly"
)

# iii) Tree cover
hotspots_trees <- extract_zonal_stats(
  variable_name = "trees",
  dir_path = config$trees_basic_path,
  hotspots_data = hotspots,
  hotspots_buffer = hotspots_buffer,
  output_dir = file.path("data", "intermediate", "hotspots"),
  file_pattern = "Percent_Tree_Cover.*\\.tif$",
  exclude_pattern = "SD",
  date_pattern = ".*doy(\\d{4}).*$",
  date_replacement = "\\1",
  time_frequency = "annual"
)



# ----------------------------------------------------
# 2. Species Richness and Biodiversity
# ----------------------------------------------------

# Define output file paths
biodiversity_output_dir <- file.path("data", "intermediate", "hotspots")
biodiversity_files <- c(
  "hotspots_monthly_biodiversity.rds",
  "hotspots_weekly_biodiversity.rds", 
  "home_monthly_biodiversity.rds",
  "home_weekly_biodiversity.rds",
  "hotspots_monthly_congestion.rds",
  "hotspots_weekly_congestion.rds"
)

# Create full paths
biodiversity_paths <- file.path(biodiversity_output_dir, biodiversity_files)
names(biodiversity_paths) <- c("monthly_bio", "weekly_bio", "home_monthly_bio", 
                              "home_weekly_bio", "monthly_cong", "weekly_cong")

# Check if all biodiversity files exist
all_files_exist <- all(file.exists(biodiversity_paths))

if (all_files_exist) {
  cat("All biodiversity output files already exist, loading existing data...\n")
  
  # Load existing files
  monthly_complete <- readRDS(biodiversity_paths["monthly_bio"])
  weekly_complete <- readRDS(biodiversity_paths["weekly_bio"])
  home_monthly_complete <- readRDS(biodiversity_paths["home_monthly_bio"])
  home_weekly_complete <- readRDS(biodiversity_paths["home_weekly_bio"])
  
  # Load congestion data into species_data list structure
  species_data <- list(
    monthly_congestion = readRDS(biodiversity_paths["monthly_cong"]),
    weekly_congestion = readRDS(biodiversity_paths["weekly_cong"])
  )
  
  cat("Loaded existing biodiversity and congestion data.\n")
  cat("Monthly biodiversity records:", nrow(monthly_complete), "\n")
  cat("Weekly biodiversity records:", nrow(weekly_complete), "\n")
  cat("Monthly congestion records:", nrow(species_data$monthly_congestion), "\n")
  cat("Weekly congestion records:", nrow(species_data$weekly_congestion), "\n")
  
} else {
  cat("Some biodiversity files missing. Computing biodiversity metrics...\n")
  
  # TO-DO: Update input birdlife data path
  cat("Loading and processing species data...\n")

  # ----------------------------------------------------
  # Function to load BirdLife species range data (for fallback richness)
  # ----------------------------------------------------
  load_birdlife_richness <- function(hotspots_buffer) {
    cat("Processing BirdLife species ranges...\n")
    
    # Create bounding box for efficient reading
    bbox_wkt <- hotspots_buffer_WGS84 %>%
      st_bbox() %>%
      st_as_sfc() %>%
      st_as_text()
    
    # Load species ranges
    species_path <- '/Users/mbraaksma/Files/base_data/biodiversity-wtp/species/BOTW_2024_2.gpkg'
    species_ranges <- st_read(
      dsn = species_path,
      layer = "all_species",
      wkt_filter = bbox_wkt
    ) %>%
      select(sci_name, presence, geom) %>%
      filter(presence == 1) %>%
      filter(st_geometry_type(.) == "MULTIPOLYGON") %>%
      st_transform(crs = india_crs)
    
    # Calculate species richness per hotspot
    sf::sf_use_s2(FALSE) # Disable spherical geometry
    
    hotspot_intersections <- st_intersects(species_ranges, hotspots_buffer)
    hotspot_species_counts <- integer(nrow(hotspots_buffer))
    
    for(i in seq_along(hotspot_intersections)) {
      hotspot_ids <- hotspot_intersections[[i]]
      if(length(hotspot_ids) > 0) {
        hotspot_species_counts[hotspot_ids] <- hotspot_species_counts[hotspot_ids] + 1
      }
    }
    
    return(hotspot_species_counts)
  }

  # ----------------------------------------------------
  # Function to process eBird observation data
  # ----------------------------------------------------

  process_ebird_data <- function(config, hotspots_mapping) {
    cat("Loading eBird data...\n")
    
    # Load eBird data
    ebird <- fread(
      config$ebird_basic_path,
      select = c('LATITUDE', 'LONGITUDE', 'OBSERVATION DATE',
                 'OBSERVER ID', 'SAMPLING EVENT IDENTIFIER',
                 'PROTOCOL TYPE', 'DURATION MINUTES',
                 'EFFORT DISTANCE KM', 'ALL SPECIES REPORTED',
                 'LOCALITY', 'LOCALITY TYPE', 'COMMON NAME', 'CATEGORY'),
      quote = ""
    )
    
    # Clean column names
    colnames(ebird) <- gsub('\\.', '_', tolower(make.names(colnames(ebird))))
    
    # ─────────────────────────────────────────────────────
    # Hotspot-based biodiversity
    # ─────────────────────────────────────────────────────
    species_hotspot <- ebird %>%
      filter(locality_type == 'H', category == "species") %>%
      select(lat = latitude, lon = longitude, observation_date, common_name) %>%
      mutate(
        observation_date = as.Date(observation_date),
        year_month = format(observation_date, "%Y-%m"),
        year_week = format(observation_date, "%Y-%U")
      ) %>%
      count(lat, lon, year_month, year_week, common_name, name = "n")
    
    species_sf <- st_as_sf(species_hotspot, coords = c('lon', 'lat'), crs = 4326) %>%
      st_transform(crs = india_crs)
    
    hotspots_sf <- st_as_sf(hotspots_mapping, coords = c('lon', 'lat'), crs = 4326) %>%
      st_transform(crs = india_crs)
    
    idx <- st_nearest_feature(species_sf, hotspots_sf)
    species_hotspot$cluster_id <- hotspots_sf$cluster_id[idx]
    
    # ─────────────────────────────────────────────────────
    # Home-based biodiversity
    # ─────────────────────────────────────────────────────
    home_coords <- ebird %>%
      filter(str_detect(str_to_lower(locality),
                        'my home|my house|my balcony|my terrace|my backyard|my street|my yard|my veranda')) %>%
      group_by(user_id = observer_id) %>%
      summarize(
        lon_home = mean(longitude, na.rm = TRUE),
        lat_home = mean(latitude, na.rm = TRUE),
        .groups = "drop"
      )

    ebird_home_obs <- ebird %>%
      inner_join(home_coords, by = c("observer_id" = "user_id")) %>%
      filter(category == "species", locality_type == "H") %>%
      select(lat = latitude, lon = longitude, observation_date, common_name,
             lon_home, lat_home, user_id = observer_id) %>%  # Keep home coordinates
      mutate(
        observation_date = as.Date(observation_date),
        year_month = format(observation_date, "%Y-%m"),
        year_week = format(observation_date, "%Y-%U")
      ) %>%
      count(user_id, lat_home, lon_home, year_month, year_week, common_name, name = "n") %>%
      rename(cluster_id = user_id)  # to reuse biodiversity metric functions

    # ─────────────────────────────────────────────────────
    # Hotspot-based congestion (trips and users)
    # ─────────────────────────────────────────────────────
    congestion <- ebird %>%
      filter(locality_type == 'H') %>%
      mutate(
        observation_date = as.Date(observation_date),
        year_month = format(observation_date, "%Y-%m"),
        year_week = format(observation_date, "%Y-%U")
      ) %>%
      select(lat = latitude, lon = longitude, year_month, year_week,
             observer_id, sampling_event_identifier) %>%
      distinct()  # One row per observation event
    
    # Assign to nearest hotspot cluster
    congestion_sf <- st_as_sf(congestion, coords = c('lon', 'lat'), crs = 4326) %>%
      st_transform(crs = india_crs)
    
    idx_cong <- st_nearest_feature(congestion_sf, hotspots_sf)
    congestion$cluster_id <- hotspots_sf$cluster_id[idx_cong]
    
    # Aggregate by cluster and period
    monthly_congestion <- congestion %>%
      group_by(cluster_id, year_month) %>%
      summarise(
        n_users = n_distinct(observer_id),
        n_trips = n_distinct(sampling_event_identifier),
        .groups = "drop"
      )
    weekly_congestion <- congestion %>%
      group_by(cluster_id, year_week) %>%
      summarise(
        n_users = n_distinct(observer_id),
        n_trips = n_distinct(sampling_event_identifier),
        .groups = "drop"
      )

    # Get all unique clusters
    all_clusters <- unique(hotspots_sf$cluster_id)
    
    # Create complete time series
    all_months <- seq.Date(
      from = as.Date("2010-01-01"),
      to = Sys.Date(),
      by = "month"
    ) %>% format("%Y-%m")
    all_weeks <- seq.Date(
      from = as.Date("2010-01-01"),
      to = Sys.Date(),
      by = "week"
    ) %>% format("%Y-%U")
    
    # Full grids
    monthly_grid <- expand_grid(
      cluster_id = all_clusters,
      year_month = all_months
    )
    weekly_grid <- expand_grid(
      cluster_id = all_clusters,
      year_week = all_weeks
    )
    
    # Join congestion counts to full grid
    monthly_congestion <- monthly_grid %>%
      left_join(monthly_congestion, by = c("cluster_id", "year_month")) %>%
      mutate(
        n_users = replace_na(n_users, 0),
        n_trips = replace_na(n_trips, 0)
      )
    weekly_congestion <- weekly_grid %>%
      left_join(weekly_congestion, by = c("cluster_id", "year_week")) %>%
      mutate(
        n_users = replace_na(n_users, 0),
        n_trips = replace_na(n_trips, 0)
      )
    
    return(list(
      hotspot_species = species_hotspot,
      home_species = ebird_home_obs,
      monthly_congestion = monthly_congestion,
      weekly_congestion = weekly_congestion
    ))
  }

  # ----------------------------------------------------
  # Function to calculate biodiversity metrics
  # ----------------------------------------------------

  calculate_biodiversity_metrics <- function(species_data, time_unit = "year_month", preserve_home_coords = FALSE) {
    cat(sprintf("Calculating %s biodiversity metrics...\n", 
                ifelse(time_unit == "year_month", "MONTHLY", "WEEKLY")))
    
    # Check if this is home data (has lat_home, lon_home columns)
    has_home_coords <- all(c("lat_home", "lon_home") %in% colnames(species_data))
    
    if (has_home_coords && preserve_home_coords) {
      # For home data, preserve coordinates during aggregation
      species_cluster <- species_data %>%
        group_by(cluster_id, common_name, !!sym(time_unit), lat_home, lon_home) %>%
        summarise(total_n = sum(n), .groups = "drop")
      
      # Create species abundance matrix
      species_wide <- species_cluster %>%
        pivot_wider(
          names_from = common_name,
          values_from = total_n,
          values_fill = 0
        )
      
      # Extract species matrix (excluding ID and coordinate columns)
      id_cols <- c("cluster_id", time_unit, "lat_home", "lon_home")
      species_matrix <- species_wide %>%
        select(-all_of(id_cols)) %>%
        as.matrix()
      
      # Calculate diversity metrics
      biodiversity_metrics <- species_wide %>%
        select(all_of(id_cols)) %>%
        mutate(
          observed_richness = rowSums(species_matrix > 0),
          shannon = vegan::diversity(species_matrix, index = "shannon"),
          simpson = vegan::diversity(species_matrix, index = "simpson"),
          total_abundance = rowSums(species_matrix)
        )
      
    } else {
      # Regular aggregation for hotspot data
      species_cluster <- species_data %>%
        group_by(cluster_id, common_name, !!sym(time_unit)) %>%
        summarise(total_n = sum(n), .groups = "drop")
      
      # Create species abundance matrix
      species_wide <- species_cluster %>%
        pivot_wider(
          names_from = common_name,
          values_from = total_n,
          values_fill = 0
        )
      
      # Extract species matrix (excluding ID columns)
      id_cols <- c("cluster_id", time_unit)
      species_matrix <- species_wide %>%
        select(-all_of(id_cols)) %>%
        as.matrix()
      
      # Calculate diversity metrics
      biodiversity_metrics <- species_wide %>%
        select(all_of(id_cols)) %>%
        mutate(
          observed_richness = rowSums(species_matrix > 0),
          shannon = vegan::diversity(species_matrix, index = "shannon"),
          simpson = vegan::diversity(species_matrix, index = "simpson"),
          total_abundance = rowSums(species_matrix)
        )
    }
    
    return(biodiversity_metrics)
  }

  # ----------------------------------------------------
  # Function to fill missing periods with BirdLife richness
  # ----------------------------------------------------

  fill_missing_periods <- function(biodiversity_data, birdlife_richness, 
                                  hotspots_buffer, time_unit = "year_month", 
                                  is_home_data = FALSE) {
    cat("Filling missing time periods with BirdLife richness estimates...\n")
    
    # Create complete time series grid
    if (is_home_data) {
      # For home data, use unique user IDs (cluster_id represents user_id in home data)
      all_clusters <- as.character(unique(biodiversity_data$cluster_id))
    } else {
      all_clusters <- as.character(unique(hotspots_buffer$cluster_id))
    }
    
    if (time_unit == "year_month") {
      all_periods <- seq.Date(
        from = as.Date("2010-01-01"), 
        to = Sys.Date(), 
        by = "month"
      ) %>% format("%Y-%m")
    } else if (time_unit == "year_week") {
      all_periods <- seq.Date(
        from = as.Date("2010-01-01"), 
        to = Sys.Date(), 
        by = "week"
      ) %>% format("%Y-%U")
    }
    
    if (is_home_data) {
      # For home data, preserve home coordinates
      # Get unique user-coordinate combinations
      user_coords <- biodiversity_data %>%
        select(cluster_id, lat_home, lon_home) %>%
        distinct()
      
      complete_grid <- expand_grid(
        cluster_id = all_clusters,
        !!time_unit := all_periods
      ) %>%
        left_join(user_coords, by = "cluster_id")
      
      # For home data, use a default richness value since BirdLife is location-specific
      # You might want to use median observed richness or another approach
      default_richness <- median(biodiversity_data$observed_richness, na.rm = TRUE)
      
      complete_data <- complete_grid %>%
        left_join(biodiversity_data, by = c("cluster_id", time_unit, "lat_home", "lon_home")) %>%
        mutate(
          final_richness = coalesce(observed_richness, default_richness),
          data_source = ifelse(is.na(observed_richness), "default", "ebird")
        )
      
    } else {
      # Regular hotspot processing
      complete_grid <- expand_grid(
        cluster_id = all_clusters,
        !!time_unit := all_periods
      )
      
      # Merge BirdLife richness with hotspots
      birdlife_df <- hotspots_buffer %>%
        st_drop_geometry() %>%
        select(cluster_id) %>%
        mutate(
          cluster_id = as.character(cluster_id),
          birdlife_richness = birdlife_richness
        )
      
      # Ensure `biodiversity_data` cluster_id is also character
      biodiversity_data <- biodiversity_data %>%
        mutate(cluster_id = as.character(cluster_id))
      
      # Fill missing periods
      complete_data <- complete_grid %>%
        left_join(biodiversity_data, by = c("cluster_id", time_unit)) %>%
        left_join(birdlife_df, by = "cluster_id") %>%
        mutate(
          final_richness = coalesce(observed_richness, birdlife_richness),
          data_source = ifelse(is.na(observed_richness), "birdlife", "ebird")
        )
    }
    
    return(complete_data)
  }

  # ----------------------------------------------------
  # Main execution of Species Richness and Biodiversity
  # ----------------------------------------------------

  # Load BirdLife richness data
  birdlife_richness <- load_birdlife_richness(hotspots_buffer)

  # Process eBird data
  hotspots_mapping <- readRDS("./data/intermediate/hotspots/hotspots_clust_10km.rds")
  species_data <- process_ebird_data(config, hotspots_mapping)

  # Calculate biodiversity metrics
  monthly_biodiversity <- calculate_biodiversity_metrics(species_data$hotspot_species, "year_month", preserve_home_coords = FALSE)
  weekly_biodiversity <- calculate_biodiversity_metrics(species_data$hotspot_species, "year_week", preserve_home_coords = FALSE)
  home_monthly_biodiversity <- calculate_biodiversity_metrics(species_data$home_species, "year_month", preserve_home_coords = TRUE)
  home_weekly_biodiversity <- calculate_biodiversity_metrics(species_data$home_species, "year_week", preserve_home_coords = TRUE)
  
  # Fill missing periods with BirdLife richness
  monthly_complete <- fill_missing_periods(
    monthly_biodiversity, birdlife_richness, hotspots_buffer, "year_month", is_home_data = FALSE
  )
  weekly_complete <- fill_missing_periods(
    weekly_biodiversity, birdlife_richness, hotspots_buffer, "year_week", is_home_data = FALSE
  )
  home_monthly_complete <- fill_missing_periods(
    home_monthly_biodiversity, birdlife_richness, hotspots_buffer, "year_month", is_home_data = TRUE
  )
  home_weekly_complete <- fill_missing_periods(
    home_weekly_biodiversity, birdlife_richness, hotspots_buffer, "year_week", is_home_data = TRUE
  )

  # Create output directory if it doesn't exist
  if (!dir.exists(biodiversity_output_dir)) {
    dir.create(biodiversity_output_dir, recursive = TRUE)
    cat("Created output directory:", biodiversity_output_dir, "\n")
  }

  # Save results
  # Hotspots biodiversity
  saveRDS(monthly_complete, biodiversity_paths["monthly_bio"])
  saveRDS(weekly_complete, biodiversity_paths["weekly_bio"])
  # Home biodiversity
  saveRDS(home_monthly_complete, biodiversity_paths["home_monthly_bio"])
  saveRDS(home_weekly_complete, biodiversity_paths["home_weekly_bio"])
  # Hotspots congestion
  saveRDS(species_data$monthly_congestion, biodiversity_paths["monthly_cong"])
  saveRDS(species_data$weekly_congestion, biodiversity_paths["weekly_cong"])

  cat("Biodiversity analysis complete! Files saved.\n")
}

cat("Species richness and biodiversity processing complete.\n")


# ----------------------------------------------------
# 3. Merge Site Attributes to Choice Set (by Trip Date)
# ----------------------------------------------------

cat("Merging site attributes to choice set by trip date...\n")

# Load choice set 
choice_set_path <- file.path("data", "clean", "master_cs50km_clust10km_sample1.rds")
choice_set <- readRDS(choice_set_path)

# Check the structure of choice set to understand date variables
cat("Choice set dimensions:", dim(choice_set), "\n")
cat("Choice set columns:", colnames(choice_set), "\n")

# Function to merge attributes with different temporal granularities to daily trip data
merge_daily_trip_attributes <- function(choice_set) {
  
  # Ensure choice set has date variable
  date_vars <- intersect(c("date", "trip_date", "observation_date"), colnames(choice_set))
  if(length(date_vars) == 0) {
    stop("No date variable found in choice set. Need 'date', 'trip_date', or 'observation_date'")
  }
  
  date_var <- date_vars[1]  # Use first available date variable
  cat("Using date variable:", date_var, "\n")
  
  # Convert cluster_id to character and ensure date is Date class
  merged_data <- choice_set %>%
    mutate(
      cluster_id = as.character(cluster_id),
      !!date_var := as.Date(!!sym(date_var)),
      # Create time matching variables
      year_month = format(!!sym(date_var), "%Y-%m"),
      year_week = format(!!sym(date_var), "%Y-%U"), 
      year = as.numeric(format(!!sym(date_var), "%Y"))
    )
  
  # ADD HOTSPOT COORDINATES - merge cluster coordinates from the hotspots data
  cat("Adding hotspot cluster coordinates...\n")
  hotspot_coords <- hotspots %>%
    select(cluster_id, lat, lon) %>%
    rename(lat_cluster = lat, lon_cluster = lon) %>%
    mutate(cluster_id = as.character(cluster_id))
  
  merged_data <- merged_data %>%
    left_join(hotspot_coords, by = "cluster_id")
  
  cat("Created time matching variables: year_month, year_week, year\n")
  
  # Debug choice set structure
  cat("\nChoice set structure after time variable creation:\n")
  cat("- Total rows:", nrow(merged_data), "\n")
  cat("- Unique clusters:", length(unique(merged_data$cluster_id)), "\n")
  cat("- Sample cluster_ids:", paste(head(unique(merged_data$cluster_id), 5), collapse = ", "), "\n")
  cat("- Date range:", paste(range(merged_data[[date_var]], na.rm = TRUE), collapse = " to "), "\n")
  cat("- Year-month range:", paste(range(merged_data$year_month, na.rm = TRUE), collapse = " to "), "\n")
  cat("- Year range:", paste(range(merged_data$year, na.rm = TRUE), collapse = " to "), "\n")
  cat("- Sample year_months:", paste(head(unique(merged_data$year_month), 5), collapse = ", "), "\n")
  
  # 1. MONTHLY BIODIVERSITY - Match by year_month
  cat("Merging monthly biodiversity data...\n")
  bio_monthly <- monthly_complete %>%
    select(cluster_id, year_month, observed_richness, shannon, simpson, 
           total_abundance, final_richness, data_source) %>%
    rename_with(~paste0("bio_", .), -c(cluster_id, year_month)) %>%
    mutate(cluster_id = as.character(cluster_id))
  
  merged_data <- merged_data %>%
    left_join(bio_monthly, by = c("cluster_id", "year_month"))
  
  # 2. WEEKLY BIODIVERSITY - Match by year_week (optional - for comparison)
  cat("Merging weekly biodiversity data...\n")
  bio_weekly <- weekly_complete %>%
    select(cluster_id, year_week, observed_richness, shannon, simpson, 
           total_abundance, final_richness, data_source) %>%
    rename_with(~paste0("bio_week_", .), -c(cluster_id, year_week)) %>%
    mutate(cluster_id = as.character(cluster_id))
  
  merged_data <- merged_data %>%
    left_join(bio_weekly, by = c("cluster_id", "year_week"))
  
  # 3. MONTHLY CONGESTION - Match by year_month
  cat("Merging monthly congestion data...\n")
  cong_monthly <- species_data$monthly_congestion %>%
    select(cluster_id, year_month, n_users, n_trips) %>%
    rename_with(~paste0("cong_", .), -c(cluster_id, year_month)) %>%
    mutate(cluster_id = as.character(cluster_id))
  
  merged_data <- merged_data %>%
    left_join(cong_monthly, by = c("cluster_id", "year_month"))
  
  # 4. WEEKLY CONGESTION - Match by year_week (optional)
  cat("Merging weekly congestion data...\n")
  cong_weekly <- species_data$weekly_congestion %>%
    select(cluster_id, year_week, n_users, n_trips) %>%
    rename_with(~paste0("cong_week_", .), -c(cluster_id, year_week)) %>%
    mutate(cluster_id = as.character(cluster_id))
  
  merged_data <- merged_data %>%
    left_join(cong_weekly, by = c("cluster_id", "year_week"))
  
  # 5. MONTHLY PRECIPITATION - Match by year_month
  cat("Merging monthly precipitation data...\n")
  if(exists("hotspots_precip") && nrow(hotspots_precip) > 0) {
    # Debug precipitation data structure
    cat("Precipitation data structure:\n")
    cat("- Rows:", nrow(hotspots_precip), "\n")
    cat("- Columns:", paste(colnames(hotspots_precip), collapse = ", "), "\n")
    cat("- Sample cluster_ids:", paste(head(unique(hotspots_precip$cluster_id), 5), collapse = ", "), "\n")
    cat("- Sample year_months:", paste(head(unique(hotspots_precip$year_month), 5), collapse = ", "), "\n")
    cat("- Date range:", paste(range(hotspots_precip$year_month, na.rm = TRUE), collapse = " to "), "\n")
    
    precip_clean <- hotspots_precip %>%
      select(cluster_id, year_month, precip) %>%
      mutate(cluster_id = as.character(cluster_id))
    
    # Check for join mismatches
    choice_clusters <- unique(merged_data$cluster_id)
    choice_months <- unique(merged_data$year_month)
    precip_clusters <- unique(precip_clean$cluster_id)
    precip_months <- unique(precip_clean$year_month)
    
    cat("- Choice set unique clusters:", length(choice_clusters), "\n")
    cat("- Precip data unique clusters:", length(precip_clusters), "\n")
    cat("- Clusters in both:", length(intersect(choice_clusters, precip_clusters)), "\n")
    cat("- Choice set unique months:", length(choice_months), "\n")
    cat("- Precip data unique months:", length(precip_months), "\n")
    cat("- Months in both:", length(intersect(choice_months, precip_months)), "\n")
    
    merged_data <- merged_data %>%
      left_join(precip_clean, by = c("cluster_id", "year_month"))
  }
  
  # 6. MONTHLY TEMPERATURE - Match by year_month
  cat("Merging monthly temperature data...\n")
  if(exists("hotspots_temp") && nrow(hotspots_temp) > 0) {
    # Debug temperature data structure
    cat("Temperature data structure:\n")
    cat("- Rows:", nrow(hotspots_temp), "\n")
    cat("- Columns:", paste(colnames(hotspots_temp), collapse = ", "), "\n")
    cat("- Date range:", paste(range(hotspots_temp$year_month, na.rm = TRUE), collapse = " to "), "\n")
    
    temp_clean <- hotspots_temp %>%
      select(cluster_id, year_month, temp) %>%
      mutate(cluster_id = as.character(cluster_id))
    
    merged_data <- merged_data %>%
      left_join(temp_clean, by = c("cluster_id", "year_month"))
  }
  
  # 7. ANNUAL TREE COVER - Match by year
  cat("Merging annual tree cover data...\n")
  if(exists("hotspots_trees") && nrow(hotspots_trees) > 0) {
    # Debug trees data structure
    cat("Trees data structure:\n")
    cat("- Rows:", nrow(hotspots_trees), "\n")
    cat("- Columns:", paste(colnames(hotspots_trees), collapse = ", "), "\n")
    cat("- Sample year_month values:", paste(head(unique(hotspots_trees$year_month), 5), collapse = ", "), "\n")
    
    # Check if year_month in trees data is actually year or year-month format
    sample_years <- head(hotspots_trees$year_month, 10)
    cat("- Sample year_month formats:", paste(sample_years, collapse = ", "), "\n")
    
    trees_clean <- hotspots_trees %>%
      select(cluster_id, year_month, trees) %>%
      mutate(
        cluster_id = as.character(cluster_id),
        # Handle different year formats
        year = if(all(nchar(as.character(year_month)) == 4, na.rm = TRUE)) {
          as.numeric(year_month)  # Already year format
        } else {
          as.numeric(substr(as.character(year_month), 1, 4))  # Extract year from year-month
        }
      ) %>%
      select(cluster_id, year, trees)
    
    cat("- Trees years available:", paste(sort(unique(trees_clean$year)), collapse = ", "), "\n")
    cat("- Choice set years:", paste(sort(unique(merged_data$year)), collapse = ", "), "\n")
    cat("- Years in both:", length(intersect(unique(trees_clean$year), unique(merged_data$year))), "\n")
    
    merged_data <- merged_data %>%
      left_join(trees_clean, by = c("cluster_id", "year"))
  }
  
  # Clean up temporary matching variables (optional - keep for debugging)
  # merged_data <- merged_data %>%
  #   select(-year_month, -year_week, -year)
  
  return(merged_data)
}

# Function to add HOME-based biodiversity attributes
merge_home_attributes <- function(merged_data) {
  cat("Merging home-based biodiversity data...\n")
  
  # Check if choice set has user/observer identifier
  user_vars <- intersect(c("user_id", "observer_id", "birder_id"), colnames(merged_data))
  if(length(user_vars) == 0) {
    cat("No user identifier found - skipping home biodiversity merge\n")
    return(merged_data)
  }
  
  user_var <- user_vars[1]
  cat("Using user variable:", user_var, "\n")
  
  # Monthly home biodiversity - KEEP HOME COORDINATES
  home_bio_monthly <- home_monthly_complete %>%
    select(cluster_id, year_month, observed_richness, shannon, simpson, 
           total_abundance, final_richness, data_source, 
           lat_home, lon_home) %>%  # ADD THESE COORDINATES
    rename_with(~paste0("home_bio_", .), -c(cluster_id, year_month, lat_home, lon_home)) %>%
    rename(!!user_var := cluster_id)  # cluster_id in home data is actually user_id
  
  merged_data <- merged_data %>%
    left_join(home_bio_monthly, by = c(user_var, "year_month"))
  
  # Weekly home biodiversity - KEEP HOME COORDINATES  
  home_bio_weekly <- home_weekly_complete %>%
    select(cluster_id, year_week, observed_richness, shannon, simpson, 
           total_abundance, final_richness, data_source,
           lat_home, lon_home) %>%  # ADD THESE COORDINATES
    rename_with(~paste0("home_bio_week_", .), -c(cluster_id, year_week, lat_home, lon_home)) %>%
    rename(!!user_var := cluster_id)
  
  merged_data <- merged_data %>%
    left_join(home_bio_weekly, by = c(user_var, "year_week"))
  
  return(merged_data)
}

# Execute the merging
cat("Starting attribute merging process...\n")

# Main merge of site attributes
choice_set_with_attributes <- merge_daily_trip_attributes(choice_set)

# Add home attributes if user data available
choice_set_with_attributes <- merge_home_attributes(choice_set_with_attributes)


# Print comprehensive summary
cat("\n=== MERGE SUMMARY ===\n")
cat("Original choice set rows:", nrow(choice_set), "\n")
cat("Merged data rows:", nrow(choice_set_with_attributes), "\n")
cat("New columns added:", length(setdiff(colnames(choice_set_with_attributes), colnames(choice_set))), "\n")

# Check data coverage (missing values)
cat("\n=== DATA COVERAGE ===\n")
key_vars <- c("bio_final_richness", "bio_shannon", "cong_n_users", "precip", "temp", "trees")
existing_key_vars <- intersect(key_vars, colnames(choice_set_with_attributes))

for(var in existing_key_vars) {
  missing_count <- sum(is.na(choice_set_with_attributes[[var]]))
  missing_pct <- round(100 * missing_count / nrow(choice_set_with_attributes), 2)
  cat(sprintf("%-20s: %7d missing (%6.2f%%)\n", var, missing_count, missing_pct))
}

# Check temporal coverage
cat("\n=== TEMPORAL COVERAGE ===\n")
date_range <- range(choice_set_with_attributes[[names(choice_set_with_attributes)[grepl("date|Date", names(choice_set_with_attributes))][1]]], na.rm = TRUE)
cat("Trip date range:", as.character(date_range[1]), "to", as.character(date_range[2]), "\n")

month_coverage <- choice_set_with_attributes %>%
  filter(!is.na(bio_final_richness)) %>%
  distinct(year_month) %>%
  nrow()
cat("Months with biodiversity data:", month_coverage, "\n")

# Save the merged choice set
output_filename <- "master_cs50km_clust10km_sample1_attributes.rds" 
output_path <- file.path("data", "clean", output_filename)
saveRDS(choice_set_with_attributes, output_path)
cat("\nMerged choice set saved to:", output_path, "\n")

