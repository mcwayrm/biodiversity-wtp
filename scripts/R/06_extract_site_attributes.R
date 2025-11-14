#!/usr/bin/env Rscript
# scripts/R/6_extract_site_attributes.R
#############################################
#  - Loads Voronoi polygons
#  - Extracts zonal statistics from raster data (precipitation, temperature, tree cover)
#  - Saves: outputs$precip, outputs$temp, outputs$trees
#
#  Required params:
#    - projection_crs: CRS for processing
#  Required inputs:
#    - voronoi_shp: Voronoi polygons GPKG
#    - hotspots_clustered: Clustered hotspots with cluster_id
#    - precip_dir: Directory with precipitation rasters
#    - temp_dir: Directory with temperature rasters
#    - trees_dir: Directory with tree cover rasters
#############################################

# -----------------------------------------------------------------------------
# Extract Zonal Statistics from Raster Files
# -----------------------------------------------------------------------------
extract_zonal_stats <- function(
  variable_name,                # Name of variable (e.g., "precip", "temp", "trees")
  dir_path,                     # Directory containing raster files
  hotspots_data,                # Hotspots dataframe with cluster_id
  voronoi_buffer,               # Voronoi polygons for extraction
  date_pattern,                 # Regex pattern to extract date from filename
  date_replacement,             # Replacement pattern for date extraction
  file_pattern = "\\.tif$",     # Pattern to match raster files
  exclude_pattern = NULL,       # Pattern to exclude files (e.g., "SD")
  time_frequency = "monthly",   # "monthly", "annual", or custom
  extraction_fun = mean,        # Function for zonal statistics
  na.rm = TRUE                  # Remove NA values in extraction
) {
  
  if (!dir.exists(dir_path)) {
    stop("Directory path does not exist: ", dir_path)
  }
  
  # Get list of raster files
  raster_files <- list.files(dir_path, pattern = file_pattern, full.names = TRUE)
  
  if (!is.null(exclude_pattern)) {
    raster_files <- raster_files[!grepl(exclude_pattern, raster_files)]
  }
  
  raster_files <- sort(raster_files)
  
  if (length(raster_files) == 0) {
    stop("No raster files found matching pattern in: ", dir_path)
  }
  
  message("Extracting ", variable_name, " from ", length(raster_files), " files")
  
  # Initialize storage
  variable_stats <- list()
  
  # Convert buffer to terra vector object once
  buffer_vect <- terra::vect(voronoi_buffer)
  
  # Loop through raster files
  for (i in seq_along(raster_files)) {
    f <- raster_files[i]
    
    # Progress indicator
    if (i %% ceiling(length(raster_files) / 4) == 0) {
      percent <- round(100 * i / length(raster_files))
      message("  Progress: ", i, "/", length(raster_files), " (", percent, "%)")
    }
    
    tryCatch({
      # Load raster
      r <- terra::rast(f)
      
      # Extract date from filename
      date_label <- sub(date_pattern, date_replacement, basename(f))
      
      if (date_label == basename(f)) {
        warning("Date pattern did not match for file: ", basename(f))
        next
      }
      
      # Check and align CRS
      if (!terra::same.crs(r, buffer_vect)) {
        buffer_vect_proj <- terra::project(buffer_vect, terra::crs(r))
      } else {
        buffer_vect_proj <- buffer_vect
      }
      
      # Extract zonal statistics
      stats <- terra::extract(r, buffer_vect_proj, 
                              fun = extraction_fun, 
                              na.rm = na.rm, 
                              touches = TRUE)
      
      if (is.null(stats) || nrow(stats) == 0) {
        warning("No data extracted for file: ", basename(f))
        next
      }
      
      # Get data column (first non-ID column)
      data_columns <- colnames(stats)[colnames(stats) != "ID"]
      if (length(data_columns) == 0) {
        warning("No data columns found for file: ", basename(f))
        next
      }
      
      data_col <- stats[[data_columns[1]]]
      
      # Handle NA values by filling from nearest polygon
      na_indices <- which(is.na(data_col))
      if (length(na_indices) > 0) {
        all_centroids <- terra::centroids(buffer_vect_proj)
        valid_idx <- which(!is.na(data_col))
        valid_centroids <- all_centroids[valid_idx]
        valid_values <- data_col[valid_idx]
        
        for (na_idx in na_indices) {
          target_centroid <- all_centroids[na_idx]
          dists <- terra::distance(target_centroid, valid_centroids)
          nearest_idx <- which.min(dists)
          data_col[na_idx] <- valid_values[nearest_idx]
        }
      }
      
      # Verify data length
      if (length(data_col) != nrow(hotspots_data)) {
        stop("Data length mismatch for file: ", basename(f))
      }
      
      # Create results dataframe
      stats_df <- hotspots_data %>%
        mutate(
          !!rlang::sym(if (time_frequency == "annual") "year" else "year_month") := date_label,
          !!rlang::sym(variable_name) := data_col
        )
      
      variable_stats[[date_label]] <- stats_df
      
    }, error = function(e) {
      message("Error processing file ", basename(f), ": ", e$message)
    })
  }
  
  if (length(variable_stats) == 0) {
    stop("No data was successfully extracted")
  }
  
  # Combine all results
  result_data <- dplyr::bind_rows(variable_stats)
  
  # Summary
  total_obs <- nrow(result_data)
  missing_obs <- sum(is.na(result_data[[variable_name]]))
  
  message("Extraction complete: ", total_obs, " observations, ", 
          missing_obs, " missing (", round(100 * missing_obs / total_obs, 2), "%)")
  
  return(result_data)
}

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------

# Load clustered hotspots (for cluster_id reference)
hotspots <- read_parquet(inputs$hotspots_clustered) %>%
  select(cluster_id, lat, lon) %>%
  distinct(cluster_id, .keep_all = TRUE)

# Load Voronoi polygons (limited version for extraction)
voronoi_polygons <- st_read(inputs$voronoi_shp, 
                           layer = "cluster_voronoi_limited",
                           quiet = TRUE)

# Transform to WGS84 for raster extraction
voronoi_wgs84 <- st_transform(voronoi_polygons, crs = 4326)

message("Loaded ", nrow(hotspots), " clusters and ", nrow(voronoi_polygons), " Voronoi polygons")

# -----------------------------------------------------------------------------
# Extract Precipitation
# -----------------------------------------------------------------------------

hotspots_precip <- extract_zonal_stats(
  variable_name = "precip",
  dir_path = inputs$precip_dir,
  hotspots_data = hotspots,
  voronoi_buffer = voronoi_wgs84,
  date_pattern = ".*_(\\d{4})_(\\d{2})\\.tif$",
  date_replacement = "\\1-\\2",
  time_frequency = "monthly"
)

write_parquet(hotspots_precip, outputs$precip)

# -----------------------------------------------------------------------------
# Extract Temperature
# -----------------------------------------------------------------------------

hotspots_temp <- extract_zonal_stats(
  variable_name = "temp",
  dir_path = inputs$temp_dir,
  hotspots_data = hotspots,
  voronoi_buffer = voronoi_wgs84,
  date_pattern = ".*_(\\d{4})_(\\d{2})\\.tif$",
  date_replacement = "\\1-\\2",
  time_frequency = "monthly"
)

write_parquet(hotspots_temp, outputs$temp)

# -----------------------------------------------------------------------------
# Extract Tree Cover
# -----------------------------------------------------------------------------

hotspots_trees <- extract_zonal_stats(
  variable_name = "trees",
  dir_path = inputs$trees_dir,
  hotspots_data = hotspots,
  voronoi_buffer = voronoi_wgs84,
  file_pattern = "Percent_Tree_Cover.*\\.tif$",
  exclude_pattern = "SD",
  date_pattern = ".*doy(\\d{4}).*$",
  date_replacement = "\\1",
  time_frequency = "annual"
)

write_parquet(hotspots_trees, outputs$trees)

message("=== SITE ATTRIBUTE EXTRACTION COMPLETE ===")
message("Precipitation: ", nrow(hotspots_precip), " records")
message("Temperature: ", nrow(hotspots_temp), " records")
message("Tree cover: ", nrow(hotspots_trees), " records")