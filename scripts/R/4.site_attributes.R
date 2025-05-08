# PROJECT: RUM Model
# PURPOSE: Construct data on site attributes (pull factors)
####################################
# Sections:
# 1. Distance to coast
# 2. Rainfall
# 3. Temperature
# 4. Tree cover
# 5. Protected area
# 6. Species richness
####################################


### SET-UP
# Load config
config_path <- file.path("scripts", "R", "0.load_config.R")
source(config_path)

library(terra) # For rasters and zonal stats

cols <- c('loc_id', 'country', 'state', 'county', 'lat', 'lon', 'name', 'time', 'v9')
hotspots <- readRDS(config$hotspots_path) %>%
  setNames(cols) %>%  # Assign column names
  select(lat, lon, name)  # Keep necessary columns
  stopifnot(nrow(hotspots) == 12622) # CHECK: Obs = 12,622 hotspots

# Straight-line dist from home to hotspot
hotspots$buffer <- st_buffer(st_as_sf(hotspots, 
                                coords = c('lon', 'lat'),
                                crs = 4326), 
                            dist = 10000)
plot(hotspots$buffer)

# ----------------------------------------------------
# 1. Attribute Distance to coast
# ----------------------------------------------------



# ----------------------------------------------------
# 2. Attribute Rainfall
# ----------------------------------------------------
# Get list of raster files
dir_path <- file.path("data", "raster", "era5_total_precipitation")
raster_files <- list.files(dir_path, pattern = "\\.tif$", full.names = TRUE)
raster_files <- sort(raster_files)

# Loop through raster files and extract zonal stats
precip_stats <- list()
for (f in raster_files) {
  r <- rast(f)
  # Extract mean value within each buffer
  stats <- terra::extract(r, vect(hotspots$buffer), fun = mean, na.rm = TRUE)
  date_label <- sub(".*_(\\d{4}_\\d{2})\\.tif$", "\\1", basename(f))
  precip_stats[[paste0("precip_", date_label)]] <- stats[[2]]
}

# Combine with original hotspots
hotspots <- bind_cols(hotspots, as.data.frame(precip_stats))
hotspots_precip <- hotspots %>%
  pivot_longer(
    cols = starts_with("precip_"),
    names_to = "date",
    names_prefix = "precip_",
    values_to = "precip"
  )

# ----------------------------------------------------
# 3. Attribute Temperature
# ----------------------------------------------------
# Get list of raster files
dir_path <- file.path("data", "raster", "era5_2m_temperature")
raster_files <- list.files(dir_path, pattern = "\\.tif$", full.names = TRUE)
raster_files <- sort(raster_files)

# Loop through raster files and extract zonal stats
temp_stats <- list()
for (f in raster_files) {
  r <- rast(f)
  # Extract mean value within each buffer
  stats <- terra::extract(r, vect(hotspots$buffer), fun = mean, na.rm = TRUE)
  date_label <- sub(".*_(\\d{4}_\\d{2})\\.tif$", "\\1", basename(f))
  temp_stats[[paste0("temp_", date_label)]] <- stats[[2]]
}

# Combine with original hotspots
hotspots <- bind_cols(hotspots, as.data.frame(temp_stats))
hotspots_temp <- hotspots %>%
  pivot_longer(
    cols = starts_with("temp_"),
    names_to = "date",
    names_prefix = "temp_",
    values_to = "temp"
  )

# ----------------------------------------------------
# 4. Attribute Tree cover
# ----------------------------------------------------


# ----------------------------------------------------
# 5. Attribute Protected area
# ----------------------------------------------------


# ----------------------------------------------------
# 6. Attribute Species richness
# ----------------------------------------------------

