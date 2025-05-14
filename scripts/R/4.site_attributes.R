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


### SET-UP
# Load config
config_path <- file.path("scripts", "R", "0.load_config.R")
source(config_path)

# Bring in hotspot data (e.g., the sites we want to add attributes to)
cols <- c('loc_id', 'country', 'state', 'county', 'lat', 'lon', 'name', 'time', 'v9')
hotspots <- readRDS(config$hotspots_path) %>%
    setNames(cols) %>%  # Assign column names
    select(lat, lon, name)  # Keep necessary columns
    stopifnot(nrow(hotspots) == 12622) # CHECK: Obs = 12,622 hotspots

# Straight-line dist from home to hotspot
hotspots$buffer <- st_buffer(st_as_sf(hotspots,
                                coords = c('lon', 'lat'),
                                crs = 4326),
                            dist = 10000) # 10km buffer around each hotspot
plot(hotspots$buffer)

# ----------------------------------------------------
# 1. Attribute Rainfall
# ----------------------------------------------------
# Get list of raster files
dir_path <- file.path(config$precip_basic_path)
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
# 2. Attribute Temperature
# ----------------------------------------------------
# Get list of raster files
dir_path <- file.path(config$temp_basic_path)
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
# 3. Tree cover
# ----------------------------------------------------
# Get list of raster files
dir_path <- file.path(config$trees_basic_path)
raster_files <- list.files(dir_path, pattern = "\\.tif$", full.names = TRUE)
raster_files <- sort(raster_files)

# Loop through raster files and extract zonal stats
temp_stats <- list()
for (f in raster_files) {
    r <- rast(f)
    # Extract mean value within each buffer
    stats <- terra::extract(r, vect(hotspots$buffer), fun = mean, na.rm = TRUE)
    date_label <- sub(".*doy(\\d{4}).*$", "\\1", basename(f))
    temp_stats[[paste0("trees_", date_label)]] <- stats[[2]]
}

# Combine with original hotspots
hotspots <- bind_cols(hotspots, as.data.frame(temp_stats))
hotspots_trees <- hotspots %>%s
    pivot_longer(
        cols = starts_with("trees_"),
        names_to = "date",
        names_prefix = "trees_",
        values_to = "trees"
    )

# ----------------------------------------------------
# 4. Attribute Species richness
# ----------------------------------------------------

# Note the data is not time varying
# NOTE: This is data imputation when e-bird lagged species richness is missing

# Start with Simpson and Shannon indices
    # Simple and agonistic about structure 
# Next we do Weitzman index 
    # Pull data bird genology 
    # RYAN: Find data set 

# Still need to make the species richness data (panel data) using lagged e-bird data 


# ----------------------------------------------------
# 5. Save updated hotspots with attributes
# ----------------------------------------------------

# Update hotspots with attributes
hotspots <- hotspots %>%
    left_join(hotspots_precip, by = c("lat", "lon", "name")) %>%
    left_join(hotspots_temp, by = c("lat", "lon", "name")) %>%
    left_join(hotspots_trees, by = c("lat", "lon", "name"))

# Save updated hotspots
saveRDS(hotspots, file = config$hotspots_path)


