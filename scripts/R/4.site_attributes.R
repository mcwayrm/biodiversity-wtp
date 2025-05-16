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
    select(lat, lon, name) %>% # Keep necessary columns
    distinct(lat, lon, .keep_all = TRUE) # Keep only unique lat/lon observations
    stopifnot(nrow(hotspots) == 12561) # CHECK: Obs = 12,561 hotspots

# Straight-line dist from home to hotspot
cat("Generating Hotspot Buffers...\n")
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
start_time <- Sys.time()
cat("Running Zonal Stats for Rainfall...\n")
for (f in raster_files) {
    r <- rast(f)
    date_label <- sub(".*_(\\d{4})_(\\d{2})\\.tif$", "\\1-\\2", basename(f))
    # Extract mean for each buffer
    stats <- terra::extract(r, vect(hotspots$buffer), fun = mean, na.rm = TRUE)
    
    # Combine with hotspot lat/lon (row numbers correspond)
    stats_df <- hotspots %>%
        select(lat, lon) %>%
        mutate(
            year_month = date_label,
            precip = stats[[2]]
        )
    precip_stats[[date_label]] <- stats_df
}
hotspots_precip <- bind_rows(precip_stats)

end_time <- Sys.time()
cat("Rainfall Zonal Stats (Mins): ", round(difftime(end_time, start_time, units = "secs"), 2)/60, "\n")

# ----------------------------------------------------
# 2. Attribute Temperature
# ----------------------------------------------------
dir_path <- file.path(config$temp_basic_path)
raster_files <- list.files(dir_path, pattern = "\\.tif$", full.names = TRUE)
raster_files <- sort(raster_files)

temp_stats <- list()
start_time <- Sys.time()
cat("Running Zonal Stats for Temperature\n")
for (f in raster_files) {
  r <- rast(f)
  stats <- terra::extract(r, vect(hotspots$buffer), fun = mean, na.rm = TRUE)
  date_label <- sub(".*_(\\d{4})_(\\d{2})\\.tif$", "\\1-\\2", basename(f))

  df <- hotspots %>%
    select(lat, lon) %>%
    mutate(
      year_month = date_label,
      temp = stats[[2]]
    )

  temp_stats[[date_label]] <- df
}

hotspots_temp <- bind_rows(temp_stats)
cat("Temperature Zonal Stats (Mins): ", round(difftime(Sys.time(), start_time, units = "secs"), 2)/60, "\n")


# ----------------------------------------------------
# 3. Attribute Tree Cover
# ----------------------------------------------------
dir_path <- file.path(config$trees_basic_path)
raster_files <- list.files(dir_path, pattern = "Percent_Tree_Cover.*\\.tif$", full.names = TRUE)
raster_files <- raster_files[!grepl("SD", raster_files)]
raster_files <- sort(raster_files)

tree_stats <- list()
start_time <- Sys.time()
cat("Running Zonal Stats for Tree Cover\n")
for (f in raster_files) {
  r <- rast(f)
  stats <- terra::extract(r, vect(hotspots$buffer), fun = mean, na.rm = TRUE)
  date_label <- sub(".*doy(\\d{4}).*$", "\\1-\\2", basename(f))

  df <- hotspots %>%
    select(lat, lon) %>%
    mutate(
      year_month = date_label,
      trees = stats[[2]]
    )

  tree_stats[[date_label]] <- df
}

trees_df <- bind_rows(tree_stats)
# Create full monthly grid for all years in trees_df
months <- sprintf("%02d", 1:12)
hotspots_trees <- trees_df %>%
  mutate(year = substr(year_month, 1, 4)) %>%  # extract year from 'YYYY-MM' or 'YYYY'
  crossing(month = months) %>%          # create all month combinations
  mutate(year_month = paste0(year, "-", month)) %>%
  select(lat, lon, year_month, trees)

cat("Tree Cover Zonal Stats (Mins): ", round(difftime(Sys.time(), start_time, units = "secs"), 2)/60, "\n")


# ----------------------------------------------------
# 4. Attribute Species richness
# ----------------------------------------------------
cat("Generating Species Richness Indices...\n")

ebird <- fread(config$ebird_basic_path,
              select = c('LATITUDE', 'LONGITUDE','OBSERVATION DATE',
                        'OBSERVER ID', 'SAMPLING EVENT IDENTIFIER',
                        'PROTOCOL TYPE','DURATION MINUTES',
                        'EFFORT DISTANCE KM','ALL SPECIES REPORTED',
                        'LOCALITY', 'LOCALITY TYPE', 'COMMON NAME', 'CATEGORY'),
              quote = "")
colnames(ebird) <- gsub('\\.', '_', tolower(make.names(colnames(ebird))))

# Filter for species-level records at hotspots
species <- ebird %>%
  filter(locality_type == 'H', category == "species") %>%
  select(lat = latitude, lon = longitude, observation_date, common_name) %>%
  mutate(year_month = format(as.Date(observation_date), "%Y-%m")) %>%
  count(lat, lon, year_month, common_name, name = "n")

# Wide format: one column per species
species_wide <- species %>%
  pivot_wider(
    names_from = common_name,
    values_from = n,
    values_fill = 0
  )

# Extract species columns as matrix
species_matrix <- species_wide %>%
  select(-lat, -lon, -year_month) %>%
  as.matrix()

# Calculate diversity metrics
hotspots_species <- species_wide %>%
  mutate(
    richness = rowSums(species_matrix > 0),
    shannon = vegan::diversity(species_matrix, index = "shannon"),
    simpson = vegan::diversity(species_matrix, index = "simpson")
  ) %>%
  select(lat, lon, year_month, richness, shannon, simpson)


# Note the data is not time varying
# NOTE: This is data imputation when e-bird lagged species richness is missing

# Start with Simpson and Shannon indices
    # Simple and agonistic about structure 
# Next we do Weitzman index 
    # Pull data bird genology 
    # RYAN: Find data set 

# Still need to make the species richness data (panel data) using lagged e-bird data 


# ----------------------------------------------------
# 5. Merge all attributes
# ----------------------------------------------------

site_attributes <- hotspots_precip %>%
  left_join(hotspots_temp, by = c("lat", "lon", "year_month")) %>%
  left_join(hotspots_trees, by = c("lat", "lon", "year_month")) %>%
  left_join(hotspots_species, by = c("lat", "lon", "year_month"))


# ----------------------------------------------------
# 6. Save updated hotspots with attributes
# ----------------------------------------------------

# Save updated hotspots
saveRDS(hotspots, file = file.path("data", "intermediate", "hotspots", "hotspots_attributes.rds"))
cat("Site Attributes saved.\n")

# TODO: better handling of missing values
skim(site_attributes)
# temp, precip, trees likely missing due to buffers being outside of raster extent (off coast)
# species richness needs to be imputed with species range data 
