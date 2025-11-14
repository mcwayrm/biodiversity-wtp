#!/usr/bin/env Rscript
# scripts/R/8_merge_site_attributes.R
#############################################
#  - Loads master choice set data
#  - Loads all site attributes (weather, biodiversity, protected areas)
#  - Merges attributes to choice set with temporal matching and fallback logic
#  - Saves: outputs$master_data_with_attributes
#
#  Required inputs:
#    - master_data: Choice set from stage 5
#    - precip, temp, trees: Raster-extracted attributes from stage 6
#    - monthly/weekly/seasonal richness & congestion: From stage 7
#    - protected_areas_shp: Protected areas shapefile
#    - hotspots_clustered: For cluster locations (protected area distance calc)
#############################################

# -----------------------------------------------------------------------------
# Load Choice Set
# -----------------------------------------------------------------------------

choice_set <- read_parquet(inputs$master_data)
setDT(choice_set)

message("Loaded choice set: ", nrow(choice_set), " rows")
message("Unique trips: ", choice_set[, uniqueN(trip_id)])
message("Unique clusters: ", choice_set[, uniqueN(cluster_id)])

# Prepare cluster_id as character
choice_set[, cluster_id := as.character(cluster_id)]

# -----------------------------------------------------------------------------
# Create Temporal Matching Variables
# -----------------------------------------------------------------------------

choice_set[, `:=`(
  year = year(date),
  month = month(date),
  week = isoweek(date),
  year_week = sprintf("%04d-%02d", isoyear(date), isoweek(date)),
  year_month = format(date, "%Y-%m"),
  season = fcase(
    month %in% c(12, 1, 2), "Winter",
    month %in% c(3, 4, 5), "Spring",
    month %in% c(6, 7, 8), "Summer",
    default = "Fall"
  )
)]
choice_set[, year_season := paste0(year, "-", season)]

# Create lookback periods for expected richness/congestion
choice_set[, `:=`(
  prev_week = sprintf("%04d-%02d", 
                     isoyear(date - 7), 
                     isoweek(date - 7)),
  prev_month = format(date %m-% months(1), "%Y-%m"),
  prev_year_season = paste0(year - 1, "-", season)
)]

message("Created temporal matching variables")

# -----------------------------------------------------------------------------
# Load and Merge Species Richness (with fallback logic)
# -----------------------------------------------------------------------------

message("\n--- Merging Species Richness ---")

monthly_richness <- read_parquet(inputs$monthly_richness)
weekly_richness <- read_parquet(inputs$weekly_richness)
seasonal_richness <- read_parquet(inputs$seasonal_richness)

setDT(monthly_richness)
setDT(weekly_richness)
setDT(seasonal_richness)

monthly_richness[, cluster_id := as.character(cluster_id)]
weekly_richness[, cluster_id := as.character(cluster_id)]
seasonal_richness[, cluster_id := as.character(cluster_id)]

# Merge previous week richness
choice_set <- merge(
  choice_set,
  weekly_richness[, .(cluster_id, year_week, prev_week_richness = richness)],
  by.x = c("cluster_id", "prev_week"),
  by.y = c("cluster_id", "year_week"),
  all.x = TRUE
)

# Merge previous month richness
choice_set <- merge(
  choice_set,
  monthly_richness[, .(cluster_id, year_month, prev_month_richness = richness)],
  by.x = c("cluster_id", "prev_month"),
  by.y = c("cluster_id", "year_month"),
  all.x = TRUE
)

# Merge previous year season richness
choice_set <- merge(
  choice_set,
  seasonal_richness[, .(cluster_id, year_season, prev_year_season_richness = richness)],
  by.x = c("cluster_id", "prev_year_season"),
  by.y = c("cluster_id", "year_season"),
  all.x = TRUE
)

# Apply fallback logic: prev_week -> prev_month -> prev_year_season
choice_set[, expected_richness := fcoalesce(
  prev_week_richness,
  prev_month_richness,
  prev_year_season_richness
)]

choice_set[, expected_richness_source := fcase(
  !is.na(prev_week_richness), "prev_week",
  !is.na(prev_month_richness), "prev_month",
  !is.na(prev_year_season_richness), "prev_year_season",
  default = "missing"
)]

message("Expected richness available: ", 
        choice_set[!is.na(expected_richness), .N], " / ", nrow(choice_set))

# -----------------------------------------------------------------------------
# Load and Merge Congestion (with fallback logic)
# -----------------------------------------------------------------------------

message("\n--- Merging Congestion ---")

monthly_congestion <- read_parquet(inputs$monthly_congestion)
weekly_congestion <- read_parquet(inputs$weekly_congestion)
seasonal_congestion <- read_parquet(inputs$seasonal_congestion)

setDT(monthly_congestion)
setDT(weekly_congestion)
setDT(seasonal_congestion)

monthly_congestion[, cluster_id := as.character(cluster_id)]
weekly_congestion[, cluster_id := as.character(cluster_id)]
seasonal_congestion[, cluster_id := as.character(cluster_id)]

# Merge previous week congestion
choice_set <- merge(
  choice_set,
  weekly_congestion[, .(cluster_id, year_week, prev_week_users = n_users)],
  by.x = c("cluster_id", "prev_week"),
  by.y = c("cluster_id", "year_week"),
  all.x = TRUE
)

# Merge previous month congestion
choice_set <- merge(
  choice_set,
  monthly_congestion[, .(cluster_id, year_month, prev_month_users = n_users)],
  by.x = c("cluster_id", "prev_month"),
  by.y = c("cluster_id", "year_month"),
  all.x = TRUE
)

# Merge previous year season congestion
choice_set <- merge(
  choice_set,
  seasonal_congestion[, .(cluster_id, year_season, prev_year_season_users = n_users)],
  by.x = c("cluster_id", "prev_year_season"),
  by.y = c("cluster_id", "year_season"),
  all.x = TRUE
)

# Apply fallback logic
choice_set[, expected_congestion := fcoalesce(
  prev_week_users,
  prev_month_users,
  prev_year_season_users
)]

choice_set[, expected_congestion_source := fcase(
  !is.na(prev_week_users), "prev_week",
  !is.na(prev_month_users), "prev_month",
  !is.na(prev_year_season_users), "prev_year_season",
  default = "missing"
)]

message("Expected congestion available: ", 
        choice_set[!is.na(expected_congestion), .N], " / ", nrow(choice_set))

# -----------------------------------------------------------------------------
# Merge Weather and Tree Cover
# -----------------------------------------------------------------------------

message("\n--- Merging Weather and Tree Cover ---")

# Precipitation
precip_data <- read_parquet(inputs$precip)
setDT(precip_data)
precip_data[, cluster_id := as.character(cluster_id)]

choice_set <- merge(
  choice_set,
  precip_data[, .(cluster_id, year_month, precip)],
  by = c("cluster_id", "year_month"),
  all.x = TRUE
)
message("Merged precipitation: ", 
        choice_set[!is.na(precip), .N], " / ", nrow(choice_set))

# Temperature
temp_data <- read_parquet(inputs$temp)
setDT(temp_data)
temp_data[, cluster_id := as.character(cluster_id)]

choice_set <- merge(
  choice_set,
  temp_data[, .(cluster_id, year_month, temp)],
  by = c("cluster_id", "year_month"),
  all.x = TRUE
)
message("Merged temperature: ", 
        choice_set[!is.na(temp), .N], " / ", nrow(choice_set))

# Tree cover
trees_data <- read_parquet(inputs$trees)
setDT(trees_data)
trees_data[, `:=`(
  cluster_id = as.character(cluster_id),
  year = as.numeric(year)
)]

choice_set <- merge(
  choice_set,
  trees_data[, .(cluster_id, year, trees)],
  by = c("cluster_id", "year"),
  all.x = TRUE
)
message("Merged tree cover: ", 
        choice_set[!is.na(trees), .N], " / ", nrow(choice_set))

# -----------------------------------------------------------------------------
# Process and Merge Protected Areas
# -----------------------------------------------------------------------------

message("\n--- Processing Protected Areas ---")

# Load clustered hotspots for locations
hotspots <- read_parquet(inputs$hotspots_clustered)
setDT(hotspots)
hotspots[, cluster_id := as.character(cluster_id)]

# Get cluster centroids
cluster_centroids <- hotspots[, .(
  lat = mean(lat, na.rm = TRUE),
  lon = mean(lon, na.rm = TRUE)
), by = cluster_id]

# Load protected areas shapefile
protected_areas_raw <- st_read(inputs$protected_areas_shp, quiet = TRUE)

# Clean and validate geometries
message("Cleaning protected area geometries...")
valid_indices <- logical(nrow(protected_areas_raw))

for (i in 1:nrow(protected_areas_raw)) {
  if (i %% 100 == 0) message("  Processing geometry ", i, "/", nrow(protected_areas_raw))
  
  tryCatch({
    geom_fixed <- st_make_valid(protected_areas_raw[i, ])
    
    if (!st_is_empty(geom_fixed) && 
        st_geometry_type(geom_fixed) %in% c("POLYGON", "MULTIPOLYGON") &&
        as.numeric(st_area(geom_fixed)) > 0) {
      protected_areas_raw[i, ] <- geom_fixed
      valid_indices[i] <- TRUE
    }
  }, error = function(e) {
    valid_indices[i] <- FALSE
  })
}

protected_areas <- protected_areas_raw[valid_indices, ] %>%
  st_transform(crs = params$projection_crs)

message("Valid protected areas: ", nrow(protected_areas), " polygons")

# Convert cluster centroids to sf
hotspots_points <- st_as_sf(cluster_centroids, 
                            coords = c('lon', 'lat'), 
                            crs = 4326) %>%
  st_transform(crs = params$projection_crs)

# Check if clusters are in protected areas
intersections <- st_intersects(hotspots_points, protected_areas)
is_in_pa <- lengths(intersections) > 0

# Calculate distance to nearest PA
distances_to_pa <- numeric(nrow(hotspots_points))
distances_to_pa[is_in_pa] <- 0

if (sum(!is_in_pa) > 0) {
  message("Computing distances for ", sum(!is_in_pa), " clusters outside protected areas")
  points_outside <- hotspots_points[!is_in_pa, ]
  dist_matrix <- st_distance(points_outside, protected_areas)
  min_distances <- apply(dist_matrix, 1, min)
  distances_to_pa[!is_in_pa] <- as.numeric(min_distances) / 1000  # Convert to km
}

# Create PA data table
pa_data <- data.table(
  cluster_id = cluster_centroids$cluster_id,
  in_protected_area = is_in_pa,
  dist_to_pa_km = distances_to_pa
)

# Merge to choice set
choice_set <- merge(
  choice_set,
  pa_data,
  by = "cluster_id",
  all.x = TRUE
)

message("Clusters in protected areas: ", sum(pa_data$in_protected_area), 
        " / ", nrow(pa_data))

# -----------------------------------------------------------------------------
# Save Final Output
# -----------------------------------------------------------------------------

write_parquet(choice_set, outputs$master_data_with_attributes)

message("\n=== MERGE COMPLETE ===")
message("Final dataset: ", nrow(choice_set), " rows, ", ncol(choice_set), " columns")

# Summary statistics
message("\n--- Data Availability Summary ---")
message("Expected richness: ", 
        sprintf("%.1f%%", 100 * choice_set[!is.na(expected_richness), .N] / nrow(choice_set)))
message("Expected congestion: ", 
        sprintf("%.1f%%", 100 * choice_set[!is.na(expected_congestion), .N] / nrow(choice_set)))
message("Precipitation: ", 
        sprintf("%.1f%%", 100 * choice_set[!is.na(precip), .N] / nrow(choice_set)))
message("Temperature: ", 
        sprintf("%.1f%%", 100 * choice_set[!is.na(temp), .N] / nrow(choice_set)))
message("Tree cover: ", 
        sprintf("%.1f%%", 100 * choice_set[!is.na(trees), .N] / nrow(choice_set)))

# Source breakdown
message("\n--- Richness Source Breakdown ---")
print(choice_set[, .N, by = expected_richness_source][order(-N)])

message("\n--- Congestion Source Breakdown ---")
print(choice_set[, .N, by = expected_congestion_source][order(-N)])