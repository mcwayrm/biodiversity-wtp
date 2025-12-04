#!/usr/bin/env Rscript
# scripts/R/4_generate_voronoi_clusters.R
#############################################
#  - Loads hotspots file
#  - Generates clusters and Voronoi polygons (GPKG) and centroids
#  - Saves: outputs$hotspots_clustered (parquet), outputs$voronoi_gpkg (gpkg), outputs$voronoi_centroids (parquet)
#############################################

# -----------------------------------------------------------------------------
# Create Voronoi polygons from cluster centroids FUNCTION
# -----------------------------------------------------------------------------
create_cluster_voronoi <- function(hotspots_df, max_distance_km = 10, boundary_sf = NULL, proj_crs) {

  # Load district boundaries and immediately project + validate
  india_dist <- st_read(inputs$district_shp, quiet = TRUE) %>%
    st_transform(crs = proj_crs) %>%
    st_make_valid()

  # Compute cluster centroids (latitude/longitude in original coords for reference)
  cluster_centroids <- hotspots_df %>%
    group_by(cluster_id) %>%
    summarize(
      n_hotspots = n(),
      centroid_lat = mean(lat, na.rm = TRUE),
      centroid_lon = mean(lon, na.rm = TRUE),
      .groups = "drop"
    )

  # Convert centroids to sf in WGS84, then immediately project
  centroids_temp_sf <- st_as_sf(cluster_centroids,
                                coords = c('centroid_lon', 'centroid_lat'),
                                crs = 4326) %>%
    st_transform(crs = proj_crs)
  
  # Attach district codes to centroids (to filter offshore clusters)
  joined <- st_join(centroids_temp_sf, india_dist, join = st_intersects)
  cluster_centroids$c_code_2011 <- joined$c_code_11

  # Filter out offshore/invalid clusters
  cluster_centroids_filtered <- cluster_centroids %>% filter(!is.na(c_code_2011))

  if (nrow(cluster_centroids_filtered) == 0) stop("No onshore centroids available to build Voronoi polygons")

  # Convert filtered centroids to sf and project (STAY IN PROJECTED CRS)
  centroids_sf <- st_as_sf(cluster_centroids_filtered,
                          coords = c('centroid_lon', 'centroid_lat'), crs = 4326) %>%
    st_transform(crs = proj_crs) %>%
    st_make_valid()

  # Build Voronoi from filtered centroids (ALL IN PROJECTED CRS)
  voronoi_polygons <- centroids_sf %>%
    st_union() %>%
    st_voronoi() %>%
    st_collection_extract("POLYGON") %>%
    st_sf() %>%
    st_set_crs(proj_crs)  # Explicitly set CRS after voronoi

  # Assign polygons back to centroids using intersection (STAY IN PROJECTED CRS)
  voronoi_assigned <- st_join(voronoi_polygons,
                              centroids_sf,
                              join = st_intersects) %>%
    filter(!is.na(cluster_id)) %>%
    st_make_valid()

  # Clip to boundary if provided (ALL IN PROJECTED CRS)
  if (!is.null(boundary_sf)) {
    # Boundary should already be in projected CRS and valid
    voronoi_boundary_clipped <- st_intersection(
      voronoi_assigned, 
      st_union(boundary_sf)
    ) %>%
    st_make_valid()
  } else {
    voronoi_boundary_clipped <- voronoi_assigned
  }

  # Create unlimited version (transform to WGS84 only at the end)
  voronoi_unlimited <- st_transform(voronoi_boundary_clipped, crs = 4326)

  # Distance-limited version: intersect with buffers around centroids (STAY IN PROJECTED CRS)
  voronoi_distance_limited <- NULL
  if (!is.null(max_distance_km) && length(max_distance_km) == 1 && !is.na(max_distance_km)) {
    max_distance_m <- max_distance_km * 1000
    centroid_buffers <- st_buffer(centroids_sf, dist = max_distance_m) %>%
      st_make_valid()
    
    voronoi_clipped <- st_intersection(
      voronoi_boundary_clipped, 
      st_union(centroid_buffers)
    ) %>%
    st_make_valid()
    
    # Only transform to WGS84 at the very end
    voronoi_distance_limited <- st_transform(voronoi_clipped, crs = 4326)
  }

  return(list(unlimited = voronoi_unlimited, distance_limited = voronoi_distance_limited))
}


# -----------------------------------------------------------------------------
# Load hotspots and convert to clusters
# -----------------------------------------------------------------------------
hotspot_cols <- c('loc_id', 'country', 'state', 'county', 'lat', 'lon', 'name', 'time', 'v9')
hotspots <- readRDS(inputs$hotspots) %>%
  setNames(hotspot_cols) %>%
  select(lat, lon, name)

# Load and project district boundaries once
india_dist <- st_read(inputs$district_shp, quiet = TRUE) %>%
  st_transform(crs = params$projection_crs) %>%
  st_make_valid()

# Attach district codes and clean duplicates
hotspots_sf <- st_as_sf(hotspots, coords = c('lon','lat'), crs = 4326) %>%
  st_transform(crs = params$projection_crs)

hotspots$c_code_2011 <- st_join(hotspots_sf, india_dist, join = st_intersects)$c_code_11
hotspots <- hotspots %>% 
  filter(!is.na(c_code_2011)) %>%
  distinct(lat, lon, .keep_all = TRUE) %>%
  distinct(c_code_2011, name, .keep_all = TRUE)

# Project for clustering (already in projected CRS)
hotspots_proj <- st_as_sf(hotspots, coords = c('lon','lat'), crs = 4326) %>%
  st_transform(params$projection_crs)
distm <- st_distance(hotspots_proj)

# Hierarchal Cluster based on distance
hc <- hclust(as.dist(distm), method = params$clustering_method)
hotspots$cluster_id <- cutree(hc, h = params$clust_size_km * 1000)

# Build India mainland boundary (IN PROJECTED CRS, VALIDATED)
india_mainland <- india_dist %>%
  filter(!grepl("Andaman|Nicobar|Lakshadweep", STATE_UT, ignore.case = TRUE))

india_boundary <- india_mainland %>%
  st_union() %>%
  st_make_valid() %>%
  st_sf() %>%
  mutate(country = "India")

# Create Voronoi polygons (pass projected CRS to function)
voronoi_results <- create_cluster_voronoi(
  hotspots,
  max_distance_km = params$voronoi_limit_km,
  boundary_sf = india_boundary,
  proj_crs = params$projection_crs
)

st_write(voronoi_results$unlimited, outputs$voronoi_shp,
         layer = "cluster_voronoi_unlimited", delete_layer = TRUE, quiet = TRUE)
st_write(voronoi_results$distance_limited, outputs$voronoi_shp,
         layer = "cluster_voronoi_limited", delete_layer = TRUE, quiet = TRUE)


# Produce centroids table and save as parquet 
# point-on-surface in projected CRS, then convert to WGS84 to get lon/lat
poi_proj <- st_point_on_surface(st_transform(voronoi_results$distance_limited, crs = params$projection_crs)) %>%
  st_transform(crs = 4326)
coords <- st_coordinates(poi_proj)
centroids_tbl <- voronoi_results$distance_limited %>%
  st_drop_geometry() %>%
  mutate(
    lon = coords[,1],
    lat = coords[,2]
  ) %>%
  select(cluster_id, lat, lon, n_hotspots)
write_parquet(centroids_tbl, outputs$hotspots_clustered)