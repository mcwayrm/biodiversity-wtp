# PROJECT: RUM Model
# PURPOSE: Select Choice Set
# AUTHOR: Raahil Madhok
####################################
# Description:
# We start with a list of choices sets from hotspots. This is what they chose between. 
# Trips is the actual choice. We compare the trip to the potential hotspot choices (filtered)
# Only consider hotspots close to you and for the counterfacutal alternative that are in the same cluster.
# Selection is ACROSS clusters. So, Trip is within a cluster and we consider the counterfactuals of going to other clusters within a radius of `home`.
# This results in a data set of counterfacutals and actual trips to be used in the mixed logit. 
####################################

choice_set <- function(choice_df, trip_df, module = 'cluster', radius = 20, clust_size = 10) {
  # Define output file path
  file_path <- ifelse(module == 'cluster',
                      paste0('./data/intermediate/choice_sets/choice_set_', radius, 'km_clust_', clust_size, 'km.rds'),
                      paste0('./data/intermediate/choice_sets/choice_set_', radius, 'km.rds'))
  
  # Check if the file already exists
  if (file.exists(file_path)) {
    writeLines(paste("Loading existing choice set from:", file_path))
    return(readRDS(file_path))
  }
  
  # Generate cleaned hotspot list (grouping the hotspots into clusters)
  writeLines(paste("Processing choice set with module:", module))
  hs <- rec_clust(choice_df, module = module, clust_size = ifelse(module == 'cluster', clust_size, NA)) %>% # Command comes from hotspot_clustering.R
    mutate(lat2 = lat, lon2 = lon)
    
  # Filter observed trips within radius
  writeLines(paste("Extracting choice set within", radius, "km from home..."))
  trip_filtered <- trip_df %>%
    filter(geo_dist <= radius) %>%
    distinct(user_id, .keep_all = TRUE) %>%
    select(user_id, lon_home, lat_home) %>%
    mutate(lat_home2 = lat_home, lon_home2 = lon_home)
  
  # Buffer around home
  home_buf <- st_as_sf(trip_filtered, coords = c('lon_home2', 'lat_home2'), crs = 4326) %>%
    st_buffer(set_units(radius, km))
  
  # Get hotspots within radius
  choice <- st_join(home_buf, st_as_sf(hs, coords = c('lon2', 'lat2'), crs = 4326), join = st_intersects) %>%
    filter(!is.na(lat)) %>%
    st_drop_geometry()
  
  # Compute distance from home to alternatives
  writeLines("Computing distance from home to counterfactual sites...")
  choice$geo_dist <- st_distance(
    st_as_sf(choice, coords = c('lon_home', 'lat_home'), crs = 4326),
    st_as_sf(choice, coords = c('lon', 'lat'), crs = 4326),
    by_element = TRUE
  ) %>% set_units(km) %>% as.numeric()
  
  # Save results
  saveRDS(choice, file_path)
  writeLines(paste("Choice set saved to:", file_path))
  
  return(choice)
}