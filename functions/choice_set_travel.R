# PROJECT: RUM Model
# PURPOSE: Select Choice Set
# AUTHOR: Raahil Madhok

# Load Packages
require(units)
source(config$hotspot_clustering_path)

choice_set <- function(df,
                       config,
                       module = 'cluster', 
                       radius = 20, 
                       clust_size = 10){
  
  # Function: choice_set
  ## Inputs: 
  ### df: a data frame - hotspots
  ### module: - set to 'cluster' for recreation areas
  ###         - set to 'all' for hotspots  
  ### radius: choice set buffer around home
  ### clust_size: recreation area size (km)
  
  ## Outputs:
  ## df: dataframe of alternative sites w/n `radius` around home
  
  #-----------------------------------
  # GENERATE CLEANED HOTSPOT LIST
  #------------------------------------
  
  # All Hotspots (no recreation areas)
  if(module == 'all'){
    
    print('Processing hotspots...')
    
    # hotspots
    hs <- rec_clust(df,
                    config = config,
                    module = module) %>%
      mutate(lat2 = lat, lon2 = lon)
  }
  
  # Lump into Recreation Areas
  if(module == 'cluster') {
    
    print(paste('Computing recreation areas of size: ', clust_size, ' km...', sep=''))
    
    # hotspots
    hs <- rec_clust(df,
                    config = config,
                    module = module, 
                    clust_size = clust_size) %>%
      mutate(lat2 = lat, lon2 = lon)
  }
  
  #----------------------------------------------------------
  # Extract Choice Set Around Users' Homes
  #----------------------------------------------------------
  
  if(module == 'cluster'){
    
    print(paste('Extracting recreation areas (', clust_size, 'km) within ', radius, 'km from home', sep=''))
  
    } else{
    
      print(paste('Extracting choice set within ', radius, 'km from home', sep=''))
  }
  
  # User List
  ebird <- readRDS(config$ebird_trip_hotspots_path) # observed hotspot trips
  ebird <- filter(ebird, geo_dist <= radius) # observed trips  w/n radius 
  user <- distinct(ebird, user_id, .keep_all=T) %>% # User list 
    select(user_id, lon_home, lat_home) %>%
    mutate(lat_home2=lat_home, lon_home2=lon_home)
  
  # Buffer around home
  home_buf <- st_buffer(st_as_sf(user, 
                                 coords = c('lon_home2', 'lat_home2'),
                                 crs = 4326), radius/100)
  
  # Get hotspots w/n raduis
  choice <- st_join(home_buf, st_as_sf(hs, 
                                       coords = c('lon2', 'lat2'), 
                                       crs=4326), 
                    join=st_intersects)
  choice <- filter(choice, !is.na(lat)) # drop users with no hotspots near home
  choice <- st_drop_geometry(choice)
  
  # Distance from home to alternatives
  print('Computing distance from home to counterfactual sites...')
  choice$geo_dist <- st_distance(st_as_sf(choice, 
                                          coords = c('lon_home', 'lat_home'),
                                          crs=4326), 
                                 st_as_sf(choice, 
                                          coords = c('lon', 'lat'),
                                          crs=4326), 
                                 by_element = T) %>% set_units(km)
  choice$geo_dist <- as.numeric(choice$geo_dist)
  
  # Save intermediate
  if(module == 'cluster'){
    file_name <- paste0("choice_set_", radius, "km_clust_", clust_size, "km.rds")
    output_path <- do.call(file.path, c(as.list(config$choice_sets_dir), file_name))
    saveRDS(choice, output_path)
  
    } else{
    file_name <- paste0("choice_set_", radius, "km_clust_", clust_size, "km.rds")
    output_path <- do.call(file.path, c(as.list(config$choice_sets_dir), file_name))
    saveRDS(choice, output_path)
    }
  
  return(choice)
  
}

