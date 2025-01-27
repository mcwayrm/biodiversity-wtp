# PURPOSE: Create clusters from hotspots
# AUTHOR: Raahil Madhok

# Load Packages
require(cluster)    # clustering algorithms
require(factoextra)

rec_clust <- function(df, 
                      module = 'all',
                      clust_size = 10){
  
  # Function: rec_clust
  ## Inputs: 
  ### df: a data frame - hotspots
  ### clust_size: cluster size (km)
  
  ## Outputs:
  ## df: dataframe of hotspots with rec area ID's
  
  # Load district map
  india_dist <- st_read('./data/shp/district-2011/district-2011.shp')
  
  
  # add district code to hostpots
  df$c_code_2011 <- st_join(st_as_sf(df, coords=c('lon', 'lat'), crs=4326), 
                            india_dist, join = st_intersects)$c_code_11
  
  # Remove off-coast hotspots
  df <- filter(df, !is.na(c_code_2011))
  
  # Drop duplicates
  #---------------------------------------------------
  # Some hotspots w same lat-lon entered twice
  # Some hotspots have same name, coords slightly off
  #----------------------------------------------------
  df <- distinct(df, lat, lon, .keep_all=T) # drop duplicate coordinates 
  df <- distinct(df, c_code_2011, name, .keep_all=T) # drop duplicate names w/n district
  
  #------------------------------------------------------
  # Heirarchical Clustering of Hotspots
  #------------------------------------------------------
  
  # Distance Matrix (5 mins)
  print('computing distance matrix between hotspots...')
  distm <- st_distance(st_as_sf(df, coords = c('lon', 'lat'), crs=4326))
  
  # Construct heirachical dendogram
  hc <- hclust(as.dist(distm), method="complete")
  
  # Cut hc tree at specified distance (in meters)
  # Assigns hotspot cluster id to each hotspot
  df$hsid <- cutree(hc, h=clust_size*1000)
  
  if(module == 'all') {
    
    # Save intermediate
    df <- select(df, -hsid)
    saveRDS(df, paste('./data/rds/intermediate/hotspots/hotspots_all_', clust_size, 'km.rds', sep=''))
    
    return(df)
  
  }

  if(module == 'cluster'){
    
    # Cluster centroids
    df <- df %>%
      group_by(hsid) %>%
      summarize(lat = mean(lat), lon = mean(lon))
    
    # District in centroid of recreation area
    df$c_code_2011 <- st_join(st_as_sf(df, 
                                       coords=c('lon', 'lat'), 
                                       crs=4326), 
                              india_dist, 
                              join = st_intersects)$c_code_11
    
    # remove clusters with off-coast centers
    df <- filter(df, !is.na(c_code_2011))
    
    # Save intermediate data
    writeRDS(df, paste('./data/rds/intermediate/hotspots/hotspots_clust_', clust_size, 'km.rds', sep=''))
   
    return(df)
  
    }
  
}
