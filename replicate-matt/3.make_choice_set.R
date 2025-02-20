# PROJECT: RUM Model
# PURPOSE: Select Choice Set for each user

### SET-UP
# Directories
rm(list=ls())

# Load Packages
source("replicate-matt/load_config.R")
source(config$choice_set_travel_path)
packages <- c('sf', 'tidyverse', 'data.table')
pacman::p_load(packages, character.only = TRUE, install = FALSE)

# Settings
module <- 'cluster'    # set as 'cluster' or 'all'
samp <- 1              # random sample of users (scale 0-1)
radius <- 50          # buffer radius around home (100, 50, or 20)
clust_size <- 10       # recreation area size (10km or 5km)

# ----------------------------------------------------
# CONSTRUCT COUTERFACTUAL CHOICE SET
# ----------------------------------------------------
# Read observed trips
trips <- readRDS(config$ebird_trip_hotspots_path)
trips <- filter(trips, geo_dist <= radius) # subset trips w/n radius

# Random sample of users
set.seed(12345)
users <- sample_frac(distinct(trips, user_id), samp)

# Trips by subset of users
sample <- inner_join(trips, users, by='user_id')

# Read hotspots
cols <- c('loc_id', 'country', 'state', 'county', 'lat', 'lon', 'name', 'time', 'v9')
hotspots <- readRDS(config$hotspots_path)
  # './data/rds/hotspots.rds')
colnames(hotspots) <- cols
hotspots <- hotspots[, c('lat', 'lon', 'name')]

# Construct Choice Sets
if(module == 'cluster'){
  
  # Construct on the fly
  cset <- choice_set(hotspots,
                     module = module, 
                     radius = radius, 
                     clust_size = clust_size)
  
  # Read
  #cset <- readRDS(paste('./data/rds/intermediate/choice_sets/choice_set_', radius, 'km_clust_', clust_size,'km.rds', sep=''))
  cset <- inner_join(cset, users, by='user_id') # choice sets of random user sample
  
  # Cluster of observed trip
  sample$hsid <- st_join(st_as_sf(sample, 
                                 coords=c('lon', 'lat'), 
                                 crs=4326), 
                        st_as_sf(cset, 
                                 coords=c('lon', 'lat'), 
                                 crs=4326), 
                        join = st_nearest_feature)$hsid
  
} else{
  
  # Construct on the fly
  cset <- choice_set(hotspots, 
                     module = module, 
                     radius = radius)
  
  # Read
  #cset <- readRDS(paste('./data/rds/intermediate/choice_sets/choice_set_', radius, 'km.rds', sep=''))
  cset <- inner_join(cset, users, by='user_id') %>% rename(locality = name)
}

# Append counterfactuals
triplist <- sample$trip_id
stack_cset <- function(i) {
  
  print(paste('appending counterfactuals ', match(i, triplist), ' of ', length(triplist), sep=''))
  
  # user id of trip
  user <- sample$user_id[sample$trip_id == i]
  
  # user's choice set
  user_cset <- cset[cset$user_id == user,]

  # Remove observed site from counterfactual set
  if(module=='cluster'){
    id <- sample$hsid[sample$trip_id == i]
    user_cset <- filter(user_cset, hsid!=id)
  } else{
    trip_lat <- sample$lat[sample$trip_id == i]
    trip_lon <- sample$lon[sample$trip_id == i]
    user_cset <- filter(user_cset, lat!=trip_lat & lon!=trip_lon)
  }

  # Append choice set to trip
  row <- sample[sample$trip_id == i,]
  data <- bind_rows(row, user_cset)
  
}
master <- as.data.frame(rbindlist(lapply(triplist, stack_cset)))

# Clean
master <- master %>%
  mutate(choice = ifelse(is.na(trip_id), 0, 1)) %>%
  select(-c(starts_with('lat'), starts_with('lon'), duration, 
            distance, num_observers, complete)) %>%
  fill(trip_id, date, year, month, yearmonth, c_code_2011_home)

# Save
if(module == 'cluster'){
  
  # Remove locality -- save disk space
  master <- select(master, -locality)
  
  # Construct full path
  file_name <- paste0("master_cs", radius, "km_clust_", clust_size, "km.rds")
  output_path <- do.call(file.path, append(config$choice_sets_dir, file_name))
  saveRDS(master, output_path)
  print('fin!')
} else{
    # Construct full path
  file_name <- paste0("master_cs", radius, "km_clust_", clust_size, "km.rds")
  output_path <- do.call(file.path, append(config$choice_sets_dir, file_name))
  saveRDS(master, output_path)
  print('fin!')
}

