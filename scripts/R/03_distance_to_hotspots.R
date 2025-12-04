# scripts/R/3_distance_to_hotspots.R
#############################################
#  - Loads filtered eBird trips file
#  - Calculates distance from user home to visited hotspots
#  - Saves: data/scenario_dir/ebird_trips_hotspots.parquet
#############################################

#-----------------------------------------
# Select observed hotspots
#-----------------------------------------
# We identify the straight-line (Euclidean) distance
# from home to every hotspot visited

# Read ebird trips
ebird <- read_parquet(inputs$ebird_trips_filtered)

# Select trips to hotspots
ebird <- ebird %>%
        filter(locality_type == 'H') %>%
        select(-c(locality_type, locality))

#-----------------------------------------
# Euclidean Distance To Destination
#-----------------------------------------

# Straight-line dist from home to hotspot
ebird$geo_dist <- st_distance(st_as_sf(ebird, 
                                coords = c('lon_home', 'lat_home'),
                                crs = 4326), # World Geodetic System 1984
                              st_as_sf(ebird, 
                                coords = c('lon', 'lat'),
                                crs = 4326), # World Geodetic System 1984
                              by_element = TRUE) %>% set_units(km)
ebird$geo_dist <- as.numeric(ebird$geo_dist)

# Save observed choice
write_parquet(ebird, outputs$ebird_trips_hotspots)

#--------------------------------------
# Driving Distance To Destination
#--------------------------------------
# NOTE: Reason we don't do this is because it is expensive. Need to pay to run the google maps API.

# Set great circle radius (for google map)
# radius <- 30

# Keep trips w/n radius to reduce API calls
#ebird <- filter(ebird, geo_dist <= radius)
# api_key = rio::import("../data/api-key-ebird-raahil.txt")
# set.api.key(api_key)
# map_by_slice <- function(i){
#   print(i)
#   this_slice <- tryCatch({
#     as.data.frame(gmapsdistance(origin = paste(ebird[i,'lat_home'], ebird[i,'lon_home'], sep='+'),
#                                 destination = paste(ebird[i,'lat'], ebird[i,'lon'], sep='+'),
#                                 mode='driving'))
#   }, error = function(e) {
#     message("Error occurred while mapping ", i, ".")
#     data.frame(Time = NA, Distance = NA, Status = NA)
#   })
#   
#   return(this_slice)
# }
# idx <- 1:nrow(ebird)
# results <- lapply(idx, map_by_slice)
# results <- as.data.frame(rbindlist(results))
# ebird <- cbind(ebird, results[,c('Time', 'Distance')])
# ebird <- ebird %>%
#   rename(drive_time = Time, drive_dist = Distance) %>%
#   mutate(drive_time = drive_time / 60, 
#          drive_dist = drive_dist / 1000)
# rm(list='results')

# Save observed choice
#saveRDS(ebird, 'ebird_trip_hotspots.rds')