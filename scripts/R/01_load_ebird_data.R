# scripts/R/1_load_raw_data.R
#############################################
#  - Loads eBird basic file (58M rows)
#  - Cleans names
#  - Saves: data/scenario_dir/ebird_trips.parquet
#############################################

#-----------------------------------------------------
# eBird Cleaning
#-----------------------------------------------------
# Load eBird
ebird <- fread(
  inputs$ebird_basic,
  select = c(
    'LATITUDE', 'LONGITUDE', 'OBSERVATION DATE',
    'OBSERVER ID', 'SAMPLING EVENT IDENTIFIER',
    'PROTOCOL TYPE', 'DURATION MINUTES',
    'EFFORT DISTANCE KM', 'ALL SPECIES REPORTED',
    'LOCALITY', 'LOCALITY TYPE'
  ),
  quote = ""
)
stopifnot(nrow(ebird) == 58563093) # Asserts correct ebird file size

# Standardize names
colnames(ebird) <- gsub("\\.", "_", tolower(make.names(colnames(ebird))))

# Rename to clean schema
ebird <- ebird %>%
  rename(
    lat = latitude,
    lon = longitude,
    user_id = observer_id,
    trip_id = sampling_event_identifier,
    duration = duration_minutes,
    distance = effort_distance_km,
    complete = all_species_reported
  )

# Set distance = 0 for stationary trips
ebird <- mutate(ebird, distance = replace(distance, is.na(distance), 0))

# Collapse to user–trip level
ebird <- distinct(ebird, trip_id, .keep_all = TRUE)
stopifnot(nrow(ebird) == 3463250) # Asserts unique user-trips in ebird data

# Add dates
ebird$date      <- ymd(ebird$observation_date)
ebird$year      <- year(ebird$date)
ebird$month     <- month(ebird$date)
ebird$yearmonth <- format(ebird$date, "%Y-%m")

# Filter Trips
# Note: identify recreational birdwatching sessions as opposed to incidental app-recording
ebird <- ebird %>%
  filter(
    complete == 1, # Completed trip
    duration <= 5 * 60, # Duration is in minutes. So selecting all observations less than 5 hours (didn't leave the app open)
    #number_observers <= 10 # Is the thought here that there should not be large parties? Large parties indicate tourists?
  )
  stopifnot(nrow(ebird) == 3008200) # CHECK: Obs = 3,008,200 trips

# Select Protocol (n=2,991,609 trips)
protocol <- c('Stationary', 'Traveling')
ebird <- filter(ebird, protocol_type %in% protocol)
  stopifnot(nrow(ebird) == 2991609) # CHECK: Obs = 2,991,609 trips
  # Note: 1,659 trips removed (0.05%)

# Add district code to trips 
# Load District Map
dist <- st_read(inputs$district_shp, quiet = TRUE) %>%
  dplyr::select(c_code_11) %>% rename(c_code_2011 = c_code_11)
  # CHECK: Obs = 641 

ebird$c_code_2011 <- st_join(st_as_sf(ebird,
                                      coords = c('lon', 'lat'),
                                      crs = 4326), # World Geodetic System 1984
                            dist,
                            join = st_intersects)$c_code_2011

# Remove out-of-bounds trips
ebird <- filter(ebird, !is.na(c_code_2011))
  stopifnot(nrow(ebird) == 2964004) # CHECK: Obs = 2,964,004 trips
  # Note: 7,605 trips removed (0.25%)

#-----------------------------------------------------
# Home Coordinates
#-----------------------------------------------------
# Note: compute home based on *all* trips
# Real Home (n=393 users) ----------------------------
user_home <- ebird %>%
  filter(str_detect(str_to_lower(locality),
                    'my home|my house|my balcony|
                    my terrace|my backyard|my street|
                    my yard|my veranda')) %>%
  group_by(user_id) %>%
  summarize(lon_home_real = mean(lon, na.rm = TRUE),
            lat_home_real = mean(lat, na.rm = TRUE))
  stopifnot(nrow(user_home) == 347) # CHECK: Obs = 347 users homes

# Assign users to a district they live in using spatial intersection with district polygons
user_home$c_code_2011_home_real <- st_join(st_as_sf(user_home,
                                                    coords = c('lon_home_real', 'lat_home_real'),
                                                    crs = 4326), # World Geodetic System 1984
                                          dist,
                                          join = st_intersects)$c_code_2011
# Save home data
write_parquet(user_home, outputs$ebird_trips_home)

# Imputed Home --------------------------------------
# 1. Estimate gravitational center of all trips
# 2. Compute distance from "home" to each trip
# 3. Remove outliers (e.g. faraway trips)
# 4. Recompute "home" from remaining trips

# Center of user's trips
user <- distinct(ebird, user_id, lon, lat) %>%
  group_by(user_id) %>%
  mutate(lon_home = mean(lon, na.rm = TRUE),
        lat_home = mean(lat, na.rm = TRUE))
  stopifnot(nrow(user) == 1086215) # CHECK: Obs = 1,086,215 trips
# Straight-line distance from center to each site (linear-arc distance)
user$distance <- st_distance(st_as_sf(user,
                                      coords = c('lon_home', 'lat_home'),
                                      crs = 4326), # World Geodetic System 1984
                            st_as_sf(user,
                                      coords = c('lon', 'lat'),
                                      crs = 4326), # World Geodetic System 1984
                            by_element = TRUE)
user$distance <- as.numeric(user$distance) / 1000 # in km

# Remove outlier trips
  # TODO: Is the best method for outlier detection?
user <- user %>%
  group_by(user_id) %>%
  filter(distance <= quantile(distance, 0.75) + 1.5 * IQR(distance))
  stopifnot(nrow(user) == 997519) # CHECK: Obs = 997,519 trips
# Note: 88,696 trips removed (8.2%)

# Recompute home
user <- user %>%
  group_by(user_id) %>%
  summarize(lon_home = mean(lon, na.rm = TRUE),
            lat_home = mean(lat, na.rm = TRUE))
  stopifnot(nrow(user) == 42486) # CHECK: Obs = 42,486 homes

# Overlay home districts
# Note: If gravity center is off coast, home is the centroid of nearest dist
user$c_code_2011_home <- st_join(st_as_sf(user,
                                          coords = c('lon_home', 'lat_home'),
                                          crs = 4326), # World Geodetic System 1984
                                dist,
                                join = st_intersects)$c_code_2011
# Handle homes in the ocean
# For off-coast homes, assign id of nearest district
user_na <- filter(user, is.na(c_code_2011_home))
  stopifnot(nrow(user_na) == 786) # CHECK: 786 users out of 42,486
# Note: 1.9% of users have homes off-coast
user_na$c_code_2011_home <- st_join(st_as_sf(user_na,
                                            coords = c('lon_home', 'lat_home'),
                                            crs = 4326), # World Geodetic System 1984
                                    dist,
                                    join = st_nearest_feature)$c_code_2011
  sum(is.na(user_na)) # CHECK: How many NAs remain == 0
  # Note: remaining NA's are in northern Kashmir, which have no census code

# District centroids
centroids <- as.data.frame(st_coordinates(st_centroid(dist$geometry))) %>%
  rename(lon_home = X, lat_home = Y)
centroids$c_code_2011_home <- dist$c_code_2011
user_na <- left_join(user_na[, c('user_id', 'c_code_2011_home')],
                    centroids,
                    by = 'c_code_2011_home')

# TODO: Create a table of mapping off-coast homes into nearby districts.

# TODO: What is the average distance away?
# temp <- st_distance(x = st_as_sf(user_na,
#                                 coords = c('lon_home', 'lat_home'),
#                                 crs = 4326), # World Geodetic System 1984)
#                     y = dist)

# Bind to main user list
user <- rbind(filter(user, !is.na(c_code_2011_home)), user_na)
rm(list = c('centroids', 'user_na'))

# Save user list
write_parquet(user, outputs$ebird_trips_imputed)

#-----------------------------------------------------
# Final Output
#-----------------------------------------------------
# Add imputed home coordinates
ebird <- left_join(ebird, user, by = 'user_id')
write_parquet(ebird, outputs$ebird_trips)