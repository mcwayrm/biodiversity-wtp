# PROJECT: eBird Valuation
# PURPOSE: Clean ebird data
####################################
# Sections:
# 1. Pre-process the raw e-bird data into a usable format
# 2. Determine what is home for the users
# 3. Filter trips to only those that are actual bird watching sessions
# 4. Identify Districts from e-bird trips
# 5. Save clean e-bird data
####################################

### SET-UP
# Load config
config_path <- file.path("scripts", "R", "0.load_config.R")
source(config_path)

# Load District Map
dist <- st_read(config$district_path) %>%
  dplyr::select(c_code_11) %>% rename(c_code_2011 = c_code_11)


#-----------------------------------------------------
# 1. Pre-Process
#-----------------------------------------------------

# load ebird (20 mins)
# note: data are at the user-trip-species-time level
ebird <- fread(config$ebird_basic_path,
              select = c('LATITUDE', 'LONGITUDE','OBSERVATION DATE',
                        'OBSERVER ID', 'SAMPLING EVENT IDENTIFIER',
                        'PROTOCOL TYPE','DURATION MINUTES',
                        'EFFORT DISTANCE KM','ALL SPECIES REPORTED',
                        'LOCALITY', 'LOCALITY TYPE'),
              quote = "")
colnames(ebird) <- gsub('\\.', '_', tolower(make.names(colnames(ebird))))

# Clean names
ebird <- ebird %>%
  rename(lat = latitude,
        lon = longitude,
        user_id = observer_id,
        trip_id = sampling_event_identifier,
        duration = duration_minutes,
        distance = effort_distance_km,
        complete = all_species_reported)

# collapse to user-trip-time level (n = 3,463,250 trips)
ebird <- distinct(ebird, trip_id, .keep_all = TRUE)

# Dates
ebird$date <- ymd(ebird$observation_date)
ebird$year <- year(ebird$date)
ebird$month <- month(ebird$date)
ebird$yearmonth <- format(ebird$date, "%Y-%m")

#-----------------------------------------------------
# 2. Homes Coordinates
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

# Assign users to a district they live in using spatial intersection with district polygons
user_home$c_code_2011_home_real <- st_join(st_as_sf(user_home,
                                                    coords = c('lon_home_real', 'lat_home_real'),
                                                    crs = 4326), # World Geodetic System 1984
                                          dist,
                                          join = st_intersects)$c_code_2011
# Save
saveRDS(user_home, config$user_home_real_path)

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
  # note: we can come up with a better way
user <- user %>%
  group_by(user_id) %>%
  filter(distance <= quantile(distance, 0.75) + 1.5 * IQR(distance))

# Recompute home
user <- user %>%
  group_by(user_id) %>%
  summarize(lon_home = mean(lon, na.rm = TRUE),
            lat_home = mean(lat, na.rm = TRUE))

# Overlay home districts
# -------------------------------------
# Note: If gravity center is off coast home is the centroid of nearest dist
#--------------------------------------
user$c_code_2011_home <- st_join(st_as_sf(user,
                                          coords = c('lon_home', 'lat_home'),
                                          crs = 4326), # World Geodetic System 1984
                                dist,
                                join = st_intersects)$c_code_2011

# Handle homes in the ocean
# For off-coast homes, assign id of nearest district
user_na <- filter(user, is.na(c_code_2011_home))  # 1,104 users out of 49,206
user_na$c_code_2011_home <- st_join(st_as_sf(user_na,
                                            coords = c('lon_home', 'lat_home'),
                                            crs = 4326), # World Geodetic System 1984
                                    dist,
                                    join = st_nearest_feature)$c_code_2011
# note: remaining NA's are in northern Kashmir, which have no census code

# TODO: Create a table of mapping off-coast homes into nearby districts.

# TODO: What is the average distance away?
# temp <- st_distance(x = st_as_sf(user_na,
#                                 coords = c('lon_home', 'lat_home'),
#                                 crs = 4326), # World Geodetic System 1984)
#                     y = dist)

# District centroids
centroids <- as.data.frame(st_coordinates(st_centroid(dist$geometry))) %>%
  rename(lon_home = X, lat_home = Y)
centroids$c_code_2011_home <- dist$c_code_2011
user_na <- left_join(user_na[, c('user_id', 'c_code_2011_home')],
                    centroids,
                    by = 'c_code_2011_home')

# Bind to main user list
user <- rbind(filter(user, !is.na(c_code_2011_home)), user_na)
rm(list = c('centroids', 'user_na'))

# Save user list
saveRDS(user, config$user_home_impute_path)

#-----------------------------------------------------
# 3. Filter Trips
#-----------------------------------------------------
# Note: identify recreational birdwatching sessions as opposed to incidental app-recording

# Filter from eBird manual
ebird <- ebird %>%
  filter(
    complete == 1, # Completed trip
    duration <= 5 * 60, # Duration is in minutes. So selecting all observations less than 5 hours (didn't leave the app open)
    #number_observers <= 10 # Is the thought here that there should not be large parties? Large parties indicate tourists?
  )

# Select Protocol (n=2,991,609 trips)
protocol <- c('Stationary', 'Traveling')
ebird <- filter(ebird, protocol_type %in% protocol)

#-----------------------------------------------------
# 4. Identify Districts of Trip (2011 census boundary)
#-----------------------------------------------------

# Add district code (10 mins)
ebird$c_code_2011 <- st_join(st_as_sf(ebird,
                                      coords = c('lon', 'lat'),
                                      crs = 4326), # World Geodetic System 1984
                            dist,
                            join = st_intersects)$c_code_2011

# Remove out-of-bounds trips
ebird <- filter(ebird, !is.na(c_code_2011)) # n=27,605 trips out of bounds

#-----------------------------------------------------
# 5. Final processing steps
#-----------------------------------------------------

# Set distance = 0 for stationary trips
ebird <- mutate(ebird, distance = replace(distance, is.na(distance), 0))

# Add imputed home coordinates
ebird <- left_join(ebird, user, by = 'user_id')

# Save
saveRDS(ebird, config$ebird_trip_clean_path)
