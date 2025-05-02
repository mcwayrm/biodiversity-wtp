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
  # CHECK: Obs = 641 

#-----------------------------------------------------
# 1. Pre-Processs
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
  stopifnot(nrow(ebird) == 58563093) # CHECK: Obs = 58,563,093 trips
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

# collapse to user-trip-time level
ebird <- distinct(ebird, trip_id, .keep_all = TRUE)
  stopifnot(nrow(ebird) == 3463250) # CHECK: Obs = 3,463,250 trips

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
  stopifnot(nrow(user_home) == 393) # CHECK: Obs = 393 users homes
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
  stopifnot(nrow(user) == 1317233) # CHECK: Obs = 1,317,233 trips
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
  stopifnot(nrow(user) == 1214152) # CHECK: Obs = 1,214,152 trips
# Note: 103,081 trips removed (7.8%)

# Recompute home
user <- user %>%
  group_by(user_id) %>%
  summarize(lon_home = mean(lon, na.rm = TRUE),
            lat_home = mean(lat, na.rm = TRUE))
  stopifnot(nrow(user) == 49206) # CHECK: Obs = 49,206 homes

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
user_na <- filter(user, is.na(c_code_2011_home))  
  stopifnot(nrow(user_na) == 1104) # CHECK: 1,104 users out of 49,206
# Note: 2.2% of users have homes off-coast
user_na$c_code_2011_home <- st_join(st_as_sf(user_na,
                                            coords = c('lon_home', 'lat_home'),
                                            crs = 4326), # World Geodetic System 1984
                                    dist,
                                    join = st_nearest_feature)$c_code_2011
  sum(is.na(user_na)) # CHECK: How many NAs remain == 47
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
  stopifnot(nrow(ebird) == 3008200) # CHECK: Obs = 3,008,200 trips

# Select Protocol (n=2,991,609 trips)
protocol <- c('Stationary', 'Traveling')
ebird <- filter(ebird, protocol_type %in% protocol)
  stopifnot(nrow(ebird) == 2991609) # CHECK: Obs = 2,991,609 trips
  # Note: 1,659 trips removed (0.05%)

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
ebird <- filter(ebird, !is.na(c_code_2011))
  stopifnot(nrow(ebird) == 2964004) # CHECK: Obs = 2,964,004 trips
  # Note: 7,605 trips removed (0.25%)


#-----------------------------------------------------
# 5. Sample Selection: Restrict to users with at least 1 trip every 6 months 
#-----------------------------------------------------

# 1. Define parameters
interval_months <- 3    # 2 for bi-monthly, 3 for quarterly, 6 for semi-annual
min_years_active <- 8

# 2. Compute time periods: month 1–3 = period 1, 4–6 = period 2, etc.
monthly_activity <- ebird %>%
  mutate(
    year = year(date),
    month = month(date),
    period = ceiling(month / interval_months)
  ) %>%
  distinct(user_id, year, period)

# 3. Filter users who are active in every period within a year
# Total number of periods in a year
n_periods <- ceiling(12 / interval_months)

# Count how many unique periods each user was active in per year
active_per_year <- monthly_activity %>%
  group_by(user_id, year) %>%
  summarize(n_periods_active = n_distinct(period), .groups = "drop") %>%
  filter(n_periods_active == n_periods)

# 4. Keep users with at least N qualifying years
active_users <- active_per_year %>%
  count(user_id, name = "n_years") %>%
  filter(n_years >= min_years_active)

# 5. Subset the main data
ebird <- ebird %>%
  semi_join(active_users, by = "user_id")
  

#-----------------------------------------------------
# 6. Final processing steps
#-----------------------------------------------------

# Set distance = 0 for stationary trips
ebird <- mutate(ebird, distance = replace(distance, is.na(distance), 0))

# Add imputed home coordinates
ebird <- left_join(ebird, user, by = 'user_id')

# Save
saveRDS(ebird, config$ebird_trip_clean_path)
