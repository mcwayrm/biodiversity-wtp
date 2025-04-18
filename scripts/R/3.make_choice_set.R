# PROJECT: RUM Model
# PURPOSE: Select Choice Set for each user
####################################
# Sections:
# 1. Set Parameters
# 2. Construct Counterfactual Choice Set
# 3. Append Counterfactuals
# 4. Save Results
####################################


### SET-UP
# Load config
config_path <- file.path("scripts", "R", "0.load_config.R")
source(config_path)

# ----------------------------------------------------
# 1. Set Parameters
# ----------------------------------------------------

# Parameter settings
module <- 'cluster'       # set as 'cluster' or 'all'
samp <- 0.001             # random sample of users (scale 0-1) -- Random sample 0.1% of users (test case)
radius <- 50              # buffer radius -- kilometers -- around home (100, 50, or 20)
clust_size <- 10          # recreation area size (10km or 5km)
# CHECK: Print parameters to check what we are about to run.
writeLines(paste("Generating choice set data with parameters:",
                "\n\tModule:", module,
                "\n\tSample Proportion:", samp,
                "\n\tRadius:", radius, "km",
                "\n\tCluster Size:", clust_size, "km"))

# ----------------------------------------------------
# 2. CONSTRUCT COUTERFACTUAL CHOICE SET
# ----------------------------------------------------

# Read and filter observed trips
writeLines(paste("Loading trip data from:", config$ebird_trip_hotspots_path))
trips <- readRDS(config$ebird_trip_hotspots_path) %>%
  filter(geo_dist <= radius) # Subset trips within radius
  stopifnot(nrow(trips) == 492485) # CHECK: Subset of Obs = 492,485

# Random sample of users
set.seed(12345)
users <- trips %>%
  distinct(user_id) %>%
  sample_frac(samp)

# Subset trips to sampled users
sample <- inner_join(trips, users, by = 'user_id') # This subsets trips for the random sample

# Read and format hotspots
writeLines(paste("Loading hotspot data from:", config$hotspots_path))
cols <- c('loc_id', 'country', 'state', 'county', 'lat', 'lon', 'name', 'time', 'v9') # TODO: What is v9?
  # TODO: This is not reading in correctly. 
hotspots <- readRDS(config$hotspots_path) %>%
  setNames(cols) %>%  # Assign column names
  select(lat, lon, name)  # Keep necessary columns
  stopifnot(nrow(hotspots) == 12622) # CHECK: Obs = 12,622 hotspots

# Construct Choice Sets
cset <- choice_set( # function comes from choice_set_travel.R
  choice_df = hotspots,
  trip_df = trips,
  module = module,
  radius = radius,
  clust_size = ifelse(module == 'cluster', clust_size, NA)
)

# Subset choice sets for sampled users
cset <- inner_join(cset, users, by = 'user_id')

if (module == 'cluster') {
  # Assign cluster ID based on nearest feature
  sample <- sample %>%
    mutate(hsid = st_join(
      st_as_sf(., coords = c('lon', 'lat'), crs = 4326),
      st_as_sf(cset, coords = c('lon', 'lat'), crs = 4326),
      join = st_nearest_feature
    )$hsid)
} else {
  # Rename 'name' column for non-cluster case
  cset <- rename(cset, locality = name)
}

# ----------------------------------------------------
# 3. APPEND COUNTERFACTUALS
# ----------------------------------------------------

# Function to stack choice sets per trip
stack_cset <- function(i) { # TODO: Should this be it's own script? Actually we should have all the functions in one scripts
  
  # Extract user ID
  user <- sample$user_id[sample$trip_id == i]
  
  # Extract user-specific choice set
  user_cset <- filter(cset, user_id == user)

  # Remove observed site from counterfactual set
  if (module == "cluster") {
    id <- sample$hsid[sample$trip_id == i]
    user_cset <- filter(user_cset, hsid != id)
  } else {
    trip_lat <- sample$lat[sample$trip_id == i]
    trip_lon <- sample$lon[sample$trip_id == i]
    user_cset <- filter(user_cset, lat != trip_lat & lon != trip_lon)
  }

  # Ensure `user_cset` is always a data frame
  if (nrow(user_cset) == 0) {
    user_cset <- as.data.frame(matrix(ncol = ncol(sample), nrow = 0))
    colnames(user_cset) <- colnames(sample)
  }

  # Extract the trip row
  row <- filter(sample, trip_id == i)

  # Standardize column types before merging
  user_cset <- mutate_all(user_cset, as.character)
  row <- mutate_all(row, as.character)

  # Bind observed trip with counterfactuals
  tryCatch(
    {
      bind_rows(row, user_cset)
    },
    error = function(e) {
      message("Error in bind_rows for trip_id: ", i)
      message("row dims: ", paste(dim(row), collapse = " x "))
      message("user_cset dims: ", paste(dim(user_cset), collapse = " x "))
      stop(e)
    }
  )
}

# Execute choice stack with progress bar
triplist <- sample$trip_id
writeLines("Creating choice set stack")
master_list <- pblapply(triplist, stack_cset)


# ----------------------------------------------------
# 4. Save Counterfactual Data Set
# ----------------------------------------------------

# Convert list to data frame
master <- as.data.frame(rbindlist(master_list, fill = TRUE))

# Clean dataset
master <- master %>%
  mutate(choice = ifelse(is.na(trip_id), 0, 1)) %>%
  select(-c(starts_with('lat'), starts_with('lon'), duration,
            distance, complete)) %>%
  fill(trip_id, date, year, month, yearmonth, c_code_2011_home)

# Save results
file_name <- paste0("master_cs", radius, "km_clust_", clust_size, "km.rds")
output_path <- do.call(file.path, c(as.list(config$clean_data_dir), file_name))
saveRDS(master, output_path)

writeLines(paste("Finished! File saved at: ", output_path))
