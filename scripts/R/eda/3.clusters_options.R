# PROJECT: RUM Model
# PURPOSE: Select Choice Set for each user (counterfactuals)
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



# # Generate cleaned hotspot list (grouping the hotspots into clusters)
# writeLines(paste("Processing choice set with module:", module))
# hs <- rec_clust(choice_df, module = module, clust_size = ifelse(module == 'cluster', clust_size, NA)) %>% # Command comes from hotspot_clustering.R
#     mutate(lat2 = lat, lon2 = lon)


# Function: rec_clust
## Inputs: 
### df: a data frame - hotspots
### clust_size: cluster size (km)

## Outputs:
## df: dataframe of hotspots with rec area ID's

# Read and format hotspots
writeLines(paste("Loading hotspot data from:", config$hotspots_path))
cols <- c('loc_id', 'country', 'state', 'county', 'lat', 'lon', 'name', 'time', 'v9') # TODO: What is v9?
  # TODO: This is not reading in correctly. 
hotspots <- readRDS(config$hotspots_path) %>%
  setNames(cols) %>%  # Assign column names
  select(lat, lon, name)  # Keep necessary columns
  stopifnot(nrow(hotspots) == 12622) # CHECK: Obs = 12,622 hotspots


# Load district map
india_dist <- st_read(file.path("data", "shp", "district-2011", "district-2011.shp"))  

# add district code to hostpots
hotspots$c_code_2011 <- st_join(st_as_sf(hotspots, coords=c('lon', 'lat'), crs=4326), 
                          india_dist, join = st_intersects)$c_code_11

# Remove off-coast hotspots
hotspots <- filter(hotspots, !is.na(c_code_2011))

# Drop duplicates
#---------------------------------------------------
# Some hotspots w same lat-lon entered twice
# Some hotspots have same name, coords slightly off
#----------------------------------------------------
hotspots <- distinct(hotspots, lat, lon, .keep_all=T) # drop duplicate coordinates 
hotspots <- distinct(hotspots, c_code_2011, name, .keep_all=T) # drop duplicate names w/n district

#------------------------------------------------------
# Heirarchical Clustering of Hotspots
#------------------------------------------------------

# TESTING
# Convert tibble to sf POINT object
hotspots_sf <- hotspots %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(32643)  # Project to UTM zone appropriate for India

# HCLUST 
coords <- st_coordinates(hotspots_sf)
d <- dist(coords)
hc <- hclust(d, method = "ward.D2")
hotspots_sf$hclust_cluster <- cutree(hc, k = 10)  # try k=10 or adjust

#DBSCAN 
# install.packages("dbscan")
library(dbscan)
# Tune eps — distance threshold in meters; minPts = minimum core points
db <- dbscan(coords, eps = 2000, minPts = 10)  # try eps = 2000 meters
hotspots_sf$dbscan_cluster <- as.factor(db$cluster)  # 0 = noise

# PLOT
library(ggplot2)
p1 <- ggplot(hotspots_sf) +
  geom_sf(aes(color = factor(hclust_cluster)), size = 0.5) +
  labs(title = "Hierarchical Clustering", color = "Cluster") +
  theme_minimal()
p2 <- ggplot(hotspots_sf) +
  geom_sf(aes(color = dbscan_cluster), size = 0.5) +
  labs(title = "DBSCAN Density Clustering", color = "Cluster") +
  theme_minimal()
library(patchwork)
p1 + p2

# install.packages("concaveman")
library(concaveman)


# Use projected coordinates for spatial clustering
hotspots_proj <- st_transform(hotspots_sf, crs = 32643)  # UTM Zone 43N as an example for India

# Re-run DBSCAN with a larger eps (e.g., 500 meters)
db <- dbscan(st_coordinates(hotspots_proj), eps = 10000, minPts = 5)

# Add new cluster ID
hotspots_proj$dbscan_cluster <- db$cluster
table(hotspots_proj$dbscan_cluster)


# Remove noise
clustered_points <- hotspots_proj %>% 
  filter(dbscan_cluster != 0)

clusters_split <- clustered_points %>%
  group_split(dbscan_cluster)

cluster_polys <- map_dfr(clusters_split, function(cluster_sf) {
  cluster_id <- unique(cluster_sf$dbscan_cluster)
  poly_sf <- concaveman(cluster_sf)
  poly_sf$dbscan_cluster <- cluster_id
  poly_sf
})

ggplot() +
  geom_sf(data = cluster_polys, aes(fill = dbscan_cluster), alpha = 0.4, color = "black") +
  geom_sf(data = hotspots_sf, aes(color = dbscan_cluster), size = 0.5) +
  theme_minimal() +
  labs(title = "Polygons Around DBSCAN Clusters", fill = "Cluster")
  labs(title = "DBSCAN Cluster Polygons")



# Distance Matrix (5 mins)
writeLines('Computing distance matrix between hotspots...')
distm <- st_distance(st_as_sf(hotspots, coords = c('lon', 'lat'), crs=4326))

# Construct heirachical dendogram
hc <- hclust(as.dist(distm), method="complete")

# Cut hc tree at specified distance (in meters)
# Assigns hotspot cluster id to each hotspot
hotspots$cluster_id <- cutree(hc, h=clust_size*1000)

# Save full hotspots with cluster_id
saveRDS(hotspots, file.path("data", "intermediate", "hotspots", paste0("hotspots_all_", clust_size, "km.rds")))

# Cluster centroids
hotspots_cluster <- hotspots %>%
  group_by(cluster_id) %>%
  summarize(lat = mean(lat), lon = mean(lon))

# District in centroid of recreation area
hotspots_cluster$c_code_2011 <- st_join(st_as_sf(hotspots, 
                                    coords=c('lon', 'lat'), 
                                    crs=4326), 
                            india_dist, 
                            join = st_intersects)$c_code_11

# Save intermediate data
saveRDS(hotspots_cluster, file.path("data", "intermediate", "hotspots", paste0("hotspots_clust_", clust_size, "km.rds")))






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
