# PROJECT: RUM Model
# PURPOSE: Estimate travel cost for each counterfactual
# author: Jovin Lasway


### SET-UP
# Load config
config_path <- file.path("scripts", "R", "0.load_config.R")
source(config_path)


# ----------------------------------------
# Load district shapefile and extract centroids
# ----------------------------------------
dist <- st_read(config$district_path) %>%
  select(c_code_11) %>%
  rename(c_code_2011 = c_code_11)

dist_centroids <- dist %>%
  st_centroid() %>%
  mutate(
    lon_dist = st_coordinates(geometry)[, 1],
    lat_dist = st_coordinates(geometry)[, 2]
  ) %>%
  st_drop_geometry()

# ----------------------------------------
# Load master dataset and merge in home coordinates
# ----------------------------------------
master_cs50km_path <- file.path("data", "clean", "master_cs50km_voronoi10km_sample1_attributes_lagged.rds")
master_cs50km <- readRDS(master_cs50km_path)

master_cs50km <- master_cs50km %>%
  left_join(dist_centroids, by = "c_code_2011")

# ----------------------------------------
# Load and process GDP data
# ----------------------------------------
gdp_path <- file.path("data", "shp", "gdp_0_25deg", "final_GDP_0_25deg_postadjust_pop_density.csv")
gdp_all <- read_csv(gdp_path)

gdp_india <- gdp_all %>%
  filter(iso == "IND", year >= 2014, year <= 2024)

# Convert GDP data to spatial features
gdp_india_sf <- st_as_sf(gdp_india, coords = c("longitude", "latitude"), crs = 4326)

# Keep one point per grid cell for year 2020
gdp_summary <- gdp_india_sf %>%
  filter(year == 2020) %>%
  group_by(cell_id) %>%
  summarize(geometry = st_centroid(st_union(geometry)), .groups = "drop")

# Merge GDP per capita values into centroids
gdp_centroids <- left_join(
  gdp_summary,
  gdp_india[gdp_india$year == 2020, c("cell_id", "predicted_GCP_current_USD")],
  by = "cell_id"
) %>%
  rename(gdppc = predicted_GCP_current_USD)

# ----------------------------------------
# Match each home location to nearest GDP centroid
# ----------------------------------------
master_cs50km_sf <- st_as_sf(master_cs50km, coords = c("lon_home", "lat_home"), crs = 4326)

# Find nearest GDP centroid for each home location
nearest_indices <- st_nearest_feature(master_cs50km_sf, gdp_centroids)

# GDP per capita for each observation from nearest centroid
gdppc_values <- gdp_centroids$gdppc[nearest_indices]


# --------------------------------------------------------
# ASSUMPTIONS
# --------------------------------------------------------

# Value of time assumed as 1/3 of wage (Jayalath et al., 2023)
time_value_fraction <- 1/3           
# Based on 250 working days x 8 hours/day
work_hours <- 2000                   
# Assumed average rural/urban travel speed consistent with MoRTH road studies - Ministry of Road Transport and Highways 
travel_speed_kmph <- 30              
# INR/km
driving_cost_per_km <- 7.5             

# We adopt a per-kilometer driving cost of INR 7.5, consistent with estimates for vehicle running costs in India by excluding the fixed cost based on insurance, registration, and age-related depreciation.
# According to the Times of India (Feb 14, 2024), small petrol cars cost about INR 7-8/km to operate.
# References:
# - Times of India. (2024, Feb 14). *EVs get big price cuts: Prices, running cost of Petrol vs Electric cars. https://timesofindia.indiatimes.com/auto/cars/evs-get-big-price-cuts-prices-running-cost-of-petrol-vs-electric-cars/articleshow/107687660.cms

# --------------------------------------------------------
# EXTRACT GDP-based hourly wage
# --------------------------------------------------------
# GDP per capita at home
gdppc_values <- gdp_centroids$gdppc[nearest_indices]      
# INR/hour
hourly_wage <- gdppc_values / work_hours                  

# --------------------------------------------------------
# PREP DISTANCE VARIABLE (ensure numeric)
# --------------------------------------------------------
master_cs50km$geo_dist_cluster <- as.numeric(master_cs50km$geo_dist_cluster)

# --------------------------------------------------------
# TRAVEL TIME AND COST CALCULATION
# --------------------------------------------------------
master_cs50km <- master_cs50km %>%
  mutate(
    # Estimate one-way travel time (hours) = distance (km) / speed (km/h)
    travel_time_hours = geo_dist_cluster / travel_speed_kmph,
    # Calculate round-trip time cost = 2 x (1/3 of wage) x travel time
    time_cost = 2 * time_value_fraction * hourly_wage * travel_time_hours,
    # One-way driving cost = distance x cost per km
    fuel_cost = geo_dist_cluster * driving_cost_per_km,
    # Total travel cost = time cost + driving cost
    travel_cost_combined = time_cost + fuel_cost,
    # Natural log of travel cost (for regression), avoids log(0) error
    log_travel_cost_combined = log1p(travel_cost_combined)
  )

# ----------------------------------------
# Checking distribution
# ----------------------------------------

summary(master_cs50km$travel_cost_combined)
hist(master_cs50km$travel_cost_combined, breaks = 50,
    main = "Travel Cost", xlab = "Cost (INR)")

# Save result
output_path <- file.path("data", "clean", "master_cs50km_costs.rds")
saveRDS(master_cs50km, output_path)
