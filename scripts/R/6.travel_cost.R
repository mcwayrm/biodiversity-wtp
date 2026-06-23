# PROJECT: RUM Model
# PURPOSE: Estimate travel cost for each counterfactual
# AUTHOR: Jovin Lasway


### SET-UP
setwd("C:/Users/laswa/OneDrive/Documents/GitHub/biodiversity-wtp")

config_path <- file.path("scripts", "R", "utils_config.R")
source(config_path)

# ----------------------------------------
# Load district shapefile and extract centroids
# ----------------------------------------

district_path <- file.path(
  input_data_dir,
  "districts",
  "district-2011",
  "district-2011.shp"
)

dist <- st_read(district_path) %>%
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

master_cs50km_path <- file.path(
  input_data_dir,
  "clean",
  "master_cs50km_voronoi10km_sample1_attributes_lagged.rds"
)

master_cs50km <- readRDS(master_cs50km_path)

master_cs50km <- master_cs50km %>%
  left_join(dist_centroids, by = "c_code_2011") %>%
  mutate(year = as.integer(year)) %>%
  filter(year >= 2011 & year <= 2024)

min(master_cs50km$year, na.rm = TRUE)
max(master_cs50km$year, na.rm = TRUE)
# ----------------------------------------
# Load and process GDP data
# ----------------------------------------

gdp_path <- file.path(
  input_data_dir,
  "gdp_0_25deg",
  "final_GDP_0_25deg_postadjust_pop_density.csv"
)

gdp_all <- read_csv(gdp_path)

gdp_india <- gdp_all %>%
  filter(iso == "IND", year >= 2011, year <= 2024) %>%
  mutate(year = as.integer(year))

range(master_cs50km$year, na.rm = TRUE)
range(gdp_india$year, na.rm = TRUE)

gdp_india <- gdp_india %>%filter(year >= 2015 & year <= 2024) # Conditioning to match choice set data time range

# ---- 3. CPI DEFLATOR  (FRED — INDCPIALLAINMEI, base 2015=100) ---------------
#
# We deflate nominal GDP per capita to real 2015 INR so that income/wages are
# comparable in constant-purchasing-power terms across the panel.
# Since the CPI series is already indexed to 2015 = 100, the anchor is CPI_2015.
# Deflation formula: real_t = nominal_t * (CPI_2015 / CPI_t)

cpi_path <- file.path(
  input_data_dir,
  "CPI",
  "INDCPIALLAINMEI.xlsx"
)

cpi_raw <- readxl::read_excel(
  cpi_path,
  sheet = "Annual"
)

cpi_df <- cpi_raw %>%
  mutate(year = lubridate::year(as.Date(observation_date))) %>%
  rename(cpi = INDCPIALLAINMEI) %>%
  dplyr::select(year, cpi) %>%
  group_by(year) %>%
  summarise(cpi = mean(cpi, na.rm = TRUE), .groups = "drop")

cpi_2015 <- cpi_df$cpi[cpi_df$year == 2015]


# ---- 4. EXCHANGE RATE  (USD to INR) ------------------------------------------
#
# GDP is measured in current USD in the GDP file, so first convert current USD
# to nominal INR using year-specific INR/USD exchange rates.

exchange_df <- tibble(
  year = 2015:2024,
  usd_to_inr = c(
    64.15,  # 2015
    67.19,  # 2016
    65.12,  # 2017
    68.43,  # 2018
    70.41,  # 2019
    74.10,  # 2020
    73.93,  # 2021
    77.44,  # 2022
    82.57,  # 2023
    83.50   # 2024
  )
)

# ---- 5. CONVERT GDP TO NOMINAL INR -------------------------------------------
#
# Convert current USD GDP per capita into nominal INR.

gdp_india <- gdp_india %>%
  left_join(exchange_df, by = "year") %>%
  mutate(
    gdppc_nominal_INR = predicted_GCP_current_USD * usd_to_inr
  )


# ---- 6. DEFLATE TO REAL 2015 INR ---------------------------------------------
#
# Convert nominal INR GDP per capita into real 2015 INR.

gdp_india <- gdp_india %>%
  left_join(cpi_df, by = "year") %>%
  mutate(
    gdppc_real_2015_INR =
      round(gdppc_nominal_INR * (cpi_2015 / cpi), 2)
  )


# ----------------------------------------
# Convert GDP data to spatial features
# ----------------------------------------

gdp_india_sf <- st_as_sf(
  gdp_india,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)


# ----------------------------------------
# Match each home location-year to nearest GDP grid cell
# ----------------------------------------

master_cs50km_sf <- st_as_sf(
  master_cs50km,
  coords = c("lon_home", "lat_home"),
  crs = 4326,
  remove = FALSE
)

master_cs50km_list <- split(master_cs50km_sf, master_cs50km_sf$year)

matched_list <- lapply(names(master_cs50km_list), function(yr) {
  
  yr <- as.integer(yr)
  
  master_year <- master_cs50km_list[[as.character(yr)]]
  
  gdp_year <- gdp_india_sf %>%
    filter(year == yr)
  
  nearest_indices <- st_nearest_feature(master_year, gdp_year)
  
  master_year %>%
    mutate(
      gdppc_real_2015_INR =
        gdp_year$gdppc_real_2015_INR[nearest_indices]
    )
})

master_cs50km <- bind_rows(matched_list) %>%
  st_drop_geometry()

# --------------------------------------------------------
# ASSUMPTIONS
# --------------------------------------------------------

# Value of time assumed as 1/3 of wage (Jayalath et al., 2023)
time_value_fraction <- 1/3

# Based on 250 working days x 8 hours/day
work_hours <- 2000

# Assumed average rural/urban travel speed consistent with
# Ministry of Road Transport and Highways (MoRTH) study
travel_speed_kmph <- 30


# --------------------------------------------------------
# Load year-specific driving cost
# --------------------------------------------------------
# Driving cost per km varies by year and is measured in real 2021 INR/km.
# These values replace the earlier fixed assumption of INR 7.5/km.

driving_cost_path <- file.path(
  input_data_dir,
  "driving_cost",
  "driving_cost.rds"
)

driving_cost_df <- readRDS(driving_cost_path) %>%
  mutate(
    year = as.integer(year),
    driving_cost = as.numeric(driving_cost)
  ) %>%
  filter(year >= 2015 & year <= 2024)

# --------------------------------------------------------
# Calculate travel cost
# --------------------------------------------------------

master_cs50km <- master_cs50km %>%
  mutate(
    geo_dist_cluster = as.numeric(geo_dist_cluster),
    
    # GDP per capita at home location converted to hourly wage
    hourly_wage = gdppc_real_2015_INR / work_hours
  ) %>%
  left_join(driving_cost_df, by = "year") %>%
  mutate(
    travel_time_hours = geo_dist_cluster / travel_speed_kmph,
    
    time_cost =
      2 * time_value_fraction * hourly_wage * travel_time_hours,
    
    fuel_cost =
      geo_dist_cluster * driving_cost,
    
    travel_cost_combined =
      time_cost + fuel_cost,
    
    log_travel_cost_combined =
      log1p(travel_cost_combined)
  )


# --------------------------------------------------------
# Checks
# --------------------------------------------------------

range(master_cs50km$year, na.rm = TRUE)

master_cs50km %>%
  count(year) %>%
  arrange(year)

master_cs50km %>%
  filter(is.na(driving_cost)) %>%
  count(year)

master_cs50km %>%
  filter(is.na(gdppc_real_2015_INR)) %>%
  count(year)

summary(master_cs50km$gdppc_real_2015_INR)
summary(master_cs50km$travel_cost_combined)



# --------------------------------------------------------
# Plot and save travel cost distribution
# --------------------------------------------------------

x <- master_cs50km$travel_cost_combined
x <- x[!is.na(x)]

png(
  filename = file.path(
    input_data_dir,
    "driving_cost",
    "travel_cost_distribution.png"
  ),
  width = 2400,
  height = 1800,
  res = 300
)

hist(
  x,
  breaks = 50,
  probability = TRUE,
  col = "grey85",
  border = "white",
  main = "",
  xlab = "Travel Cost (real 2015 INR)",
  ylab = "Density",
  cex.lab = 1.4,
  cex.axis = 1.2
)

dens <- density(
  x,
  bw = "nrd0"
)

lines(
  dens,
  col = "#1F78B4",
  lwd = 2
)

abline(
  v = mean(x),
  col = "#E31A1C",
  lwd = 2,
  lty = 2
)

legend(
  "topleft",
  legend = c(
    "Kernel Density",
    paste0("Mean = ", round(mean(x), 2))
  ),
  col = c("#1F78B4", "#E31A1C"),
  lwd = 2,
  lty = c(1, 2),
  bty = "n",
  cex = 1.1
)

dev.off()

cat(
  "Figure saved to:\n",
  file.path(
    input_data_dir,
    "driving_cost",
    "travel_cost_distribution.png"
  ),
  "\n"
)