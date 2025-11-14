# scripts/R/utils_config.R
##########################################
# Path and Config Helpers
##########################################

# Clear objects
rm(list = ls())

# Packages used across the pipeline
packages <- c(
  "yaml", "tidyverse", "arrow", "here", "lwgeom", "logitr", "fixest",
  "sf", "config", "pacman", "data.table", "lubridate", "units",
  "pbapply", "cluster", "factoextra", "terra", "vegan"
)

# Try to load via pacman if available; otherwise try library() for installed pkgs
if (requireNamespace("pacman", quietly = TRUE)) {
  pacman::p_load(packages, character.only = TRUE, install = FALSE)
} else {
  invisible(lapply(packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Package not available: ", pkg)
    } else {
      library(pkg, character.only = TRUE)
    }
  }))
}

# Set base data dir
# Detect OS and set username
os_type <- .Platform$OS.type  # "windows" or "unix"
username <- if (os_type == "windows") Sys.getenv("USERNAME") else Sys.getenv("USER")
if (username == "mbraaksma") {
  message("Setting base data dir for user: ", username)
  input_data_dir <- "/Users/mbraaksma/Files/base_data/biodiversity-wtp/raw"
} else if (username == "ryanm") {
  message("Setting base data dir for user: ", username)
  input_data_dir <- file.path("..", "base-data")
} else {
  message("Setting base data dir to default 'data/raw' folder")
  input_data_dir <- here("data", "raw")
}

# Input dir must follow structure: 
# input_data_dir/
# ├── districts/
# │   ├── district-2011/
# │   │   ├── district-2011.shp
# ├── ebird/
# │   ├── ebd_IN_201501_202412_relDec-2024/
# │   │   ├── ebd_IN_201501_202412_relDec-2024.txt
# │   ├── hotspots.rds
# ├── era5_2m_temperature/era5_2m_temperature_2015_01.tif
# │   ├── era5_2m_temperature_2015_01.tif
# │   ├── . . .
# │   ├── era5_2m_temperature_2024_12.tif
# ├── era5_total_precipitation/
# │   ├── era5_total_precipitation_2015_01.tif
# │   ├── . . .
# │   ├── era5_total_precipitation_2024_12.tif
# ├── gdp/
# │   ├── final_GDP_0_25deg_postadjust_pop_density.csv
# ├── modis_vcf/
# │   ├── MOD44B.061_Percent_Tree_Cover_doy2015065_aid0001.tif
# │   ├── . . .
# │   ├── MOD44B.061_Percent_Tree_Cover_doy2023065_aid0001.tif
# ├── protected_areas/
# │   ├── 04_MainlandPAsShapefile/
# │   │   ├── MainlandPAs_HolesFilled100k_FewRivRemv.shp
