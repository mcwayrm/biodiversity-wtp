# PROJECT: eBird Valuation
# PURPOSE: Download and process ERA5 temperature and precipitation data for India
##################################################################################

### SET-UP
# -----------------------------
# NOTE:
# You must register at https://cds.climate.copernicus.eu/
# and declare your API key using `wf_set_key(key = "abcd1234-foo-bar-98765431-XXXXXXXXXX")`

# Load config (set working directories, credentials, etc.)
config_path <- file.path("scripts", "R", "0.load_config.R")
source(config_path)

# Additional packages
library(ecmwfr)  # For Copernicus CDS access: https://bluegreen-labs.github.io/ecmwfr/
library(terra)   # For raster handling

### DOWNLOAD + PROCESS FUNCTION
# -----------------------------
download_and_process_era5 <- function(variable, output_subdir, convert_kelvin = FALSE, convert_precip_mm = FALSE) {
  
  # Define ERA5 request
  request <- list(
    dataset_short_name = "reanalysis-era5-land-monthly-means",
    product_type = "monthly_averaged_reanalysis",
    variable = variable,
    year = as.character(2015:2024),
    month = sprintf("%02d", 1:12),
    time = "00:00",
    area = c(35, 68, 6, 97),  # India bounding box: N, W, S, E
    format = "netcdf",
    download_format = "unarchived",
    target = paste0("era5_", variable, "_monthly_2015_2024.nc")
  )

  # Submit request and download
  file <- wf_request(
    request = request,
    transfer = TRUE,
    path = "."
  )

  # Read in raster data
  r <- rast(file)

  # Extract date strings from layer names (UNIX timestamps)
  timestamps <- as.numeric(gsub(".*_valid_time=", "", names(r)))
  dates <- as.POSIXct(timestamps, origin = "1970-01-01", tz = "UTC")
  date_strings <- format(dates, "%Y_%m")

  # Create output directory
  dir_path <- file.path("data", "raster", output_subdir)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)

  # Process and write each layer
  for (i in 1:nlyr(r)) {
    layer <- r[[i]]

    if (convert_kelvin) {
      layer <- layer - 273.15  # Kelvin to Celsius
    }
    if (convert_precip_mm) {
      layer <- layer * 1000    # meters to mm
    }

    out_file <- file.path(dir_path, paste0("era5_", variable, "_", date_strings[i], ".tif"))
    writeRaster(layer, filename = out_file, filetype = "GTiff", overwrite = TRUE)
  }
  cat("Download complete. Saved", nlyr(r), "raster files to", dir_path, "\n")

  # Clean up temp netCDF file
  file.remove(file)
}


### TEMPERATURE: 2m (Celsius)
# -----------------------------
download_and_process_era5(
  variable = "2m_temperature",
  output_subdir = "era5_2m_temperature",
  convert_kelvin = TRUE
)


### PRECIPITATION: Total (mm/month)
# -----------------------------
download_and_process_era5(
  variable = "total_precipitation",
  output_subdir = "era5_total_precipitation",
  convert_precip_mm = TRUE
)