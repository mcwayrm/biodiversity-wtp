# PROJECT: eBird Valuation
# PURPOSE: Download and process ERA5 temperature and precipitation data for India
##################################################################################

### SET-UP
# -----------------------------
# NOTE:
# You must register at https://cds.climate.copernicus.eu/
# and declare your API key using `wf_set_key(key = "abcd1234-foo-bar-98765431-XXXXXXXXXX")`
# Requires additional package not in config: ecmwfr (https://bluegreen-labs.github.io/ecmwfr/)

# Load config (set working directories, credentials, etc.)
config_path <- file.path("scripts", "R", "0.load_config.R")
source(config_path)

# Additional packages
library(ecmwfr)  # For Copernicus CDS access

### NaN CHECKING FUNCTIONS
# -----------------------------
check_nan_values <- function(layer, layer_name, variable, date_string, reference_mask = NULL) {
  # Count total cells
  total_cells <- ncell(layer)
  
  # Count NaN/NA values
  nan_count <- sum(is.na(values(layer, na.rm = FALSE)))
  
  # Calculate percentage
  nan_percentage <- (nan_count / total_cells) * 100
  
  # If we have a reference mask, check for anomalous NaN patterns
  anomalous_nan <- FALSE
  anomalous_count <- 0
  
  if (!is.null(reference_mask)) {
    # Count NaN values in areas that should have data (based on reference)
    layer_vals <- values(layer, na.rm = FALSE)
    ref_vals <- values(reference_mask, na.rm = FALSE)
    
    # Areas where reference has data but current layer doesn't
    anomalous_locations <- is.na(layer_vals) & !is.na(ref_vals)
    anomalous_count <- sum(anomalous_locations, na.rm = TRUE)
    anomalous_nan <- anomalous_count > 0
    
    if (anomalous_nan) {
      cat("ANOMALY DETECTED:", layer_name, "has", anomalous_count, 
          "unexpected NaN values in areas that should have data\n")
    }
  }
  
  # Create summary
  summary_info <- list(
    layer_name = layer_name,
    variable = variable,
    date = date_string,
    total_cells = total_cells,
    nan_count = nan_count,
    nan_percentage = round(nan_percentage, 2),
    has_nan = nan_count > 0,
    anomalous_nan = anomalous_nan,
    anomalous_count = anomalous_count
  )
  
  return(summary_info)
}

create_nan_report <- function(nan_summaries, variable, save_csv=FALSE) {
  # Create data frame from summaries
  df <- do.call(rbind.data.frame, nan_summaries)
  
  
  # Calculate statistics
  total_layers <- nrow(df)
  layers_with_anomalous_nan <- sum(df$anomalous_nan, na.rm = TRUE)
  consistent_nan_percentage <- df$nan_percentage[1]  # Should be consistent for land data
  
  cat("\n=== DATA QUALITY SUMMARY FOR", toupper(variable), "===\n")
  cat("Total layers processed:", total_layers, "\n")
  cat("Expected NaN percentage (ocean/no-data areas):", consistent_nan_percentage, "%\n")
  cat("Layers with anomalous NaN patterns:", layers_with_anomalous_nan, "\n")
  
  if (layers_with_anomalous_nan > 0) {
    cat("Total anomalous NaN values found:", sum(df$anomalous_count, na.rm = TRUE), "\n")
    cat("These may indicate data quality issues requiring investigation.\n")
  } else {
    cat("All NaN patterns appear consistent (likely ocean/no-data areas).\n")
  }
  
  # Check for variation in NaN percentages (could indicate inconsistent coverage)
  nan_range <- range(df$nan_percentage)
  if (diff(nan_range) > 1) {  # More than 1% variation
    cat("WARNING: NaN percentages vary between", nan_range[1], "% and", nan_range[2], 
        "% - check for inconsistent spatial coverage\n")
  }

  # Write summary report
  if (save_csv==TRUE) {
    report_file <- file.path("data", "raster", paste0("nan_report_", variable, ".csv"))
    write.csv(df, report_file, row.names = FALSE)
    cat("Report saved to:", report_file, "\n")
  }
  
  cat("=============================================\n\n")
}

### DOWNLOAD + PROCESS FUNCTION
# -----------------------------------------
download_and_process_era5 <- function(variable, output_subdir, convert_kelvin = FALSE, 
                                    convert_precip_mm = FALSE, nan_threshold = 10) {
  
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
  
  # Initialize list to store NaN summaries
  nan_summaries <- list()
  problematic_layers <- character()
  reference_mask <- NULL
  
  # Process and write each layer
  for (i in 1:nlyr(r)) {
    layer <- r[[i]]
    layer_name <- names(r)[i]
    
    # Use first layer as reference mask for anomaly detection
    if (i == 1) {
      reference_mask <- layer
    }
    
    # Check for NaN values BEFORE processing
    nan_summary_raw <- check_nan_values(layer, paste0(layer_name, "_raw"), 
                                       variable, date_strings[i], reference_mask)
    
    # Apply conversions
    if (convert_kelvin) {
      layer <- layer - 273.15  # Kelvin to Celsius
    }
    if (convert_precip_mm) {
      layer <- layer * 1000  # meters to mm
    }
    
    # Check for NaN values AFTER processing
    nan_summary_processed <- check_nan_values(layer, paste0(layer_name, "_processed"), 
                                            variable, date_strings[i], reference_mask)
    
    # Store summaries
    nan_summaries[[length(nan_summaries) + 1]] <- nan_summary_processed
    
    # Check for anomalous NaN patterns instead of absolute threshold
    if (nan_summary_processed$anomalous_nan) {
      problematic_layers <- c(problematic_layers, 
                            paste0(date_strings[i], " (", nan_summary_processed$anomalous_count, " anomalous NaN)"))
    }
    
    # Also check if NaN percentage deviates significantly from expected
    if (i > 1) {
      expected_nan_pct <- nan_summaries[[1]]$nan_percentage
      current_nan_pct <- nan_summary_processed$nan_percentage
      
      if (abs(current_nan_pct - expected_nan_pct) > 2) {  # More than 2% difference
        problematic_layers <- c(problematic_layers, 
                              paste0(date_strings[i], " (coverage anomaly: ", 
                                   current_nan_pct, "% vs expected ", expected_nan_pct, "%)"))
      }
    }
    
    # Write raster file
    out_file <- file.path(dir_path, paste0("era5_", variable, "_", date_strings[i], ".tif"))
    writeRaster(layer, filename = out_file, filetype = "GTiff", overwrite = TRUE)
  }
  
  # Generate NaN report
  create_nan_report(nan_summaries, variable)
  
  # Final summary
  cat("Download complete. Saved", nlyr(r), "raster files to", dir_path, "\n")
  
  if (length(problematic_layers) > 0) {
    cat("\nLAYERS WITH DATA QUALITY ISSUES:\n")
    for (layer in problematic_layers) {
      cat("-", layer, "\n")
    }
    cat("Consider reviewing these layers for data quality issues.\n")
  } else {
    cat("All layers show consistent data patterns - no anomalies detected.\n")
  }
  
  # Clean up temp netCDF file
  file.remove(file)
  
  # Return summary information
  return(list(
    total_layers = nlyr(r),
    nan_summaries = nan_summaries,
    problematic_layers = problematic_layers
  ))
}

### TEMPERATURE: 2m (Celsius)
# -----------------------------
temp_results <- download_and_process_era5(
  variable = "2m_temperature",
  output_subdir = "era5_2m_temperature",
  convert_kelvin = TRUE
)

### PRECIPITATION: Total (mm/month)
# -----------------------------
precip_results <- download_and_process_era5(
  variable = "total_precipitation",
  output_subdir = "era5_total_precipitation",
  convert_precip_mm = TRUE
)

### FINAL SUMMARY
# -----------------------------
cat("\n=== OVERALL PROCESSING SUMMARY ===\n")
cat("Temperature layers processed:", temp_results$total_layers, "\n")
cat("Temperature problematic layers:", length(temp_results$problematic_layers), "\n")
cat("Precipitation layers processed:", precip_results$total_layers, "\n")
cat("Precipitation problematic layers:", length(precip_results$problematic_layers), "\n")
cat("===================================\n")