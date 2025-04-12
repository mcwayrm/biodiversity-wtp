
# Config Loader and Directory Setup
# ---------------------------------
# This script detects the operating system and sets the username to 
# determine the correct user-specific configuration from config.yml.
#
# It then extracts all directory-related variables (excluding input_data_dir),
# verifies their existence, and creates any missing directories to ensure 
# a consistent file structure for data processing.

# Clear objects
rm(list = ls())

# Detect OS and set username
os_type <- .Platform$OS.type  # "windows" or "unix"
username <- if (os_type == "windows") Sys.getenv("USERNAME") else Sys.getenv("USER")

# Load the correct config
config_list <- yaml::read_yaml("config.yml", readLines.warn = FALSE, eval.expr = FALSE)
config_name <- if (username %in% names(config_list)) username else "default"
config <- config::get(config = config_name)
writeLines(paste("OS:", os_type, "| User:", username, "| Using config:", config_name))

# Function to extract all 'dir' variables except 'input_data_dir'
extract_dirs <- function(cfg) {
  # Get all names in the config
  dir_names <- names(cfg)
  # Filter names ending with 'dir' and exclude 'input_data_dir'
  dir_vars <- dir_names[grepl("dir$", dir_names) & dir_names != "input_data_dir"]
  # Extract the directory paths
  dirs <- lapply(dir_vars, function(var) cfg[[var]])
  # Return named list of directories
  names(dirs) <- dir_vars
  return(dirs)
}

# Get all 'dir' variables except 'input_data_dir'
dir_list <- extract_dirs(config)

# Loop through the directories and create them if needed
for (dir in dir_list) {
  dir_path <- do.call(file.path, as.list(dir))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    message("Created directory: ", dir_path)
  }
}

# Source functions
source(config$choice_set_travel_path)
source(config$hotspot_clustering_path)

# Load packages
pacman::p_load(config$packages, character.only = TRUE, install = FALSE)
