# Willingness to Pay for Biodiversity

## Set up 
### Configuration File (`config.yml`)

The config.yml file defines the directory structure and file paths used in processing eBird data. It includes:
- Default directories and paths for storing shapefiles, intermediate data, and function scripts.
- User-specific configurations that adjust input data paths based on the operating system username.
- A reference file structure to clarify how data is organized.

### Structure of `config.yml`

The YAML file consists of two main sections:

1. Default Configuration
- Defines directory structures for core data categories such as shapefiles (shp), intermediate processed data (intermediate), and function scripts (functions).
- Uses !expr do.call(file.path, ...) to dynamically construct file paths.

2. User-Specific Configurations
- Provides an override mechanism for individual users based on their system username.
- Ensures that different users can specify unique input data locations while adhering to the standard directory structure.

### Configuration Loader (`0.load_config.R`)

The 0.load_config.R script is responsible for:
- Detecting the operating system (Windows or Unix).
- Identifying the current username to select the appropriate configuration.
- Loading the user-specific or default settings from `config.yml`.
- Extracting all directory-related variables except input_data_dir.
- Ensuring that all required directories exist, creating any missing ones automatically.

#### Key Features:
- Uses yaml::read_yaml() to parse the YAML file.
- Dynamically constructs directory paths based on user configuration.
- Verifies and creates directories as needed to maintain a consistent file structure.

### Usage
1. Ensure config.yml is correctly set up for your user.
2. Run 0.load_config.R at the beginning of your scripts to automatically load paths and create missing directories.
3. Proceed with data processing scripts using the loaded configurations.

This setup ensures flexibility across different environments while maintaining a structured and reproducible workflow for eBird data processing.

# Scripts
This describes the scripts in the replication package.

# Data
This describes the data needed to run the code. 
