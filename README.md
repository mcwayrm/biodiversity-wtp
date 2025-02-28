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
- Uses `yaml::read_yaml()` to parse the YAML file.
- Dynamically constructs directory paths based on user configuration.
- Verifies and creates directories as needed to maintain a consistent file structure.

### Usage
1. Ensure `config.yml` is correctly set up for your user.
2. Run `0.load_config.R` at the beginning of your scripts to automatically load paths and create missing directories.
3. Proceed with data processing scripts using the loaded configurations.

This setup ensures flexibility across different environments while maintaining a structured and reproducible workflow for eBird data processing.

# Scripts

This describes the scripts in the repository. To perform the analysis, run the following scripts in order. 

- `0.load_config.R`: This script sets up the configuration of the file paths and allows for flexible loading of the data for each user. Modifications to the personal users preferences can be made in `config.yml`. 
- `1.ebd_process.R`: This script cleans the e-bird data set and creates the data set of homes for the users, trips by the users, and attributes about the users. 
- `2.distance_to_hotspot.R`: This script determines the distance of birders from observed hotspots to create the counterfactual options for travel and their potential travel cost. 
- `3.make_choice_sets.R`: This script performs the mixed logit analysis of comparing the selected destination over possible desitinations. 

# Data

This describes the data needed to run the code. For each data set, we describe it and explain how it is used in the analysis. 

## Inputs

These are the input data (raw files) prior to processing: 

- `data/rds/hotspots.rds`: 
- `data/shp/district-2011`: This is the administrative boundaries for India from 2011 provided by GADM.
- `../ebd_IN_smp_relJan-2025.zip`: This is the e-bird data set of bird observations by users for the India sample. 

## Intermediates

These are the intermediate files generated through the scripts but not the main files for analysis: 

- 

# Final

These are the final data sets generated for the main analysis: 

- 

# Dependencies

This describes the dependencies required to perform the analysis. This includes both the software and the packages necessary including the versions that were used for the analysis. This is helful for replcation of the work. 

## Software: 

- R Programming: This is the main statistical tool used to perfom the analysis. Version 4.3.2

## Packages: 

For R: 

- `yaml`: describe and version...
- `pacman`: Efficently load in several R packages at once. 
- `sf`: Handles spatial vector data sets. 
- `tidyverse`: Tools for cleaning data.
- `data.table`: Efficient management of tabular data for data cleaning. 
- `lubridate`: Handles date formating. 
- `units`: 

## Functions: 

- `/functions/choice_set_travel.R`: This function...
- `/functions/hotspot_clustering.R`: This function...

# Questions

This is a section to ask questions and assign team members to respond to them. Below is an example. 

- @mcwayrm How do I use the questions section of the `README.md`?
    - Answer: Well, you simply follow this example. You ask a question and direct it towards an individual or generally and provide details about the specific files, tasks, or code that is of concern. Adding details assists us in the future and helps us document ongoing concerns and potential problems. 

- Put you questions here...
- @m-braaksma I am trying to document necessary software. Do you need anything to run a YAML file? Like do we need to download YAML software or Docker to run this component? 
- @rmadhok We don't have a clear understanding of the intermediate data sets and how they relate to the scripts. Can you help use make these connections?
- @rmadhok Where does `data/rds/hotspots.rds` data come from? How was it constructed? 