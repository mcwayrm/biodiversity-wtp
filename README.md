# Willingness to Pay for Biodiversity

This repository implements a modular pipeline for analyzing eBird observation data to estimate birders' willingness-to-pay (WTP) for biodiversity using random utility models (RUM).

## Overview

The pipeline processes eBird trip data, constructs choice sets of potential birding destinations, extracts site attributes (biodiversity metrics, environmental conditions, travel costs), and estimates discrete choice models to quantify WTP for biodiversity.

## Project Structure

```
biodiversity-wtp/
├── run_all.R                               # Master pipeline controller
├── scenarios.yml                           # Scenario parameter definitions
├── scripts/R/
│   ├── utils_config.R                      # Configuration and package loading
│   ├── utils_scenarios.R                   # Scenario definitions loader
│   ├── utils_tasks.R                       # Task runner with logging
│   ├── 01_load_ebird_data.R                # Clean eBird data, assign homes
│   ├── 02_filter_users.R                   # Filter to active users
│   ├── 03_distance_to_hotspots.R           # Calculate home-to-hotspot distances
│   ├── 04_generate_voronoi_clusters.R      # Create spatial clusters
│   ├── 05_create_choice_set.R              # Generate observed + counterfactual choices
│   ├── 06_extract_site_attributes.R        # Extract climate & vegetation data
│   ├── 07_compute_biodiversity_metrics.R   # Calculate species richness & congestion
│   ├── 08_merge_site_attributes.R          # Combine all site-level data
│   ├── 09_compute_travel_cost.R            # Calculate travel costs
│   ├── 10_estimate_rum_models.R            # Fit discrete choice models
│   ├── 11_generate_scenario_outputs.R      # Create maps, tables, figures
│   └── 12_generate_summary_report.R        # Cross-scenario comparison report
├── data/
│   ├── raw/                                # Raw input data (user-specific location)
│   ├── intermediate/
│   │   ├── ebird_clean/                    # Cleaned eBird trips & user homes
│   │   └── scenarios/{scenario_name}/      # Scenario-specific intermediate files
├── output/
│   └── scenarios/{scenario_name}/          # Final outputs per scenario
│       ├── figures/                        # Maps and plots
│       ├── tables/                         # Summary statistics and WTP estimates
│       └── models/                         # Saved model objects
│   └── summary_report                      # Report comparing all scenarios (html/qmd)
├── logs/
│   └── processing_times.csv                # Task execution log
└── README.md
```

## Setup

### 1. Configuration (`utils_config.R`)

The configuration script:

- Detects the operating system and username
- Sets user-specific paths to raw input data
- Loads required R packages
- Ensures consistent directory structure across users

### 2. Required Input Data Structure

Place raw data in your configured `input_data_dir` following this structure:

```
input_data_dir/
├── districts/
│   └── district-2011/
│       └── district-2011.shp              # India administrative boundaries (2011)
├── ebird/
│   ├── ebd_IN_201501_202412_relDec-2024/
│   │   └── ebd_IN_201501_202412_relDec-2024.txt  # eBird Basic Dataset for India
│   └── hotspots.rds                       # eBird hotspot locations
├── era5_2m_temperature/
│   ├── era5_2m_temperature_2015_01.tif    # Monthly temperature rasters
│   └── ...
├── era5_total_precipitation/
│   ├── era5_total_precipitation_2015_01.tif  # Monthly precipitation rasters
│   └── ...
├── gdp/
│   └── final_GDP_0_25deg_postadjust_pop_density.csv  # GDP data for travel cost
├── modis_vcf/
│   ├── MOD44B.061_Percent_Tree_Cover_doy2015065_aid0001.tif  # MODIS Tree cover
│   └── ...
└── protected_areas/
    └── 04_MainlandPAsShapefile/
        └── MainlandPAs_HolesFilled100k_FewRivRemv.shp  # Protected area boundaries
```

### 3. Scenario Configuration (`scenarios.yml`)

Define analysis scenarios with different parameters. The pipeline will automatically generate a template `scenarios.yml` if one doesn't exist using `utils_scenarios.R`.

Scenario Naming Convention: `{interval_code}-{min_years}y_c{clust_size}km_v{voronoi_limit}km_r{choice_radius}km_m{max_alternatives}`

#### Parameter Definitions

```yaml
scenarios:
  BM-2y_c5km_v5km_r5km_mInf:
    # User filtering parameters
    interval_months: 2.0                   # Activity interval: users must have ≥1 trip per this many months
    min_years_active: 2.0                  # Minimum years of observed birding activity
    interval_code: BM                      # Code for naming (BM=bimonthly, Q=quarterly, M=monthly)
    
    # Spatial clustering parameters
    clustering_method: complete            # Hierarchical clustering method (complete, single, average)
    projection_crs: 8857                   # Projected CRS for distance calculations (EPSG:8857 for India)
    clust_size_km: 5.0                     # Maximum distance for hotspots to be grouped into same cluster
    voronoi_limit_km: 5.0                  # Max distance from cluster centroid to Voronoi polygon boundary
    
    # Choice set construction parameters
    choice_radius_km: 5.0                  # Radius around user home to include counterfactual alternatives
    max_alternatives: .inf                 # Maximum counterfactual alternatives per choice set (use .inf for unlimited)
    
    # Analysis period
    analysis_start_year: 2019              # First year to include in analysis
    analysis_end_year: 2023                # Last year to include in analysis
    
    # Model specification
    model_vars:                            # Variables to include in RUM
      - expected_richness                  # Species richness (continuous)
      - expected_congestion                # Number of birders (continuous)
      - precip                             # Precipitation (mm)
      - temp                               # Temperature (°C)
      - trees                              # Tree cover (%)
      - travel_cost_combined               # Total travel cost
      - dist_to_pa_km                      # Distance to protected area (km)
    
    fe_vars:                               # Fixed effects variables
      - user_id                            # Individual birder fixed effects
      - year_month                         # Time fixed effects
      - c_code_2011                        # District fixed effects
    
    mixed_vars:                            # Random coefficient variables (for mixed logit)
      - expected_richness                  # Allow WTP for richness to vary across individuals
```


## Running the Pipeline

### Full Pipeline Execution

Run all scenarios defined in `scenarios.yml`:

```r
source("run_all.R")
```

This executes all 12 tasks for each scenario sequentially.

### Individual Task Execution

Run specific tasks manually:

```r
source("scripts/R/utils_config.R")
source("scripts/R/utils_scenarios.R")
source("scripts/R/utils_tasks.R")

# Example: Run task 1 for a specific scenario
run_task(
  script = "01_load_ebird_data.R",
  scenario_name = "BM-2y_c5km_v5km_r5km_mInf",
  input_paths = list(...),
  output_paths = list(...),
  skip_if_exists = TRUE
)
```

### Task Logging

The pipeline automatically logs processing times to `logs/processing_times.csv`:

```csv
timestamp,scenario,task,elapsed_seconds,status
2025-11-13 12:00:00,BM-2y_c5km_v5km_r5km_mInf,01_load_ebird_data.R,45.2,completed
2025-11-13 12:00:45,BM-2y_c5km_v5km_r5km_mInf,02_filter_users.R,12.8,completed
```

View processing time summaries:

```r
source("scripts/R/utils_tasks.R")
summarize_processing_times()
summarize_processing_times(scenario = "BM-2y_c5km_v5km_r5km_mInf")
```

## Pipeline Tasks

### Task 1: Load and Clean eBird Data
Script: `01_load_ebird_data.R`

Outputs:

- `data/intermediate/ebird_clean/ebird_trips.parquet` - Cleaned trip records
- `data/intermediate/ebird_clean/user_home_real.rds` - Explicit home locations
- `data/intermediate/ebird_clean/user_home_imputed.rds` - Imputed home locations

### Task 2: Filter Users
Script: `02_filter_users.R`

Outputs:

- `data/intermediate/scenarios/{scenario}/ebird_trips_filtered.parquet`

### Task 3: Calculate Distance to Hotspots
Script: `03_distance_to_hotspots.R`

Outputs:

- `data/intermediate/scenarios/{scenario}/ebird_trips_hotspots.parquet`

### Task 4: Generate Voronoi Clusters
Script: `04_generate_voronoi_clusters.R`

Outputs:

- `data/intermediate/scenarios/{scenario}/ebird_hotspots_voronoi.gpkg` - Spatial polygons
- `data/intermediate/scenarios/{scenario}/ebird_hotspots_clustered.parquet` - Cluster assignments

### Task 5: Create Choice Set
Script: `05_create_choice_set.R`

Outputs:

- `data/intermediate/scenarios/{scenario}/master_data.parquet`

### Task 6: Extract Site Attributes
Script: `06_extract_site_attributes.R`

Outputs:

- `data/intermediate/scenarios/{scenario}/site_precip.parquet`
- `data/intermediate/scenarios/{scenario}/site_temp.parquet`
- `data/intermediate/scenarios/{scenario}/site_trees.parquet`

### Task 7: Compute Biodiversity Metrics
Script: `07_compute_biodiversity_metrics.R`

Outputs:

- `data/intermediate/scenarios/{scenario}/biodiv_monthly_richness.parquet`
- `data/intermediate/scenarios/{scenario}/biodiv_weekly_richness.parquet`
- `data/intermediate/scenarios/{scenario}/biodiv_seasonal_richness.parquet`
- `data/intermediate/scenarios/{scenario}/biodiv_monthly_congestion.parquet`
- `data/intermediate/scenarios/{scenario}/biodiv_weekly_congestion.parquet`
- `data/intermediate/scenarios/{scenario}/biodiv_seasonal_congestion.parquet`

### Task 8: Merge Site Attributes
Script: `08_merge_site_attributes.R`

Outputs:

- `data/intermediate/scenarios/{scenario}/master_data_with_attributes.parquet`

### Task 9: Compute Travel Cost
Script: `09_compute_travel_cost.R`

Outputs:

- `data/intermediate/scenarios/{scenario}/master_data_with_travel_cost.parquet`

### Task 10: Estimate RUM Models
Script: `10_estimate_rum_models.R`

Outputs:

- `data/intermediate/scenarios/{scenario}/master_data_final.parquet`
- `output/scenarios/{scenario}/models/model_basic.rds`
- `output/scenarios/{scenario}/models/model_fe.rds`
- `output/scenarios/{scenario}/models/model_mixed.rds`

### Task 11: Generate Scenario Outputs
Script: `11_generate_scenario_outputs.R`

Outputs:

- `output/scenarios/{scenario}/figures/voronoi_map.png`
- `output/scenarios/{scenario}/tables/wtp_comparison.csv`
- `output/scenarios/{scenario}/tables/data_summary.csv`

### Task 12: Generate Summary Report
Script: `12_generate_summary_report.R`

Outputs:

- `output/summary_report.html`

<!-- ## Data Dictionary

### Key Variables

**Trip/Choice Variables:**

- `user_id` - Unique eBird user identifier
- `trip_id` - Unique trip identifier
- `choice` - Binary indicator (1 = observed trip, 0 = counterfactual)
- `cluster_id` - Hotspot cluster identifier
- `date` - Trip date
- `duration_minutes` - Trip duration
- `geo_dist` - Geodesic distance from home to hotspot (km)

**Site Attributes:**

- `expected_richness` - Species richness (unique species count)
- `expected_congestion` - Number of birders/checklists
- `precip` - Precipitation (mm)
- `temp` - Temperature (°C)
- `trees` - Tree cover percentage
- `dist_to_pa_km` - Distance to nearest protected area (km)

**Geographic:**

- `c_code_2011` - Census district code (2011 boundaries)
- `lon_home`, `lat_home` - User home coordinates
- `longitude`, `latitude` - Hotspot coordinates

**Economic:**

- `travel_cost_combined` - Total travel cost (distance + opportunity cost of time) -->

## Dependencies

### Software
- **R** (version 4.3.2 or higher)

### R Packages
- `yaml` - YAML configuration parsing
- `tidyverse` - Data manipulation and visualization
- `arrow` - Parquet file I/O
- `here` - Path management
- `sf` - Spatial vector data
- `terra` - Spatial raster data
- `data.table` - Efficient data processing
- `lubridate` - Date handling
- `logitr` - Logit model estimation
- `fixest` - Fixed effects models
- `vegan` - Biodiversity metrics
- `pbapply` - Progress bars
- `cluster`, `factoextra` - Clustering algorithms

<!-- ## Outputs

### Scenario-Specific Outputs

Each scenario produces:

1. **Models:** Saved model objects (`.rds`) for basic, fixed effects, and mixed logit models
2. **Tables:** WTP estimates, data summaries, coefficient tables (`.csv`)
3. **Figures:** Voronoi cluster maps, spatial distributions (`.png`)

### Cross-Scenario Summary

The final report (`output/summary_report.html`) compares:

- WTP estimates across scenarios
- Model fit statistics
- Robustness checks
- Sample composition differences -->

<!-- ## Citation

[Add citation information]

## License

[Add license information] -->

## Contact

- Raahil Madhok
- Matthew Braaksma
- Ryan McWay
- Jovin Lasway
