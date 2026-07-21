# run_all.R
##########################################
# Master Pipeline Controller
##########################################

# -----------------------------------------------------------------------------
# Set Up
# -----------------------------------------------------------------------------
# Load packages and set raw input data dir
# Looks for username to set custom paths; otherwise defaults to 'data/raw'
source(file.path("scripts", "R", "utils_config.R"))

# Load scenarios
source(file.path("scripts", "R", "utils_scenarios.R"))

# Load models
models <- yaml::read_yaml("models.yml")

# Load task runner function
source(file.path("scripts", "R", "utils_tasks.R"))

library(processx)
library(jsonlite)


# -----------------------------------------------------------------------------
# Run All Scenarios
# -----------------------------------------------------------------------------
# Loop through scenarios (call run_task with params and scenario_name)
for (scenario_name in names(scenarios)) {
  params <- scenarios[[scenario_name]]
  message("\n======================================")
  message("Scenario: ", scenario_name)
  message("======================================\n")

  # create per-scenario output dir
  scenario_dir <- here("data", "intermediate", "scenarios", scenario_name)
  if (!dir.exists(scenario_dir)) dir.create(scenario_dir, recursive = TRUE, showWarnings = FALSE)

  # --- Data Preparation Tasks (1-8) ---
  # Task 1: Load and Clean eBird Data
  run_task(
    "01_load_ebird_data.R",
    inputs = list(
      ebird_basic = file.path(input_data_dir, "ebird", "ebd_IN_201501_202412_relDec-2024", "ebd_IN_201501_202412_relDec-2024.txt"),
      district_shp = file.path(input_data_dir, "districts", "district-2011")
    ),
    outputs = list(
      ebird_trips = here("data", "intermediate", "ebird_clean", "ebird_trips.parquet"),
      ebird_trips_home = here("data", "intermediate", "ebird_clean", "user_home_real.rds"),
      ebird_trips_imputed = here("data", "intermediate", "ebird_clean", "user_home_imputed.rds")
    ),
    scenario_name = scenario_name
  )
  
  # Task 2: Filter Users to Desired Sample
  run_task(
    "02_filter_users.R",
    inputs = list(
      ebird_trips = here("data", "intermediate", "ebird_clean", "ebird_trips.parquet")
    ),
    outputs = list(
      ebird_trips_filtered = file.path(scenario_dir, "ebird_trips_filtered.parquet")
    ),
    params = params,
    scenario_name = scenario_name
  )

  # Task 3: Calculate Distance to Hotspots
  run_task(
    "03_distance_to_hotspots.R",
    inputs = list(
      ebird_trips_filtered = file.path(scenario_dir, "ebird_trips_filtered.parquet")
    ),
    outputs = list(
      ebird_trips_hotspots = file.path(scenario_dir, "ebird_trips_hotspots.parquet")
    ),
    params = params,
    scenario_name = scenario_name
  )

  # Task 4: Generate Hotspot Clusters (Voronoi Polygons)
  run_task(
    "04_generate_voronoi_clusters.R",
    inputs = list(
      hotspots = file.path(input_data_dir, "ebird", "hotspots.rds"),
      district_shp = file.path(input_data_dir, "districts", "district-2011")
    ),
    outputs = list(
      voronoi_shp = file.path(scenario_dir, "ebird_hotspots_voronoi.gpkg"),
      hotspots_clustered = file.path(scenario_dir, "ebird_hotspots_clustered.parquet")
    ),
    params = params,
    scenario_name = scenario_name
  )

  # Task 5: Create Choice Set
  run_task(
    "05_create_choice_set.R",
    inputs = list(
      hotspots_clustered = file.path(scenario_dir, "ebird_hotspots_clustered.parquet"),
      ebird_trips_hotspots = file.path(scenario_dir, "ebird_trips_hotspots.parquet")
    ),
    outputs = list(
      master_data = file.path(scenario_dir, "master_data.parquet")
    ),
    params = params,
    scenario_name = scenario_name
  )

  # Task 6: Extract Site Attributes
  run_task(
    "06_extract_site_attributes.R",
    inputs = list(
      hotspots_clustered = file.path(scenario_dir, "ebird_hotspots_clustered.parquet"),
      voronoi_shp = file.path(scenario_dir, "ebird_hotspots_voronoi.gpkg"),
      precip_dir = file.path(input_data_dir, "era5_total_precipitation"),
      temp_dir = file.path(input_data_dir, "era5_2m_temperature"),
      trees_dir = file.path(input_data_dir, "modis_vcf")
    ),
    outputs = list(
      precip = file.path(scenario_dir, "site_precip.parquet"),
      temp = file.path(scenario_dir, "site_temp.parquet"),
      trees = file.path(scenario_dir, "site_trees.parquet")
    ),
    params = params,
    scenario_name = scenario_name
  )

  # Task 7: Compute Biodiversity Metrics
  run_task(
    "07_compute_biodiversity_metrics.R",
    inputs = list(
      ebird_basic = file.path(input_data_dir, "ebird", "ebd_IN_201501_202412_relDec-2024", "ebd_IN_201501_202412_relDec-2024.txt"),
      voronoi_shp = file.path(scenario_dir, "ebird_hotspots_voronoi.gpkg"),
      migrant_species = file.path(input_data_dir, "species", "species_list_categorized.csv")
    ),
    outputs = list(
      monthly_richness = file.path(scenario_dir, "biodiv_monthly_richness.parquet"),
      weekly_richness = file.path(scenario_dir, "biodiv_weekly_richness.parquet"),
      seasonal_richness = file.path(scenario_dir, "biodiv_seasonal_richness.parquet"),
      monthly_congestion = file.path(scenario_dir, "biodiv_monthly_congestion.parquet"),
      weekly_congestion = file.path(scenario_dir, "biodiv_weekly_congestion.parquet"),
      seasonal_congestion = file.path(scenario_dir, "biodiv_seasonal_congestion.parquet"),
      monthly_shannon = file.path(scenario_dir, "biodiv_monthly_shannon.parquet"),
      weekly_shannon = file.path(scenario_dir, "biodiv_weekly_shannon.parquet"),
      seasonal_shannon = file.path(scenario_dir, "biodiv_seasonal_shannon.parquet"),
      monthly_simpson = file.path(scenario_dir, "biodiv_monthly_simpson.parquet"),
      weekly_simpson = file.path(scenario_dir, "biodiv_weekly_simpson.parquet"),
      seasonal_simpson = file.path(scenario_dir, "biodiv_seasonal_simpson.parquet"),
      monthly_migrant = file.path(scenario_dir, "biodiv_monthly_migrant.parquet"),
      weekly_migrant = file.path(scenario_dir, "biodiv_weekly_migrant.parquet"),
      seasonal_migrant = file.path(scenario_dir, "biodiv_seasonal_migrant.parquet"),
      monthly_resident = file.path(scenario_dir, "biodiv_monthly_resident.parquet"),
      weekly_resident = file.path(scenario_dir, "biodiv_weekly_resident.parquet"),
      seasonal_resident = file.path(scenario_dir, "biodiv_seasonal_resident.parquet"),
      species_matching_log = file.path(scenario_dir, "species_matching_log.csv")
    ),
    params = params,
    scenario_name = scenario_name
  )

  # Task 8: Merge Site Attributes
  run_task(
    "08_merge_site_attributes.R",
    inputs = list(
      master_data = file.path(scenario_dir, "master_data.parquet"),
      hotspots_clustered = file.path(scenario_dir, "ebird_hotspots_clustered.parquet"),
      precip = file.path(scenario_dir, "site_precip.parquet"),
      temp = file.path(scenario_dir, "site_temp.parquet"),
      trees = file.path(scenario_dir, "site_trees.parquet"),
      monthly_richness = file.path(scenario_dir, "biodiv_monthly_richness.parquet"),
      weekly_richness = file.path(scenario_dir, "biodiv_weekly_richness.parquet"),
      seasonal_richness = file.path(scenario_dir, "biodiv_seasonal_richness.parquet"),
      monthly_congestion = file.path(scenario_dir, "biodiv_monthly_congestion.parquet"),
      weekly_congestion = file.path(scenario_dir, "biodiv_weekly_congestion.parquet"),
      seasonal_congestion = file.path(scenario_dir, "biodiv_seasonal_congestion.parquet"),
      monthly_shannon = file.path(scenario_dir, "biodiv_monthly_shannon.parquet"),
      weekly_shannon = file.path(scenario_dir, "biodiv_weekly_shannon.parquet"),
      seasonal_shannon = file.path(scenario_dir, "biodiv_seasonal_shannon.parquet"),
      monthly_simpson = file.path(scenario_dir, "biodiv_monthly_simpson.parquet"),
      weekly_simpson = file.path(scenario_dir, "biodiv_weekly_simpson.parquet"),
      seasonal_simpson = file.path(scenario_dir, "biodiv_seasonal_simpson.parquet"),
      monthly_migrant = file.path(scenario_dir, "biodiv_monthly_migrant.parquet"),
      weekly_migrant = file.path(scenario_dir, "biodiv_weekly_migrant.parquet"),
      seasonal_migrant = file.path(scenario_dir, "biodiv_seasonal_migrant.parquet"),
      monthly_resident = file.path(scenario_dir, "biodiv_monthly_resident.parquet"),
      weekly_resident = file.path(scenario_dir, "biodiv_weekly_resident.parquet"),
      seasonal_resident = file.path(scenario_dir, "biodiv_seasonal_resident.parquet"),
      protected_areas_shp = file.path(input_data_dir, "protected_areas", "04_MainlandPAsShapefile")
    ),
    outputs = list(
      master_data_with_attributes = file.path(scenario_dir, "master_data_with_attributes.parquet")
    ),
    params = params,
    scenario_name = scenario_name
  )

  # Stage 9: Compute Travel Cost
  run_task(
    "09_compute_travel_cost.R",
    inputs = list(
      master_data_with_attributes = file.path(scenario_dir, "master_data_with_attributes.parquet"),
      district_shp = file.path(input_data_dir, "districts", "district-2011"),
      cpi_xlsx = file.path(input_data_dir, "cpi", "INDCPIALLAINMEI.xlsx"),
      driving_cost_rds = file.path(input_data_dir, "driving_cost", "driving_cost.rds"),
      gdp = file.path(input_data_dir, "gdp", "final_GDPC_0_25deg_postadjust_pop_dens_0_01_adjust.csv")
    ),
    outputs = list(
      master_data_with_travel_cost = file.path(scenario_dir, "master_data_with_travel_cost.parquet")
    ),
    params = params,
    scenario_name = scenario_name
  )

  # Stage 10: Compute IV
  # run_task(
  #   "10_compute_iv.R",
  #   inputs = list(
  #     master_data_with_travel_cost = file.path(scenario_dir, "master_data_with_travel_cost.parquet"),
  #     district_shp = file.path(input_data_dir, "districts", "district-2011"),
  #     species_ranges = file.path(input_data_dir, "species", "BOTW_2024_2.gpkg"),
  #     migratory_status = file.path(input_data_dir, "migratory_status", "species_list_categorized.csv"),
  #     missing_migration = file.path(input_data_dir, "migratory_status", "missing_migration.csv"),
  #     flu_outbreaks = file.path(input_data_dir, "flu_outbreaks", "flu_outbreaks_with_country.csv"),
  #     voronoi_shp = file.path(scenario_dir, "ebird_hotspots_voronoi.gpkg")
  #   ),
  #   outputs = list(
  #     master_data_with_iv = file.path(scenario_dir, "master_data_with_iv.parquet"),
  #     iv_panel = file.path(scenario_dir, "iv_panel.parquet")
  #   ),
  #   params = params,
  #   scenario_name = scenario_name
  # )
  # --- Model Estimation & Output Tasks (11-12) ---
  
  # Stage 11: Estimate RUM Models via Python (one call per scenario)
  # ===================================================================
  # Stage 11a: Prepare model data (filtering, temporal vars, means)
  run_task(
    "11a_model_data_prep.R",
    inputs = list(
      master_data_with_iv = file.path(scenario_dir, "master_data_with_travel_cost.parquet")
    ),
    outputs = list(
      model_data = file.path("output", "models", sprintf("model_data_%s.parquet", scenario_name))
    ),
    params = params,
    scenario_name = scenario_name
  )
  
  source(file.path("scripts", "R", "utils_xlogit_reticulate.R"))

  input_data_prepped <- file.path("output", "models", sprintf("model_data_%s.parquet", scenario_name))
  output_dir_models <- file.path("output", "models")

  if (!dir.exists(output_dir_models)) {
    dir.create(output_dir_models, recursive = TRUE, showWarnings = FALSE)
  }

  if (!exists(".xlogit_env_ready")) {
    setup_xlogit_env("wtp01")
    .xlogit_env_ready <- TRUE
  }

  xlogit_result <- estimate_rum_models_reticulate(
    scenario_name = scenario_name,
    input_data_path = input_data_prepped,
    output_dir = output_dir_models,
    models_config = "models.yml",
    save_demeaned = TRUE
  )

  if (!xlogit_result$success) {
    stop(paste0("XLogit estimation had ", xlogit_result$fail_count,
                " failed model(s) for scenario ", scenario_name))
  }
  
  message(paste0("  ✓ Python estimation complete for ", scenario_name))
}


# Generate Summary Report
message("Generating Summary Report...")
quarto::quarto_render(
  input = here("summary_report.qmd"),
  execute_params = list(
  scenarios = names(scenarios),
  models = names(models),
  output_dir = "output/scenarios",
  district_shp = file.path(input_data_dir, "districts", "district-2011", "district-2011.shp")
  )
)
message("Summary report generated")