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

# Load task runner function
source(file.path("scripts", "R", "utils_tasks.R"))


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

  # Task 1: Load and Clean eBird Data
  run_task(
    "01_load_ebird_data.R",
    input_paths = list(
      ebird_basic = file.path(input_data_dir, "ebird", "ebd_IN_201501_202412_relDec-2024", "ebd_IN_201501_202412_relDec-2024.txt"),
      district_shp = file.path(input_data_dir, "districts", "district-2011")
    ),
    output_paths = list(
      ebird_trips = here("data", "intermediate", "ebird_clean", "ebird_trips.parquet"),
      ebird_trips_home = here("data", "intermediate", "ebird_clean", "user_home_real.rds"),
      ebird_trips_imputed = here("data", "intermediate", "ebird_clean", "user_home_imputed.rds")
    ),
    scenario_name = scenario_name
  )
  
  # Task 2: Filter Users to Desired Sample
  run_task(
    "02_filter_users.R",
    input_paths = list(
      ebird_trips = here("data", "intermediate", "ebird_clean", "ebird_trips.parquet")
    ),
    output_paths = list(
      ebird_trips_filtered = file.path(scenario_dir, "ebird_trips_filtered.parquet")
    ),
    params = params,
    scenario_name = scenario_name
  )

  # Task 3: Calculate Distance to Hotspots
  run_task(
    "03_distance_to_hotspots.R",
    input_paths = list(
      ebird_trips_filtered = file.path(scenario_dir, "ebird_trips_filtered.parquet")
    ),
    output_paths = list(
      ebird_trips_hotspots = file.path(scenario_dir, "ebird_trips_hotspots.parquet")
    ),
    params = params,
    scenario_name = scenario_name
  )

  # Task 4: Generate Hotspot Clusters (Voronoi Polygons)
  run_task(
    "04_generate_voronoi_clusters.R",
    input_paths = list(
      hotspots = file.path(input_data_dir, "ebird", "hotspots.rds"),
      district_shp = file.path(input_data_dir, "districts", "district-2011")
    ),
    output_paths = list(
      voronoi_shp = file.path(scenario_dir, "ebird_hotspots_voronoi.gpkg"),
      hotspots_clustered = file.path(scenario_dir, "ebird_hotspots_clustered.parquet")
    ),
    params = params,
    scenario_name = scenario_name
  )

  # Task 5: Create Choice Set
  run_task(
    "05_create_choice_set.R",
    input_paths = list(
      hotspots_clustered = file.path(scenario_dir, "ebird_hotspots_clustered.parquet"),
      ebird_trips_hotspots = file.path(scenario_dir, "ebird_trips_hotspots.parquet")
    ),
    output_paths = list(
      master_data = file.path(scenario_dir, "master_data.parquet")
    ),
    params = params,
    scenario_name = scenario_name
  )

  # Task 6: Extract Site Attributes
  run_task(
    "06_extract_site_attributes.R",
    input_paths = list(
      hotspots_clustered = file.path(scenario_dir, "ebird_hotspots_clustered.parquet"),
      voronoi_shp = file.path(scenario_dir, "ebird_hotspots_voronoi.gpkg"),
      precip_dir = file.path(input_data_dir, "era5_total_precipitation"),
      temp_dir = file.path(input_data_dir, "era5_2m_temperature"),
      trees_dir = file.path(input_data_dir, "modis_vcf")
    ),
    output_paths = list(
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
    input_paths = list(
      ebird_basic = file.path(input_data_dir, "ebird", "ebd_IN_201501_202412_relDec-2024", "ebd_IN_201501_202412_relDec-2024.txt"),
      voronoi_shp = file.path(scenario_dir, "ebird_hotspots_voronoi.gpkg")
    ),
    output_paths = list(
      monthly_richness = file.path(scenario_dir, "biodiv_monthly_richness.parquet"),
      weekly_richness = file.path(scenario_dir, "biodiv_weekly_richness.parquet"),
      seasonal_richness = file.path(scenario_dir, "biodiv_seasonal_richness.parquet"),
      monthly_congestion = file.path(scenario_dir, "biodiv_monthly_congestion.parquet"),
      weekly_congestion = file.path(scenario_dir, "biodiv_weekly_congestion.parquet"),
      seasonal_congestion = file.path(scenario_dir, "biodiv_seasonal_congestion.parquet")
    ),
    params = params,
    scenario_name = scenario_name
  )

  # Task 8: Merge Site Attributes
  run_task(
    "08_merge_site_attributes.R",
    input_paths = list(
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
      protected_areas_shp = file.path(input_data_dir, "protected_areas", "04_MainlandPAsShapefile")
    ),
    output_paths = list(
      master_data_with_attributes = file.path(scenario_dir, "master_data_with_attributes.parquet")
    ),
    params = params,
    scenario_name = scenario_name
  )

  # Stage 9: Compute Travel Cost
  run_task(
    "09_compute_travel_cost.R",
    input_paths = list(
      master_data_with_attributes = file.path(scenario_dir, "master_data_with_attributes.parquet"),
      district_shp = file.path(input_data_dir, "districts", "district-2011"),
      gdp = file.path(input_data_dir, "gdp", "final_GDP_0_25deg_postadjust_pop_density.csv")
    ),
    output_paths = list(
      master_data_with_travel_cost = file.path(scenario_dir, "master_data_with_travel_cost.parquet")
    ),
    params = params,
    scenario_name = scenario_name
  )

  # Stage 10: Estimate RUM Models
  run_task(
    "10_estimate_rum_models.R",
    input_paths = list(
      master_data_with_travel_cost = file.path(scenario_dir, "master_data_with_travel_cost.parquet")
    ),
    output_paths = list(
      master_data_final = file.path(scenario_dir, "master_data_final.parquet"),
      model_basic = file.path("output", "scenarios", scenario_name, "models", "model_basic.rds"),
      wtp_basic = file.path("output", "scenarios", scenario_name, "models", "wtp_basic.rds"),
      model_fe = file.path("output", "scenarios", scenario_name, "models", "model_fe.rds"),
      wtp_fe = file.path("output", "scenarios", scenario_name, "models", "wtp_fe.rds")
      # model_mixed = file.path("output", "scenarios", scenario_name, "models", "model_mixed.rds"),
      # wtp_mixed = file.path("output", "scenarios", scenario_name, "models", "wtp_mixed.rds")
    ),
    params = params,
    scenario_name = scenario_name
  )

  # Stage 11: Generate Scenario Outputs
  run_task(
    "11_generate_scenario_outputs.R",
    input_paths = list(
      voronoi_shp = file.path(scenario_dir, "ebird_hotspots_voronoi.gpkg"),
      hotspots_clustered = file.path(scenario_dir, "ebird_hotspots_clustered.parquet"),
      master_data_final = file.path(scenario_dir, "master_data_final.parquet"),
      district_shp = file.path(input_data_dir, "districts", "district-2011", "district-2011.shp"),
      model_basic = file.path("output", "scenarios", scenario_name, "models", "model_basic.rds"),
      model_fe = file.path("output", "scenarios", scenario_name, "models", "model_fe.rds"),
      model_mixed = file.path("output", "scenarios", scenario_name, "models", "model_mixed.rds")
    ),
    output_paths = list(
      # voronoi_plot = file.path("output", "scenarios", scenario_name, "figures", "voronoi_map.png"),
      # wtp_table = file.path("output", "scenarios", scenario_name, "tables", "wtp_comparison.csv"),
      data_summary = file.path("output", "scenarios", scenario_name, "tables", "data_summary.csv")
    ),
    params = params,
    scenario_name = scenario_name
  )
}

# Generate Summary Report
message("\n======================================")
message("Generating Summary Report")
message("======================================\n")

quarto::quarto_render(
  input = here("summary_report.qmd"),
  execute_params = list(
    scenarios = names(scenarios),
    output_dir = "scenarios"
  )
)

message("Summary report generated")

# # List of scenario names
# scenarios <- c(
#   "A-5y_c5km_v5km_r5km_mInf",
#   "A-5y_c5km_v5km_r5km_mInf_ERmonth",
#   "A-5y_c5km_v5km_r5km_mInf_ERseason",
#   "A-5y_c5km_v5km_r30km_mInf",
#   "A-5y_c5km_v5km_r30km_mInf_ERmonth",
#   "A-5y_c5km_v5km_r30km_mInf_ERseason",
#   "BM-2y_c5km_v5km_r5km_mInf",
#   "BM-2y_c5km_v5km_r5km_mInf_ERmonth",
#   "BM-2y_c5km_v5km_r5km_mInf_ERseason",
#   "BM-2y_c5km_v5km_r30km_mInf",
#   "BM-2y_c5km_v5km_r30km_mInf_ERmonth",
#   "BM-2y_c5km_v5km_r30km_mInf_ERseason",
#   "Q-2y_c5km_v5km_r5km_mInf",
#   "Q-2y_c5km_v5km_r5km_mInf_ERmonth",
#   "Q-2y_c5km_v5km_r5km_mInf_ERseason",
#   "Q-2y_c5km_v5km_r30km_mInf",
#   "Q-2y_c5km_v5km_r30km_mInf_ERmonth",
#   "Q-2y_c5km_v5km_r30km_mInf_ERseason"
# )

# output_dir <- "output/scenarios"

# for (scenario in scenarios) {
#   for (model_type in c("basic", "fe")) {
#     model_path <- file.path(output_dir, scenario, "models", paste0("model_", model_type, ".rds"))
#     wtp_path <- file.path(output_dir, scenario, "models", paste0("wtp_", model_type, ".rds"))
#     if (file.exists(model_path)) {
#       model_obj <- readRDS(model_path)
#       if (!is.null(model_obj$wtp)) {
#         saveRDS(model_obj$wtp, wtp_path)
#         message("Saved WTP for ", model_type, " in scenario: ", scenario)
#       } else {
#         message("No WTP found in model for ", model_type, " in scenario: ", scenario)
#       }
#     } else {
#       message("Model file not found for ", model_type, " in scenario: ", scenario)
#     }
#   }
# }
