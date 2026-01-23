# scripts/R/utils_scenarios.R
##########################################
# Generate scenarios.yml
##########################################

generate_scenarios <- function(output_path = "scenarios.yml",
                               intervals = c(3, 6, 12),
                               min_years = c(2),
                               clust_sizes = c(5),
                               voronoi_multipliers = c(1),
                               choice_radii = c(5, 30), 
                               max_alternatives = c(Inf),
                               interval_codes = NULL,
                               analysis_start_year = 2019,
                               analysis_end_year = 2023,
                               model_vars = c("expected_richness",
                                              "expected_congestion",
                                              "precip",
                                              "temp",
                                              "trees",
                                              "travel_cost_combined",
                                              "dist_to_pa_km"),
                               fe_vars = c("user_id", "year_month", "c_code_2011"),
                               mixed_vars = c("expected_richness")) {

  # default human-friendly codes
  default_codes <- c("2" = "BM", "3" = "Q", "6" = "SA", "12" = "A")
  if (is.null(interval_codes)) {
    interval_codes <- default_codes
  } else {
    # ensure names are character keys
    names(interval_codes) <- as.character(names(interval_codes))
  }

  scenarios <- list()

  for (iv in intervals) {
    iv_chr <- as.character(iv)
    code <- if (!is.null(interval_codes[[iv_chr]])) interval_codes[[iv_chr]] else paste0("I", iv_chr)
    for (yrs in min_years) {
      for (cs in clust_sizes) {
        for (vm in voronoi_multipliers) {
          voronoi_limit <- cs * vm
          for (cr in choice_radii) {
            for (ma in max_alternatives) {
              # scenario name includes interval code, years, cluster size, voronoi limit, choice radius, and max alternatives
              ma_tag <- if (is.finite(ma)) as.character(ma) else "Inf"
              name <- sprintf("%s-%dy_c%dkm_v%dkm_r%dkm_m%s", code, yrs, cs, voronoi_limit, cr, ma_tag)
              scenarios[[name]] <- list(
                interval_months    = iv,
                min_years_active   = yrs,
                interval_code      = code,
                # clustering / voronoi parameters
                clustering_method  = 'complete',
                projection_crs     = 8857,
                clust_size_km      = cs,
                voronoi_limit_km   = voronoi_limit,
                # user choice set parameters
                choice_radius_km   = cr,
                max_alternatives   = ma,
                # RUM estimation / analysis parameters (variable via function args)
                analysis_start_year = analysis_start_year,
                analysis_end_year   = analysis_end_year,
                model_vars          = model_vars,
                fe_vars             = fe_vars,
                mixed_vars          = mixed_vars
              )
            }
          }
        }
      }
    }
  }

  # ensure directory exists
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  yaml::write_yaml(list(scenarios = scenarios), output_path)
  message("Created ", output_path, " (", length(scenarios), " scenarios)")
  invisible(scenarios)
}

# Generate scenarios.yml if no present
if (!file.exists(file.path("scenarios.yml"))) {
  message("scenarios.yml not found. Generating...")
  generate_scenarios()
}

# Load scenarios
scenarios <- yaml::read_yaml("scenarios.yml")$scenarios