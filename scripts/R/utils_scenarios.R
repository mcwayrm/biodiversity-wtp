# scripts/R/utils_scenarios.R
##########################################
# Generate scenarios.yml
##########################################

generate_scenarios <- function(output_path = "scenarios.yml",
                               intervals = c(2, 3, 6, 12),
                               min_years = c(2, 4, 6, 8),
                               clust_sizes = c(5, 10),
                               voronoi_multipliers = c(1, 2),
                               choice_radii = c(5, 10),
                               max_alternatives = c(Inf),
                               interval_codes = NULL) {

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
              # example: BM-8y_c5km_v10km_r5km_mInf
              ma_tag <- if (is.finite(ma)) as.character(ma) else "Inf"
              name <- sprintf("%s-%dy_c%dkm_v%dkm_r%dkm_m%s", code, yrs, cs, voronoi_limit, cr, ma_tag)
              scenarios[[name]] <- list(
                interval_months    = iv,
                min_years_active   = yrs,
                interval_code      = code,
                # clustering / voronoi parameters
                clustering_method  = 'complete',
                projection_crs      = 8857,
                clust_size_km      = cs,
                voronoi_limit_km   = voronoi_limit,
                # user choice set parameters
                choice_radius_km   = cr,
                max_alternatives   = ma
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
if (!file.exists(file.path("scenarios", "scenarios.yml"))) {
  message("scenarios.yml not found. Generating...")
  generate_scenarios()
}

# Load scenarios
scenarios <- yaml::read_yaml("scenarios.yml")$scenarios