# scripts/R/make_sample_robustness_table.R
# ===========================================================================
# SAMPLE ROBUSTNESS TABLE (LaTeX)
# ===========================================================================

library(tidyverse)
library(xtable)

# ---------------------------------------------------------------------------
# Paths and parameters
# ---------------------------------------------------------------------------

MODELS_DIR <- file.path("output", "models")
OUTPUT_TEX_A <- file.path("output", "tables", "sample_robustness_table_ER.tex")
OUTPUT_TEX_B <- file.path("output", "tables", "sample_robustness_table_ERmigrant.tex")
OUTPUT_TEX_C <- file.path("output", "tables", "sample_robustness_table_ERmigrantseason.tex")

SCENARIO <- "c5km_v5km_r50km_mInf"

SAMPLES <- c(
  "full",
  "above_median",
  "below_median",
  "only2015"
)

# ---------------------------------------------------------------------------
# Model specifications
# ---------------------------------------------------------------------------

SPECS <- list(
  list(
    name = "ER",
    label = "Expected Richness"
  ),
  list(
    name = "ERmigrant",
    label = "Expected Richness (Migrant vs. Resident)"
  ),
  list(
    name = "ERmigrantseason",
    label = "Expected Richness (Migrant/Resident $\\times$ Season)"
  )
)

# ---------------------------------------------------------------------------
# Variable labels
#
# IMPORTANT:
# The order here determines the order of rows in the final table.
# ---------------------------------------------------------------------------

VAR_LABELS <- c(

  # ER table
  "expected_richness_dm" =
    "Expected Richness (utils)",

  # ERmigrant / ERmigrantseason tables
  "migrant_richness_dm" =
    "Migrant Richness (utils)",

  "resident_richness_dm" =
    "Resident Richness (utils)",

  # ERmigrantseason table
  "migrant_richness_Fall_dm" =
    "Migrant Richness $\\times$ Fall (utils)",

  "resident_richness_Fall_dm" =
    "Resident Richness $\\times$ Fall (utils)",

  "migrant_richness_Winter_dm" =
    "Migrant Richness $\\times$ Winter (utils)",

  "resident_richness_Winter_dm" =
    "Resident Richness $\\times$ Winter (utils)",

  "migrant_richness_Spring_dm" =
    "Migrant Richness $\\times$ Spring (utils)",

  "resident_richness_Spring_dm" =
    "Resident Richness $\\times$ Spring (utils)",

  # Shown as its own coefficient row in every table (not collapsed into
  # Controls) -- it is the price numeraire, so it has no WTP row of its own.
  "log_travel_cost_dm" =
    "Log Travel Cost (utils)"
)

# Rows shown as a coefficient row in every table but with no corresponding
# WTP row (the price variable is the WTP denominator, not a WTP-eligible
# attribute).
NO_WTP_VARS <- c("log_travel_cost_dm")

# Control variables collapsed into a single "Controls" Yes/No indicator row
# rather than shown as individual coefficient rows.
CONTROL_VARS <- c(
  "log_trip_number_dm", "group_size_dm", "expected_congestion_dm",
  "precip_dm", "temp_dm", "trees_dm"
)

# Which VAR_LABELS rows (main variables of interest) belong to each table --
# tables differ in which richness variables they include. log_travel_cost_dm
# is appended to every table as a shared coefficient row.
PANEL_VARS <- list(
  ER = c("expected_richness_dm", "log_travel_cost_dm"),
  ERmigrant = c("migrant_richness_dm", "resident_richness_dm", "log_travel_cost_dm"),
  ERmigrantseason = c(
    "migrant_richness_dm", "resident_richness_dm",
    "migrant_richness_Fall_dm", "resident_richness_Fall_dm",
    "migrant_richness_Winter_dm", "resident_richness_Winter_dm",
    "migrant_richness_Spring_dm", "resident_richness_Spring_dm",
    "log_travel_cost_dm"
  )
)

SUMMARY_LABELS <- c(
  "Log-Likelihood",
  "N Observations",
  "N Choice Situations"
)

# ---------------------------------------------------------------------------
# Load coefficients from CSV
# ---------------------------------------------------------------------------

load_coefs <- function(spec_name, sample) {

  scenario_str <- paste0(sample, "_", SCENARIO)

  filename <- paste0(
    spec_name,
    "_indxyear-sitexseason-hour_choiceset10_Mixed_",
    scenario_str,
    "_coefficients.csv"
  )

  filepath <- file.path(MODELS_DIR, filename)

  if (!file.exists(filepath)) {
    warning("File not found: ", filepath)
    return(NULL)
  }

  read_csv(filepath, show_col_types = FALSE) %>%
    rename(
      variable = Variable,
      estimate = Estimate,
      std.error = `Std.Err`,
      p_value = `p-value`
    ) %>%
    select(
      variable,
      estimate,
      std.error,
      p_value
    ) %>%
    mutate(
      sig = case_when(
        p_value < 0.01 ~ "***",
        p_value < 0.05 ~ "**",
        p_value < 0.10 ~ "*",
        TRUE ~ ""
      )
    )
}

# ---------------------------------------------------------------------------
# Load WTP (currency-scaled willingness-to-pay) from CSV
# ---------------------------------------------------------------------------

load_wtp <- function(spec_name, sample) {

  scenario_str <- paste0(sample, "_", SCENARIO)

  filename <- paste0(
    spec_name,
    "_indxyear-sitexseason-hour_choiceset10_Mixed_",
    scenario_str,
    "_wtp.csv"
  )

  filepath <- file.path(MODELS_DIR, filename)

  if (!file.exists(filepath)) {
    warning("WTP file not found: ", filepath)
    return(NULL)
  }

  read_csv(filepath, show_col_types = FALSE) %>%
    rename(
      variable = Variable,
      estimate = WTP,
      std.error = `Std.Err`,
      p_value = `p-value`
    ) %>%
    select(
      variable,
      estimate,
      std.error,
      p_value
    ) %>%
    mutate(
      sig = case_when(
        p_value < 0.01 ~ "***",
        p_value < 0.05 ~ "**",
        p_value < 0.10 ~ "*",
        TRUE ~ ""
      )
    )
}

# ---------------------------------------------------------------------------
# Load summary TXT
# ---------------------------------------------------------------------------

load_summary <- function(spec_name, sample) {

  scenario_str <- paste0(sample, "_", SCENARIO)

  filename <- paste0(
    spec_name,
    "_indxyear-sitexseason-hour_choiceset10_Mixed_",
    scenario_str,
    "_summary.txt"
  )

  filepath <- file.path(MODELS_DIR, filename)

  if (!file.exists(filepath)) {

    warning("Summary file not found: ", filepath)

    return(list(
      ll = NA_real_,
      aic = NA_real_,
      bic = NA_real_,
      n_obs = NA_real_,
      n_choice = NA_real_,
      coefs = NULL
    ))
  }

  content <- readLines(filepath, warn = FALSE)

  # -------------------------------------------------------------------------
  # Fit statistics
  # -------------------------------------------------------------------------

  extract_number <- function(pattern) {

    line <- grep(
      pattern,
      content,
      value = TRUE
    )

    if (length(line) == 0) {
      return(NA_real_)
    }

    as.numeric(
      sub(
        ".*:\\s*",
        "",
        line[1]
      )
    )
  }

  ll <- extract_number("^  Log-Likelihood:")
  aic <- extract_number("^  AIC:")
  bic <- extract_number("^  BIC:")
  n_obs <- extract_number("^  N Observations:")
  n_choice <- extract_number("^  N Choice Situations:")

  # -------------------------------------------------------------------------
  # Coefficients
  # -------------------------------------------------------------------------

  coef_start <- which(
    grepl(
      "^Variable\\s+Estimate",
      content
    )
  )

  coefs <- NULL

  if (length(coef_start) > 0) {

    start <- coef_start[1] + 2

    # Find first standard-deviation coefficient.
    # Everything before that is a mean coefficient.
    end_candidates <- which(
      grepl(
        "^\\s*sd\\.",
        content[start:length(content)]
      )
    )

    if (length(end_candidates) > 0) {
      end <- start + end_candidates[1] - 2
    } else {
      end <- length(content)
    }

    coef_lines <- content[start:end]

    # Keep actual coefficient lines
    coef_lines <- coef_lines[
      grepl(
        "^\\S+\\s+[-0-9]",
        coef_lines
      )
    ]

    if (length(coef_lines) > 0) {

      coefs <- map_dfr(
        coef_lines,
        function(x) {

          pieces <- strsplit(
            trimws(x),
            "\\s+"
          )[[1]]

          tibble(
            variable = pieces[1],
            estimate = as.numeric(pieces[2]),
            std.error = as.numeric(pieces[3]),
            t_stat = as.numeric(pieces[4]),
            p_value = as.numeric(pieces[5])
          )
        }
      ) %>%
        filter(
          !grepl("^sd\\.", variable)
        ) %>%
        mutate(
          sig = case_when(
            p_value < 0.01 ~ "***",
            p_value < 0.05 ~ "**",
            p_value < 0.10 ~ "*",
            TRUE ~ ""
          )
        )
    }
  }

  list(
    ll = ll,
    aic = aic,
    bic = bic,
    n_obs = n_obs,
    n_choice = n_choice,
    coefs = coefs
  )
}

# ---------------------------------------------------------------------------
# Build a table for ONE model specification (= one panel)
#
# This produces, in row order:
#   - one coefficient row (+ SE row) per main variable of interest for this
#     panel (from PANEL_VARS)
#   - one WTP row (+ SE row) per the same main variables
#   - a single "Controls" Yes/No indicator row
#   - fit-statistic rows
#
# variable | full | above_median | below_median | only2015
#
# It does NOT create any LaTeX yet.
# ---------------------------------------------------------------------------

build_spec_table <- function(spec_name) {

  summary_list <- map(
    SAMPLES,
    ~load_summary(spec_name, .x)
  )

  wtp_list <- map(
    SAMPLES,
    ~load_wtp(spec_name, .x)
  )

  panel_vars <- PANEL_VARS[[spec_name]]

  # -------------------------------------------------------------------------
  # Coefficient rows (main variables of interest only -- controls are
  # collapsed into a single indicator row below)
  # -------------------------------------------------------------------------

  coefficient_rows <- map_dfr(
    panel_vars,
    function(var) {

      row <- tibble(
        variable = unname(VAR_LABELS[var])
      )

      for (i in seq_along(SAMPLES)) {

        coefs <- summary_list[[i]]$coefs

        if (
          !is.null(coefs) &&
          var %in% coefs$variable
        ) {

          x <- coefs %>%
            filter(variable == var) %>%
            slice(1)

          row[[SAMPLES[i]]] <- paste0(
            "\\makecell{",
            sprintf("%.4f", x$estimate),
            x$sig,
            "\\\\",
            "(",
            sprintf("%.4f", x$std.error),
            ")",
            "}"
          )

        } else {

          row[[SAMPLES[i]]] <- ""
        }
      }

      row
    }
  )

  # -------------------------------------------------------------------------
  # WTP rows (currency-scaled WTP for the same main variables)
  # -------------------------------------------------------------------------

  wtp_rows <- map_dfr(
    setdiff(panel_vars, NO_WTP_VARS),
    function(var) {

      base_label <- sub(" \\(utils\\)$", "", unname(VAR_LABELS[var]))
      row <- tibble(
        variable = paste0("WTP: ", base_label, " (2021 INR)")
      )

      for (i in seq_along(SAMPLES)) {

        wtp <- wtp_list[[i]]

        if (
          !is.null(wtp) &&
          var %in% wtp$variable
        ) {

          x <- wtp %>%
            filter(variable == var) %>%
            slice(1)

          row[[SAMPLES[i]]] <- paste0(
            "\\makecell{",
            sprintf("%.4f", x$estimate),
            x$sig,
            "\\\\",
            "(",
            sprintf("%.4f", x$std.error),
            ")",
            "}"
          )

        } else {

          row[[SAMPLES[i]]] <- ""
        }
      }

      row
    }
  )

  # -------------------------------------------------------------------------
  # Controls indicator row -- a sample counts as having controls if its
  # coefficients file includes at least one of the CONTROL_VARS
  # -------------------------------------------------------------------------

  controls_row <- tibble(variable = "Controls")

  for (i in seq_along(SAMPLES)) {
    coefs <- summary_list[[i]]$coefs
    has_controls <- !is.null(coefs) && any(CONTROL_VARS %in% coefs$variable)
    controls_row[[SAMPLES[i]]] <- if (has_controls) "Yes" else "No"
  }

  # -------------------------------------------------------------------------
  # Summary statistics
  # -------------------------------------------------------------------------

  summary_rows <- tibble(
    variable = SUMMARY_LABELS,

    full = as.character(
      c(
        summary_list[[1]]$ll,
        summary_list[[1]]$n_obs,
        summary_list[[1]]$n_choice
      )
    ),

    above_median = as.character(
      c(
        summary_list[[2]]$ll,
        summary_list[[2]]$n_obs,
        summary_list[[2]]$n_choice
      )
    ),

    below_median = as.character(
      c(
        summary_list[[3]]$ll,
        summary_list[[3]]$n_obs,
        summary_list[[3]]$n_choice
      )
    ),

    only2015 = as.character(
      c(
        summary_list[[4]]$ll,
        summary_list[[4]]$n_obs,
        summary_list[[4]]$n_choice
      )
    )
  )

  bind_rows(
    coefficient_rows,
    wtp_rows,
    controls_row,
    summary_rows
  )
}

# ---------------------------------------------------------------------------
# Build all three specifications (one per panel)
# ---------------------------------------------------------------------------

table_list <- map(
  SPECS,
  ~build_spec_table(.x$name)
)
names(table_list) <- map_chr(SPECS, ~.x$name)

# ---------------------------------------------------------------------------
# Helper: retrieve a cell
# ---------------------------------------------------------------------------

get_cell <- function(tab, var_label, sample) {

  x <- tab[
    tab$variable == var_label,
    ,
    drop = FALSE
  ]

  if (nrow(x) == 0) {
    return("")
  }

  value <- x[[sample]][1]

  if (is.null(value) || is.na(value)) {
    return("")
  }

  as.character(value)
}

# ---------------------------------------------------------------------------
# Write one panel's table as its own standalone LaTeX table with 4 columns
# ((1) Full, (2) Above Median, (3) Below Median, (4) 2015-Only), so each
# panel gets full column width and there's room to add more robustness
# checks (more columns) later without re-splitting again.
# ---------------------------------------------------------------------------

# Output is the bare \begin{tabular}...\end{tabular} only -- no \begin{table}
# wrapper, \caption, \label, or notes -- so it can be \input{} directly into
# a hand-written \begin{table} block in the Overleaf doc (caption/label/notes
# are added there, matching the project's existing table style, e.g.
# tables/ebird_dhs_ttest.tex). A suggested caption/label/notes for each
# table is printed to the console instead of written to the file.
write_table <- function(spec_name, caption, table_label, output_path) {

  tab <- table_list[[spec_name]]
  panel_vars <- PANEL_VARS[[spec_name]]
  wtp_vars <- setdiff(panel_vars, NO_WTP_VARS)
  wtp_labels <- paste0(
    "WTP: ", sub(" \\(utils\\)$", "", unname(VAR_LABELS[wtp_vars])), " (2021 INR)"
  )
  row_labels <- c(unname(VAR_LABELS[panel_vars]), wtp_labels, "Controls", SUMMARY_LABELS)

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  sink(output_path)

  cat("\\begin{tabular}{lcccc}\n")
  cat("\\toprule\n")

  cat(" & (1) & (2) & (3) & (4) \\\\\n")
  cat("Variable & Full & Above & Below & 2015 \\\\\n")

  cat("\\midrule\n")

  for (var_label in row_labels) {

    values <- c(
      get_cell(tab, var_label, "full"),
      get_cell(tab, var_label, "above_median"),
      get_cell(tab, var_label, "below_median"),
      get_cell(tab, var_label, "only2015")
    )

    cat(
      var_label,
      " & ",
      paste(values, collapse = " & "),
      " \\\\\n",
      sep = ""
    )

    # Horizontal lines to separate coefficients / WTP / controls / fit stats
    if (var_label == tail(unname(VAR_LABELS[panel_vars]), 1)) {
      cat("\\midrule\n")
    }
    if (var_label == tail(wtp_labels, 1)) {
      cat("\\midrule\n")
    }
    if (var_label == "Controls") {
      cat("\\midrule\n")
    }
  }

  cat("\\bottomrule\n")
  cat("\\end{tabular}\n")

  sink()

  message("Wrote table to: ", output_path)
  message(
    "  Suggested caption/label/notes for the surrounding \\begin{table} block:\n",
    "    \\caption{", caption, "}\n",
    "    \\label{", table_label, "}\n",
    "    Note: Standard errors in parentheses. *p<.1, **p<.05, ***p<.01. ",
    "Column numbers in parentheses above match the corresponding column labels. ",
    "Coefficients labeled (utils) are on the standardized scale as estimated; WTP rows convert the ",
    "corresponding coefficient to real 2021 INR, using log travel cost as the price numeraire. ",
    "Controls include log trip number, group size, expected congestion, precipitation, ",
    "temperature, and tree cover. All specifications include individual-by-year, site-by-season, ",
    "and hour-of-day fixed effects.\n"
  )
}

write_table(
  "ER", "Sample Robustness: Expected Richness",
  "tab:sample_robustness_er", OUTPUT_TEX_A
)

write_table(
  "ERmigrant", "Sample Robustness: Migrant vs. Resident",
  "tab:sample_robustness_ermigrant", OUTPUT_TEX_B
)

write_table(
  "ERmigrantseason", "Sample Robustness: Migrant/Resident $\\times$ Season",
  "tab:sample_robustness_ermigrantseason", OUTPUT_TEX_C
)