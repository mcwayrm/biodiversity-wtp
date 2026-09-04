# scripts/R/make_fe_robustness_table.R
# ===========================================================================
# FE ROBUSTNESS TABLE (LaTeX)
# ===========================================================================
# Builds a LaTeX regression table showing the FE build-up sequence, from
# minimal FE structure to the main specification. Reads directly from
# already-produced *_coefficients.csv and *_summary.txt files -- no
# re-estimation, pure post-processing.
#
# Reports estimated coefficients directly (standardized scale, as xlogit
# actually estimates them), not WTP-converted values -- WTP depends on a
# reference price and isn't directly comparable column-to-column the way
# raw coefficients are.
#
# Fit stats (LL/AIC/BIC/N) are parsed from each model's own *_summary.txt,
# NOT all_models_summary.csv -- that file gets fully overwritten (not
# merged) by each --models-subset run, so it only ever reflects the most
# recently run batch, not the cumulative set of all models ever estimated.
#
# Requires the booktabs LaTeX package (\toprule/\midrule/\bottomrule) --
# already standard in most journal/thesis templates, add
# \usepackage{booktabs} if your Overleaf template doesn't have it.

library(dplyr)

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
MODELS_DIR <- file.path("output", "models")
SCENARIO <- "full_c5km_v5km_r50km_mInf"
OUTPUT_TEX <- file.path("output", "tables", "fe_robustness_table.tex")

# One entry per table column, in display order. fe_* flags are hand-set
# (not auto-parsed from the Fixed Effects text) -- more reliable than
# regex-matching a stringified Python list, and you already know exactly
# what each spec's FE structure is.
#
# ADD NEW COLUMNS HERE as new specs/samples are estimated -- e.g. the
# 2015-stable-sample and skill-tier splits, once you have separate model
# ymls for those. Column order in the table follows this list's order.
TABLE_COLUMNS <- list(
  list(model = "ER_ind_choiceset10", type = "Mixed", label = "(1)", k = 10,
       fe_individual = TRUE,  fe_indxyear = FALSE, fe_site = FALSE, fe_sitexseason = FALSE, fe_hour = FALSE),
  list(model = "ER_indxyear_choiceset10", type = "Mixed", label = "(2)", k = 10,
       fe_individual = FALSE, fe_indxyear = TRUE,  fe_site = FALSE, fe_sitexseason = FALSE, fe_hour = FALSE),
  list(model = "ER_indxyear-site_choiceset10", type = "Mixed", label = "(3)", k = 10,
       fe_individual = FALSE, fe_indxyear = TRUE,  fe_site = TRUE,  fe_sitexseason = FALSE, fe_hour = FALSE),
  list(model = "ER_indxyear-sitexseason_choiceset10", type = "Mixed", label = "(4)", k = 10,
       fe_individual = FALSE, fe_indxyear = TRUE,  fe_site = FALSE, fe_sitexseason = TRUE,  fe_hour = FALSE),
  list(model = "ER_indxyear-sitexseason-hour_choiceset10", type = "Mixed", label = "(5) Main Spec", k = 10,
       fe_individual = FALSE, fe_indxyear = TRUE,  fe_site = FALSE, fe_sitexseason = TRUE,  fe_hour = TRUE)

  # -- Add once ready, using whatever model names your new ymls define:
  # ,list(model = "ER_stable2015_choiceset10", type = "Mixed", label = "(6) Stable Sample", k = 10,
  #        fe_individual = FALSE, fe_indxyear = TRUE, fe_site = FALSE, fe_sitexseason = TRUE, fe_hour = TRUE)
  # ,list(model = "ER_aboveMedian_choiceset10", type = "Mixed", label = "(7) High-Frequency", k = 10,
  #        fe_individual = FALSE, fe_indxyear = TRUE, fe_site = FALSE, fe_sitexseason = TRUE, fe_hour = TRUE)
  # ,list(model = "ER_belowMedian_choiceset10", type = "Mixed", label = "(8) Low-Frequency", k = 10,
  #        fe_individual = FALSE, fe_indxyear = TRUE, fe_site = FALSE, fe_sitexseason = TRUE, fe_hour = TRUE)
)

# Display labels and row order for coefficients -- edit/extend as your
# model_vars change. Rows not present in a given column are left blank
# (handled automatically below), so this list can safely be a superset of
# what any one model actually includes.
#
# Only the variables of primary interest (species richness, travel cost) are
# shown as explicit coefficient rows. Everything else (congestion, trip
# number, group size, precip, temp, trees) is a control and is instead
# summarized by the "Controls" indicator row below.
VAR_LABELS <- c(
  expected_richness = "Species Richness (utils)",
  log_travel_cost = "Log Travel Cost (utils)"
)

# The variable WTP is computed for -- only species richness gets a WTP row;
# log_travel_cost is the price variable itself (the WTP denominator), so it
# has no WTP of its own.
WTP_VAR <- "expected_richness"

# Control variables collapsed into a single "Controls" Yes/No indicator row
# rather than shown as individual coefficient rows.
CONTROL_VARS <- c(
  "expected_congestion", "log_trip_number", "group_size",
  "precip", "temp", "trees"
)

# Random-coefficient SD rows (mixed logit heterogeneity terms) -- shown in
# a separate block below the main coefficients, standard practice
SD_LABELS <- c(
  "sd.expected_richness_dm" = "SD(Species Richness)"
)

# ---------------------------------------------------------------------------
# Load
# ---------------------------------------------------------------------------
load_coefs <- function(model_name, model_type) {
  path <- file.path(MODELS_DIR, sprintf("%s_%s_%s_coefficients.csv", model_name, model_type, SCENARIO))
  if (!file.exists(path)) {
    message("NOTE: not found yet, column will be blank: ", basename(path))
    return(NULL)
  }
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  df$variable <- gsub("_dm$", "", df$Variable)
  df$`Std.Err`[df$`Std.Err` == 1.000] <- NA_real_ # Treat SE = 1.000 as invalid/missing
  df
}

load_wtp <- function(model_name, model_type) {
  path <- file.path(MODELS_DIR, sprintf("%s_%s_%s_wtp.csv", model_name, model_type, SCENARIO))
  if (!file.exists(path)) {
    message("NOTE: WTP file not found, WTP row will be blank: ", basename(path))
    return(NULL)
  }
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  df$variable <- gsub("_dm$", "", df$Variable)
  df
}

parse_summary_txt <- function(model_name, model_type) {
  path <- file.path(MODELS_DIR, sprintf("%s_%s_%s_summary.txt", model_name, model_type, SCENARIO))
  if (!file.exists(path)) {
    return(list(N_obs = NA, N_choice_sits = NA, LL = NA, AIC = NA, BIC = NA))
  }
  lines <- trimws(readLines(path))
  get_num <- function(prefix) {
    line <- lines[startsWith(lines, prefix)]
    if (length(line) == 0) return(NA)
    suppressWarnings(as.numeric(trimws(sub(paste0("^", prefix), "", line[1]))))
  }
  list(
    LL = get_num("Log-Likelihood:"),
    AIC = get_num("AIC:"),
    BIC = get_num("BIC:"),
    N_obs = get_num("N Observations:"),
    N_choice_sits = get_num("N Choice Situations:")
  )
}

# ---------------------------------------------------------------------------
# Format one coefficient/SE cell, with significance stars
# ---------------------------------------------------------------------------
format_cell <- function(coef_df, var_name, est_col = "Estimate") {
  if (is.null(coef_df)) return(c("", ""))
  row <- coef_df[coef_df$variable == var_name, ]
  if (nrow(row) == 0) return(c("", ""))
  est <- row[[est_col]][1]
  se <- row$`Std.Err`[1]
  pval <- row$`p-value`[1]
  stars <- if (is.na(pval)) "" else if (pval < 0.01) "$^{***}$" else if (pval < 0.05) "$^{**}$" else if (pval < 0.10) "$^{*}$" else ""
  se_str <- if (is.na(se)) "(--)" else sprintf("(%.3f)", se)
  c(sprintf("%.3f%s", est, stars), se_str)
}

yesno <- function(x) if (isTRUE(x)) "Yes" else ""

# ---------------------------------------------------------------------------
# Build table data
# ---------------------------------------------------------------------------
col_data <- lapply(TABLE_COLUMNS, function(col) {
  coefs <- load_coefs(col$model, col$type)
  # skip_std_errs=True (used as a fallback when the numerical Hessian is
  # singular -- common for very sparse FE specs like individual-only, where
  # covariates retain much more shared seasonal/spatial variation than
  # under richer FE) leaves Std.Err all NA for that model. Flag it visibly
  # rather than silently showing blank/garbled SE cells.
  se_missing <- !is.null(coefs) && all(is.na(coefs$`Std.Err`))
  label <- if (se_missing) paste0(col$label, "$^{\\dagger}$") else col$label
  list(
    label = label,
    coefs = coefs,
    wtp = load_wtp(col$model, col$type),
    fit = parse_summary_txt(col$model, col$type),
    fe = col,
    se_missing = se_missing
  )
})

any_se_missing <- any(sapply(col_data, function(c) c$se_missing))

n_cols <- length(col_data)

# ---------------------------------------------------------------------------
# Write LaTeX
#
# Output is the bare \begin{tabular}...\end{tabular} only -- no \begin{table}
# wrapper, \caption, \label, or notes -- so it can be \input{} directly into
# a hand-written \begin{table} block in the Overleaf doc (caption/label/notes
# are added there, matching the project's existing table style).
# ---------------------------------------------------------------------------
lines_out <- c(
  sprintf("\\begin{tabular}{l%s}", strrep("c", n_cols)),
  "\\toprule",
  paste0(" & ", paste(sapply(col_data, function(c) c$label), collapse = " & "), " \\\\"),
  "\\midrule"
)

# Main coefficient rows
for (var in names(VAR_LABELS)) {
  cells <- lapply(col_data, function(c) format_cell(c$coefs, var))
  row1 <- paste0(VAR_LABELS[var], " & ", paste(sapply(cells, `[`, 1), collapse = " & "), " \\\\")
  row2 <- paste0(" & ", paste(sapply(cells, `[`, 2), collapse = " & "), " \\\\")
  lines_out <- c(lines_out, row1, row2)
}

lines_out <- c(lines_out, "\\midrule")

# WTP row (currency-scaled willingness-to-pay for species richness, from
# each column's own *_wtp.csv). calculate_wtp() (Python) computes se_wtp
# (and, from it, the WTP p-value/stars) via the delta method applied to the
# *_coefficients.csv Std.Err values -- for columns where those are the
# skip_std_errs=True placeholder (1.0, scrubbed to NA in load_coefs above),
# both se_wtp and its stars are propagated from that placeholder, not a real
# inference. Strip both here for any se_missing column so the WTP row
# doesn't show spurious precision.
wtp_cells <- lapply(col_data, function(c) {
  cell <- format_cell(c$wtp, WTP_VAR, est_col = "WTP")
  if (c$se_missing) {
    cell[1] <- sub("\\$\\^\\{[*]+\\}\\$$", "", cell[1]) # drop spurious stars
    cell[2] <- "(--)"
  }
  cell
})
lines_out <- c(
  lines_out,
  paste0("WTP: Species Richness (2021 INR) & ", paste(sapply(wtp_cells, `[`, 1), collapse = " & "), " \\\\"),
  paste0(" & ", paste(sapply(wtp_cells, `[`, 2), collapse = " & "), " \\\\"),
  "\\midrule"
)

# Random-coefficient SD rows
sd_present <- any(sapply(col_data, function(c) !is.null(c$coefs) && any(names(SD_LABELS) %in% c$coefs$Variable)))
if (sd_present) {
  for (sd_var in names(SD_LABELS)) {
    cells <- lapply(col_data, function(c) {
      if (is.null(c$coefs)) return(c("", ""))
      row <- c$coefs[c$coefs$Variable == sd_var, ]
      if (nrow(row) == 0) return(c("", ""))
      est <- row$Estimate[1]; se <- row$`Std.Err`[1]; pval <- row$`p-value`[1]
      stars <- if (is.na(pval)) "" else if (pval < 0.01) "$^{***}$" else if (pval < 0.05) "$^{**}$" else if (pval < 0.10) "$^{*}$" else ""
      se_str <- if (is.na(se)) "(--)" else sprintf("(%.3f)", se)
      c(sprintf("%.3f%s", est, stars), se_str)
    })
    row1 <- paste0(SD_LABELS[sd_var], " & ", paste(sapply(cells, `[`, 1), collapse = " & "), " \\\\")
    row2 <- paste0(" & ", paste(sapply(cells, `[`, 2), collapse = " & "), " \\\\")
    lines_out <- c(lines_out, row1, row2)
  }
  lines_out <- c(lines_out, "\\midrule")
}

# Controls indicator row (congestion, trip number, group size, precip,
# temp, trees, travel cost -- shown as a single Yes/No indicator rather
# than as individual coefficient rows; a column counts as having controls
# if its coefficients file includes at least one of them)
controls_present <- function(c) {
  !is.null(c$coefs) && any(CONTROL_VARS %in% c$coefs$variable)
}
lines_out <- c(
  lines_out,
  paste0("Controls & ", paste(sapply(col_data, function(c) yesno(controls_present(c))), collapse = " & "), " \\\\"),
  "\\midrule"
)

# FE indicator rows
fe_row <- function(label, key) {
  vals <- sapply(col_data, function(c) yesno(c$fe[[key]]))
  paste0(label, " & ", paste(vals, collapse = " & "), " \\\\")
}
lines_out <- c(
  lines_out,
  fe_row("Individual FE", "fe_individual"),
  fe_row("Individual $\\times$ Year FE", "fe_indxyear"),
  fe_row("Site FE", "fe_site"),
  fe_row("Site $\\times$ Season FE", "fe_sitexseason"),
  fe_row("Hour FE", "fe_hour"),
  "\\midrule"
)

# Fit statistics
fmt_int <- function(x) if (is.na(x)) "" else format(round(x), big.mark = ",")
fmt_num <- function(x) if (is.na(x)) "" else sprintf("%.1f", x)

lines_out <- c(
  lines_out,
  paste0("N Observations & ", paste(sapply(col_data, function(c) fmt_int(c$fit$N_obs)), collapse = " & "), " \\\\"),
  paste0("N Choice Situations & ", paste(sapply(col_data, function(c) fmt_int(c$fit$N_choice_sits)), collapse = " & "), " \\\\"),
  paste0("Log-Likelihood & ", paste(sapply(col_data, function(c) fmt_num(c$fit$LL)), collapse = " & "), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)

if (!dir.exists(dirname(OUTPUT_TEX))) dir.create(dirname(OUTPUT_TEX), recursive = TRUE)
writeLines(lines_out, OUTPUT_TEX)
message("Saved: ", OUTPUT_TEX)

# Suggested caption/label/notes for the surrounding \begin{table} block in
# the Overleaf doc (not written to OUTPUT_TEX -- add by hand, matching the
# project's existing table style, e.g. tables/ebird_dhs_ttest.tex).
message(
  "\nSuggested caption/label/notes for the surrounding \\begin{table} block:\n",
  "  \\caption{Fixed Effects Robustness: Species Richness WTP}\n",
  "  \\label{tab:fe_robustness}\n",
  "  Note: Standard errors in parentheses. *p<.1, **p<.05, ***p<.01. ",
  "Coefficients labeled (utils) are on the standardized scale as estimated; ",
  "the WTP row converts the Species Richness coefficient to real 2021 INR, ",
  "using log travel cost as the price numeraire. Controls include expected ",
  "congestion, log trip number, group size, precipitation, temperature, and tree cover.",
  if (any_se_missing) " \\dagger Standard errors not available for this specification (numerical Hessian was singular, likely due to reduced fixed-effect absorption leaving more shared variation across covariates; point estimates shown without inference)." else ""
)