#!/usr/bin/env Rscript
# scripts/R/12_generate_summary_report.R
#############################################
#  - Generates a Quarto HTML report summarizing all scenarios
#  - Includes tabs for each scenario with maps, tables, and model results
#  - Saves: outputs$summary_report (HTML)
#############################################

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

message("Generating summary report across all scenarios...")

scenarios <- params$scenarios
output_dir <- inputs$scenario_outputs

message("Scenarios: ", paste(scenarios, collapse = ", "))
message("Output directory: ", output_dir)

# Verify scenario directories exist
for (scenario in scenarios) {
  scenario_dir <- file.path(output_dir, scenario)
  if (dir.exists(scenario_dir)) {
    message("  Found scenario directory: ", scenario_dir)
    # Check for expected files
    wtp_file <- file.path(scenario_dir, "tables", "wtp_comparison.csv")
    voronoi_file <- file.path(scenario_dir, "figures", "voronoi_map.png")
    message("    WTP file exists: ", file.exists(wtp_file))
    message("    Voronoi file exists: ", file.exists(voronoi_file))
  } else {
    warning("  Scenario directory not found: ", scenario_dir)
  }
}

# -----------------------------------------------------------------------------
# Create Quarto Document
# -----------------------------------------------------------------------------

qmd_content <- paste0('---
title: "Biodiversity WTP Analysis - Summary Report"
date: "`r Sys.Date()`"
format: 
  html:
    toc: true
    toc-depth: 3
    code-fold: true
    embed-resources: true
    theme: cosmo
execute:
  echo: false
  warning: false
  message: false
---

```{r setup}
library(knitr)
library(dplyr)
library(DT)
library(ggplot2)

# Define scenarios and output directory
scenarios <- c("', paste(scenarios, collapse = '", "'), '")
output_dir <- "', output_dir, '"
```

# Overview

This report summarizes the Random Utility Model (RUM) analysis across multiple scenarios.

**Number of scenarios:** `r length(scenarios)`

**Scenarios analyzed:** `r paste(scenarios, collapse = ", ")`

---

')

# Add sections organized by output type, with tabs for each scenario
qmd_content <- paste0(qmd_content, '

# Voronoi Clusters
:::{.panel-tabset}

')

for (scenario in scenarios) {
  scenario_dir <- file.path(output_dir, scenario)
  voronoi_path <- normalizePath(file.path(scenario_dir, "figures", "voronoi_map.png"), mustWork = FALSE)
  
  qmd_content <- paste0(qmd_content, '

## ', scenario, '

```{r voronoi-', gsub("[^A-Za-z0-9]", "", scenario), '}
voronoi_path <- "', voronoi_path, '"
if (file.exists(voronoi_path)) {
  knitr::include_graphics(voronoi_path)
} else {
  cat("Voronoi map not found for scenario:", "', scenario, '\\n")
  cat("Expected path:", voronoi_path, "\\n")
}
```

')
}
qmd_content <- paste0(qmd_content, ':::')

qmd_content <- paste0(qmd_content, '

# Data Summary
:::{.panel-tabset}
')

for (scenario in scenarios) {
  scenario_dir <- file.path(output_dir, scenario)
  data_summary_path <- normalizePath(file.path(scenario_dir, "tables", "data_summary.csv"), mustWork = FALSE)
  
  qmd_content <- paste0(qmd_content, '

## ', scenario, '

```{r data-summary-', gsub("[^A-Za-z0-9]", "", scenario), '}
data_summary_path <- "', data_summary_path, '"
if (file.exists(data_summary_path)) {
  data_summary <- read.csv(data_summary_path)
  datatable(data_summary, 
            options = list(pageLength = 20, scrollX = TRUE),
            caption = "Data Summary Statistics")
} else {
  cat("Data summary not found for scenario:", "', scenario, '\\n")
  cat("Expected path:", data_summary_path, "\\n")
}
```
')
}
qmd_content <- paste0(qmd_content, ':::')


qmd_content <- paste0(qmd_content, '

# WTP Comparison 
:::{.panel-tabset}
')

for (scenario in scenarios) {
  scenario_dir <- file.path(output_dir, scenario)
  wtp_path <- normalizePath(file.path(scenario_dir, "tables", "wtp_comparison.csv"), mustWork = FALSE)
  
  qmd_content <- paste0(qmd_content, '

## ', scenario, '

```{r wtp-', gsub("[^A-Za-z0-9]", "", scenario), '}
wtp_path <- "', wtp_path, '"
if (file.exists(wtp_path)) {
  wtp_table <- read.csv(wtp_path)
  
  # Format for display
  wtp_display <- wtp_table
  numeric_cols <- sapply(wtp_display, is.numeric)
  wtp_display[numeric_cols] <- lapply(wtp_display[numeric_cols], function(x) {
    round(x, 2)
  })
  
  datatable(wtp_display,
            options = list(pageLength = 15, scrollX = TRUE),
            caption = "Willingness to Pay Estimates Across Models") %>%
    formatStyle(columns = names(wtp_display), fontSize = "90%")
} else {
  cat("WTP comparison not found for scenario:", "', scenario, '\\n")
  cat("Expected path:", wtp_path, "\\n")
}
```
')
}
qmd_content <- paste0(qmd_content, ':::')


qmd_content <- paste0(qmd_content, '

# Model Fit
:::{.panel-tabset}
')

for (scenario in scenarios) {
  scenario_dir <- file.path(output_dir, scenario)
  model_fit_path <- normalizePath(file.path(scenario_dir, "tables", "model_fit.csv"), mustWork = FALSE)
  
  qmd_content <- paste0(qmd_content, '

## ', scenario, '

```{r model-fit-', gsub("[^A-Za-z0-9]", "", scenario), '}
model_fit_path <- "', model_fit_path, '"
if (file.exists(model_fit_path)) {
  model_fit <- read.csv(model_fit_path)
  
  kable(model_fit, 
        digits = 2,
        caption = "Model Fit Statistics") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
} else {
  cat("Model fit statistics not found for scenario:", "', scenario, '\\n")
  cat("Expected path:", model_fit_path, "\\n")
}
```
')
}
qmd_content <- paste0(qmd_content, ':::')


# Add comparison section
qmd_content <- paste0(qmd_content, '

# Cross-Scenario Comparison 

## WTP Comparison

```{r cross-scenario-wtp}
# Load all WTP tables
wtp_list <- list()
for (scenario in scenarios) {
  wtp_path <- file.path(output_dir, scenario, "tables", "wtp_comparison.csv")
  if (file.exists(wtp_path)) {
    wtp <- read.csv(wtp_path)
    wtp$scenario <- scenario
    wtp_list[[scenario]] <- wtp
  }
}

if (length(wtp_list) > 0) {
  wtp_combined <- bind_rows(wtp_list)
  
  # Create comparison plot for each variable
  if ("Basic" %in% names(wtp_combined)) {
    p <- ggplot(wtp_combined, aes(x = scenario, y = Basic, fill = scenario)) +
      geom_col() +
      facet_wrap(~variable, scales = "free_y") +
      labs(title = "WTP Comparison Across Scenarios (Basic Model)",
           x = "Scenario", y = "WTP (INR)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    print(p)
  }
  
  # Show table
  datatable(wtp_combined,
            options = list(pageLength = 25, scrollX = TRUE),
            caption = "All WTP Estimates Across Scenarios")
} else {
  cat("No WTP data available for comparison")
}
```

## Model Fit Comparison

```{r cross-scenario-fit}
# Load all model fit tables
fit_list <- list()
for (scenario in scenarios) {
  fit_path <- file.path(output_dir, scenario, "tables", "model_fit.csv")
  if (file.exists(fit_path)) {
    fit <- read.csv(fit_path)
    fit$scenario <- scenario
    fit_list[[scenario]] <- fit
  }
}

if (length(fit_list) > 0) {
  fit_combined <- bind_rows(fit_list)
  
  # Plot comparison
  p <- ggplot(fit_combined, aes(x = model, y = log_likelihood, fill = scenario)) +
    geom_col(position = "dodge") +
    labs(title = "Model Log-Likelihood Comparison",
         x = "Model Type", y = "Log-Likelihood") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
  
  # Show table
  kable(fit_combined, 
        digits = 2,
        caption = "Model Fit Statistics Across Scenarios") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
} else {
  cat("No model fit data available for comparison")
}
```

---

# Session Info

```{r session-info}
sessionInfo()
```
')

# -----------------------------------------------------------------------------
# Write Quarto File
# -----------------------------------------------------------------------------

qmd_file <- tempfile(fileext = ".qmd")
writeLines(qmd_content, qmd_file)

message("Quarto document created: ", qmd_file)

# -----------------------------------------------------------------------------
# Render Report
# -----------------------------------------------------------------------------
message("Rendering HTML report...")

# Create output directory if needed
output_dir_report <- dirname(outputs$summary_report)
if (!dir.exists(output_dir_report)) {
  dir.create(output_dir_report, recursive = TRUE)
}

# Copy qmd file to output directory (Quarto renders in-place)
qmd_final <- file.path(output_dir_report, "summary_report.qmd")
file.copy(qmd_file, qmd_final, overwrite = TRUE)

# Render in the output directory
quarto::quarto_render(
  input = qmd_final
)

# Clean up only the temp qmd file
unlink(qmd_file)
# Don't delete qmd_final - leave it in output directory

message("\n=== SUMMARY REPORT GENERATION COMPLETE ===")
message("Report location: ", outputs$summary_report)
message("Source file: ", qmd_final)