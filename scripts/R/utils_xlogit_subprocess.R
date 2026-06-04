# utils_xlogit_subprocess.R
# Helper function for calling Python XLogit estimation from R

call_xlogit_estimation <- function(
    scenario_name,
    scenario_dir,
    input_data_path,
    output_dir,
    conda_env_name = "wtp01",
    models_config = "models_simple.yml",
    verbose = TRUE) {
  
  # Call Python XLogit script as subprocess for a single scenario.
  # 
  # Args:
  #   scenario_name: Name of scenario (e.g., "L_c5km_v5km_r50km_mInf")
  #   scenario_dir: Full path to scenario directory
  #   input_data_path: Full path to master_data_with_travel_cost.parquet
  #   output_dir: Output directory for results (CSV, TXT files)
  #   conda_env_name: Name of conda environment with xlogit installed
  #   models_config: Models YAML config file
  #   verbose: Print subprocess output to console
  # 
  # Returns:
  #   List with:
  #     - success: logical TRUE/FALSE
  #     - exit_code: integer exit code
  #     - stdout: captured stdout text
  #     - stderr: captured stderr text
  #     - log_file: path to saved log file
  
  # Construct file paths
  python_script <- file.path("scripts", "python", "11b_estimate_rum_xlogit.py")
  log_dir <- file.path(output_dir, "logs")
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(log_dir, paste0(scenario_name, "_", timestamp, ".log"))
  
  # Build Python command with all arguments
  cmd <- c(
    "conda", "run", "-n", conda_env_name,
    "python", python_script,
    "--scenario", scenario_name,
    "--input-data", input_data_path,
    "--output-dir", output_dir,
    "--models-config", models_config
  )
  
  if (verbose) {
    message(paste0("\n[XLOGIT] Starting Python subprocess for scenario: ", scenario_name))
    message(paste0("[XLOGIT] Log file: ", log_file))
  }
  
  # Build command - use bash -l to initialize conda properly
  cmd_string <- paste(
    "export PYTHONUNBUFFERED=1;",
    "cd", shQuote(getwd()), ";",
    "conda run -n", conda_env_name,
    "python", shQuote(python_script),
    "--scenario", scenario_name,
    "--input-data", shQuote(input_data_path),
    "--output-dir", shQuote(output_dir),
    "--models-config", models_config
  )
  
  # Wrap in bash -l for login shell (initializes conda)
  full_cmd <- paste("bash -l -c", shQuote(cmd_string))
  
  if (verbose) {
    message(paste0("[XLOGIT] Executing command..."))
  }
  
  # Run with bash login shell - output streams to console in real-time
  exit_code <- system(full_cmd)
  
  # After completion, try to read any log files that were created
  log_files <- list.files(file.path(output_dir, "logs"), pattern = scenario_name, full.names = TRUE)
  stdout_text <- ""
  stderr_text <- ""
  
  if (length(log_files) > 0) {
    # Try to read the most recent log file
    latest_log <- log_files[which.max(file.info(log_files)$mtime)]
    if (file.exists(latest_log)) {
      log_content <- readLines(latest_log, warn = FALSE)
      stdout_text <- paste(log_content, collapse = "\n")
    }
  }
  
  success <- (exit_code == 0)
  
  # Save log file (ensure directory exists again before writing)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  log_content <- c(
    paste0("XLOGIT Subprocess Log - ", Sys.time()),
    paste0("Scenario: ", scenario_name),
    paste0("Command: ", paste(cmd, collapse = " ")),
    paste0("Exit Code: ", exit_code),
    "",
    "=== STDOUT ===",
    stdout_text,
    "",
    "=== STDERR ===",
    stderr_text
  )
  
  tryCatch(
    writeLines(log_content, log_file),
    error = function(e) {
      warning(paste0("Could not write log file to ", log_file, ": ", e$message))
    }
  )
  
  if (verbose) {
    if (success) {
      message(paste0("[XLOGIT] ✓ Subprocess completed successfully (exit code 0)"))
    } else {
      message(paste0("[XLOGIT] ✗ Subprocess failed (exit code ", exit_code, ")"))
      message(paste0("[XLOGIT] Check log: ", log_file))
    }
  }
  
  # Verify output files exist
  expected_patterns <- c("*_wtp.csv", "*_coefficients.csv", "*_summary.txt")
  output_files <- list.files(output_dir, pattern = paste0(scenario_name, ".*\\.(csv|txt)$"))
  
  if (length(output_files) == 0) {
    warning(paste0("No output files found for scenario ", scenario_name, " in ", output_dir))
  } else if (verbose) {
    message(paste0("[XLOGIT] Output files created: ", length(output_files)))
  }
  
  return(list(
    success = success,
    exit_code = exit_code,
    stdout = stdout_text,
    stderr = stderr_text,
    log_file = log_file,
    output_files = output_files
  ))
}