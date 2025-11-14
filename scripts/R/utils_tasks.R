# scripts/R/utils_tasks.R
##########################################
# Define core task runner function
##########################################
# Core function to run each task
run_task <- function(script,
                      input_paths = list(),
                      output_paths = list(),
                      params = list(),
                      scenario_name = NULL,
                      skip_if_exists = TRUE,
                      log_file = "logs/processing_times.csv") {
  
  # normalize declared outputs to character vector
  out_paths_char <- unlist(lapply(output_paths, function(x) as.character(x)))
  out_names <- names(output_paths)
  if (is.null(out_names)) out_names <- paste0("out_", seq_along(out_paths_char))
  
  # check existence of each declared output
  exists_vec <- vapply(out_paths_char, function(p) if (nzchar(p)) file.exists(p) else FALSE, logical(1))
  
  # skip if all outputs already exist
  if (skip_if_exists && length(out_paths_char) > 0 && all(exists_vec)) {
    message("Skipping ", script, " (all outputs exist):")
    for (i in seq_along(out_paths_char)) {
      cat("    - ", out_names[i], ": ", out_paths_char[i], "\n")
    }
    
    # Log skipped task with 0 time
    log_task_time(script, 0, scenario_name, log_file, skipped = TRUE)
    return(invisible(TRUE))
  }
  
  message("Running ", script, " ...")
  message("Declared outputs:")
  for (i in seq_along(out_paths_char)) {
    cat("    - ", out_names[i], ": ", out_paths_char[i],
        if (exists_vec[i]) " (exists)\n" else "\n")
  }
  
  env <- new.env(parent = globalenv())
  env$inputs        <- input_paths
  env$outputs       <- output_paths
  env$params        <- params
  env$scenario_name <- scenario_name
  
  # ensure output directories exist
  for (p in out_paths_char) {
    if (!nzchar(p)) next
    d <- dirname(p)
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Start timing
  start_time <- Sys.time()
  
  # run the script and surface errors with a message
  res <- tryCatch({
    sys.source(file.path("scripts", "R", script), envir = env)
    TRUE
  }, error = function(e) {
    message("Error running ", script, ": ", conditionMessage(e))
    stop(e)
  })
  
  # Calculate elapsed time
  end_time <- Sys.time()
  elapsed_seconds <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Log the processing time
  log_task_time(script, elapsed_seconds, scenario_name, log_file)
  
  # report final output status
  final_exists <- vapply(out_paths_char, function(p) if (nzchar(p)) file.exists(p) else FALSE, logical(1))
  message("Finished ", script, " (", round(elapsed_seconds, 2), " seconds). Output status:")
  for (i in seq_along(out_paths_char)) {
    cat("    - ", out_names[i], ": ", out_paths_char[i], "\n")
  }
  
  invisible(TRUE)
}

##########################################
# Helper function to log task times
##########################################
log_task_time <- function(task_name, elapsed_seconds, scenario_name = NULL, 
                          log_file = "logs/processing_times.csv", skipped = FALSE) {
  
  # Ensure log directory exists
  log_dir <- dirname(log_file)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Handle scenario name - check if it's NULL, empty, or NA
  scenario_value <- if (is.null(scenario_name) || is.na(scenario_name) || scenario_name == "") {
    NA_character_
  } else {
    as.character(scenario_name)
  }
  
  # Create data frame for this log entry
  log_entry <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    scenario = scenario_value,
    task = task_name,
    elapsed_seconds = elapsed_seconds,
    status = if (skipped) "skipped" else "completed",
    stringsAsFactors = FALSE
  )
  
  # Append to log file (create if doesn't exist)
  if (file.exists(log_file)) {
    write.table(log_entry, log_file, append = TRUE, sep = ",", 
                row.names = FALSE, col.names = FALSE, quote = TRUE)
  } else {
    write.table(log_entry, log_file, append = FALSE, sep = ",", 
                row.names = FALSE, col.names = TRUE, quote = TRUE)
  }
}

##########################################
# Optional: Function to summarize logs
##########################################
summarize_processing_times <- function(log_file = "logs/processing_times.csv", 
                                       scenario = NULL) {
  
  if (!file.exists(log_file)) {
    message("No log file found at: ", log_file)
    return(invisible(NULL))
  }
  
  logs <- read.csv(log_file, stringsAsFactors = FALSE)
  
  # Filter by scenario if specified
  if (!is.null(scenario)) {
    logs <- logs[logs$scenario == scenario, ]
    if (nrow(logs) == 0) {
      message("No logs found for scenario: ", scenario)
      return(invisible(NULL))
    }
  }
  
  # Summarize by scenario and task
  summary <- aggregate(elapsed_seconds ~ scenario + task, 
                       data = logs[logs$status == "completed", ], 
                       FUN = function(x) c(mean = mean(x), 
                                          min = min(x), 
                                          max = max(x), 
                                          count = length(x)))
  
  print(summary)
  invisible(summary)
}