# scripts/R/utils_xlogit_reticulate.R
##########################################
# Reticulate-based XLogit Estimation Helper
##########################################
# Replaces utils_xlogit_subprocess.R. Runs xlogit estimation in-process via
# reticulate instead of launching a Python subprocess and exchanging data
# through JSON/CSV/text files.
#
# Demeaning (fixed-effect absorption) now happens entirely inside the Python
# module (estimate_rum_xlogit.py), computed directly with pandas on the
# prepped parquet from 11a. There is no global-means JSON hand-off anymore --
# that was the source of the R(data.table)/Python(dict) shape mismatch.

library(reticulate)

#' Point reticulate at the conda environment with xlogit installed.
#' Call once per R session, before estimate_rum_models_reticulate().
setup_xlogit_env <- function(conda_env_name = "wtp01") {

  # Force single-threaded BLAS/OpenMP on the Python side, set BEFORE Python is
  # touched at all. Running Python in-process (via reticulate) rather than as
  # a separate OS process means R's own linked BLAS thread pool and numpy's
  # BLAS/OpenMP thread pool now share one process -- on macOS this reliably
  # segfaults (pthread_mutex_init / OMP Error #179) the moment xlogit starts
  # its actual likelihood computation. This didn't happen under the old
  # subprocess approach because Python ran as a fully separate process there.
  Sys.setenv(OMP_NUM_THREADS = "1")
  Sys.setenv(OPENBLAS_NUM_THREADS = "1")
  Sys.setenv(MKL_NUM_THREADS = "1")
  Sys.setenv(VECLIB_MAXIMUM_THREADS = "1")
  Sys.setenv(NUMEXPR_NUM_THREADS = "1")
  Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")

  # Resolve the target interpreter directly from conda, then force-assert
  # RETICULATE_PYTHON immediately before Python is actually initialized.
  #
  # This is a defensive re-assertion, not just a sanity check: on at least
  # one dev machine, something outside this repo (most likely radian's own
  # Python bridge, rchitect) has been observed resetting RETICULATE_PYTHON
  # partway through the R session -- even when a project-level .Renviron
  # sets it correctly at startup. Re-setting it here, right before the first
  # Python-touching reticulate call, sidesteps that regardless of the cause,
  # since reticulate only locks in an interpreter on first actual use.
  target_python <- tryCatch(
    reticulate::conda_python(conda_env_name),
    error = function(e) NULL
  )

  if (!is.null(target_python) && file.exists(target_python)) {
    current <- Sys.getenv("RETICULATE_PYTHON", unset = NA)
    already_correct <- !is.na(current) &&
      identical(normalizePath(current, mustWork = FALSE),
                normalizePath(target_python, mustWork = FALSE))
    if (!already_correct) {
      message("Forcing RETICULATE_PYTHON -> ", target_python,
              if (!is.na(current)) paste0("  (was: ", current, ")") else "")
      Sys.setenv(RETICULATE_PYTHON = target_python)
    }
  } else {
    message("NOTE: could not resolve a conda python for env '", conda_env_name,
            "' via reticulate::conda_python(). Falling back to use_condaenv() only.")
  }

  reticulate::use_condaenv(conda_env_name, required = TRUE)
  message("Using Python: ", reticulate::py_config()$python)

  # Fail fast with a clear message if required packages aren't importable,
  # rather than discovering it partway through estimation
  tryCatch({
    reticulate::py_run_string("import numpy, pandas, scipy, yaml, xlogit")
  }, error = function(e) {
    stop(
      "Active Python environment is missing required packages (numpy/pandas/",
      "scipy/yaml/xlogit).\n",
      "  Active Python: ", reticulate::py_config()$python, "\n",
      "  This is commonly caused by RETICULATE_PYTHON overriding use_condaenv() ",
      "-- see note above if printed.\n",
      "  Original error: ", conditionMessage(e),
      call. = FALSE
    )
  })

  invisible(TRUE)
}

#' Run xlogit estimation for one scenario, across every model in models.yml.
#'
#' @param scenario_name    Scenario name (e.g. "full_c5km_v5km_r50km_mInf")
#' @param input_data_path  Path to model_data_{scenario}.parquet (from 11a)
#' @param output_dir       Directory for coefficients/wtp/summary outputs
#' @param models_config    Path to models.yml
#' @param save_demeaned    If TRUE, also save the sampled+demeaned dataset per
#'                         model to output_dir/demeaned/ for debugging
#' @param python_script    Path to the python estimation module
#'
#' @return A list with success (logical), success_count, fail_count, and
#'         summary (a data.frame of LL/AIC/BIC/N_obs/N_choice_sits per model)
estimate_rum_models_reticulate <- function(scenario_name,
                                            input_data_path,
                                            output_dir,
                                            models_config = "models.yml",
                                            save_demeaned = TRUE,
                                            python_script = file.path("scripts", "python", "estimate_rum_xlogit.py")) {

  if (!file.exists(input_data_path)) {
    stop("Input data not found: ", input_data_path)
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(python_script)) {
    stop("Python estimation script not found: ", python_script)
  }

  # Import as a proper Python module rather than source_python(). source_python()
  # executes the file the same way `python file.py` does, which sets __name__ to
  # "__main__" -- so the argparse CLI guard at the bottom of the script would fire
  # on every call from R and fail looking for command-line args that don't exist.
  # import_from_path() uses real import machinery, so __name__ is the module name
  # and that guard correctly stays dormant.
  py_dir <- dirname(python_script)
  py_module_name <- tools::file_path_sans_ext(basename(python_script))
  py <- reticulate::import_from_path(py_module_name, path = py_dir, convert = TRUE)

  message("\n[RETICULATE] Running xlogit estimation for scenario: ", scenario_name)

  result <- py$run_all_models(
    scenario = scenario_name,
    input_data_path = input_data_path,
    output_dir = output_dir,
    models_config_path = models_config,
    save_demeaned = save_demeaned
  )

  message(sprintf("[RETICULATE] Done: %d succeeded, %d failed",
                  result$success_count, result$fail_count))

  list(
    success = (result$fail_count == 0),
    success_count = result$success_count,
    fail_count = result$fail_count,
    summary = result$summary_df  # data.frame: Model, Scenario, N_obs, N_choice_sits, LL, AIC, BIC
  )
}