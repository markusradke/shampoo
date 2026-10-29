# Shared settings for all analysis scripts. Run scripts from the repo root.
# Settings read with `get_setting()` can be overridden per machine with an
# environment variable of the same name prefixed by "SHAMPOO_", e.g.
# `SHAMPOO_N_WORKERS=30 Rscript analysis/RQ5e_ML_linear_svm.R`.

get_setting <- function(name, default) {
  value <- Sys.getenv(paste0("SHAMPOO_", name), unset = NA)
  if (is.na(value)) default else methods::as(value, class(default))
}

RANDOM_STATE <- 42
LAMBDA <- 0.5
CHOSEN_KS <- c(5, 17, 31) # chosen from the RQ1 Gini trajectory
RUN_TAG <- get_setting("RUN_TAG", "rev1") # separates tuning runs and models

N_WORKERS <- get_setting(
  "N_WORKERS",
  # max(1L, parallel::detectCores(logical = FALSE) - 1L)
  max(1L, 10L)
)
N_TRIALS <- get_setting("N_TRIALS", 100L) # LightGBM trials and SVM grid size
N_REPEATS <- get_setting("N_REPEATS", 20L) # RQ3 split and PFI repetitions
DEBUGGING <- get_setting("DEBUG", FALSE) # subsample ML data for smoke runs

start_workers <- function(n_workers = N_WORKERS, fork = FALSE) {
  #' Start parallel workers. With `fork = TRUE`, Unix systems fork the running R process (no new R sessions, loaded packages and data are shared); Windows always starts new R sessions. Do not fork after LightGBM has run in the main process.
  if (fork && future::supportsMulticore()) {
    future::plan(future::multicore, workers = n_workers)
  } else {
    future::plan(future::multisession, workers = n_workers)
  }
}

use_project_python <- function() {
  python_path <- file.path(
    ".venv",
    if (.Platform$OS.type == "windows") "Scripts/python.exe" else "bin/python3"
  )
  Sys.setenv(RETICULATE_PYTHON = python_path)
}

get_model_path <- function(model_name, k, extension = "rds") {
  sprintf(
    "models/classification/%s_%s_k%d.%s",
    model_name,
    RUN_TAG,
    k,
    extension
  )
}

save_figure <- function(filename, plot = ggplot2::last_plot(), ...) {
  ggplot2::ggsave(
    file.path("reports/figures", filename),
    plot = plot,
    dpi = 600,
    ...
  )
}
