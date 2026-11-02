# Shared settings for all analysis scripts. Run scripts from the repo root.
# Settings read with `get_setting()` can be overridden per machine with an
# environment variable of the same name prefixed by "SHAMPOO_", e.g.
# `SHAMPOO_N_WORKERS=30 Rscript analysis/RQ5/RQ5d_ML_train_and_evaluate.R ML1`.
#
# Optional email notifications of the ML runs (RQ5d) are configured with these
# environment variables, best set in `~/.Renviron` of each machine (never in
# the repo). Without them, the notifications are only written to the log.
# NOTIFICATION_SMTP_URL: SMTP server of your mail hoster, found on its help
#   page "set up an email client" (IMAP/SMTP settings) or in the mailbox
#   settings of its customer panel. Use "smtps://<host>:465" for SSL/TLS or
#   "smtp://<host>:587" for STARTTLS.
# NOTIFICATION_SMTP_USER: login of the mailbox, usually the full address.
# NOTIFICATION_SMTP_PASSWORD: password of the mailbox (or an app password).
# NOTIFICATION_MAIL_TO: recipient of the notifications.

get_setting <- function(name, default) {
  value <- Sys.getenv(paste0("SHAMPOO_", name), unset = NA)
  if (is.na(value)) default else methods::as(value, class(default))
}

RANDOM_STATE <- 42
LAMBDA <- 0.5
CHOSEN_KS <- c(5, 17, 31) # chosen from the RQ1 Gini trajectory
FEATURE_SETS <- c("all", "no_spotify_genres") # ablation without "sp_*" features
RUN_TAG <- get_setting("RUN_TAG", "rev1") # separates tuning runs and models

count_physical_cores <- function() {
  #' Count the physical CPU cores. On Linux, `parallel::detectCores(logical = FALSE)` ignores `logical` and counts hyperthreads, so the unique (socket, core) pairs of /proc/cpuinfo are counted there.
  if (!file.exists("/proc/cpuinfo")) {
    return(parallel::detectCores(logical = FALSE))
  }
  cpuinfo <- readLines("/proc/cpuinfo")
  cores <- unique(paste(
    grep("^physical id", cpuinfo, value = TRUE),
    grep("^core id", cpuinfo, value = TRUE)
  ))
  if (length(cores) > 0) length(cores) else parallel::detectCores()
}

N_WORKERS <- get_setting("N_WORKERS", max(1L, count_physical_cores() - 1L))
N_TRIALS <- get_setting("N_TRIALS", 100L) # LightGBM trials and SVM grid size
N_REPEATS <- get_setting("N_REPEATS", 20L) # RQ3 split and PFI repetitions
DEBUGGING <- get_setting("DEBUG", FALSE) # subsample ML data for smoke runs

start_workers <- function(n_workers = N_WORKERS) {
  #' Start parallel workers as new R sessions (forking is unstable with multithreaded libraries and large heaps). Implicit BLAS / OpenMP / data.table threads are limited to one per worker; explicit thread counts like LightGBM's `num_threads` still apply.
  # workers inherit the environment variables of the main process (libraries already loaded there are not affected)
  Sys.setenv(
    OMP_NUM_THREADS = 1,
    OPENBLAS_NUM_THREADS = 1,
    MKL_NUM_THREADS = 1,
    R_DATATABLE_NUM_THREADS = 1
  )
  future::plan(future::multisession, workers = n_workers)
}

with_worker_progress <- function(expr) {
  #' Evaluate `expr` and report the progress signaled by `progressr::progressor()` in the workers live: as progress bar in interactive sessions, else as one timestamped line per update (e.g. for log files of `Rscript`).
  old_options <- options(progressr.enable = TRUE)
  on.exit(options(old_options))
  handler <- if (interactive()) {
    progressr::handler_cli()
  } else {
    progressr::handler_debug()
  }
  progressr::with_progress(expr, handlers = handler, enable = TRUE)
}

use_project_python <- function() {
  python_path <- file.path(
    ".venv",
    if (.Platform$OS.type == "windows") "Scripts/python.exe" else "bin/python3"
  )
  Sys.setenv(RETICULATE_PYTHON = python_path)
}

get_model_path <- function(model_name, k, feature_set, extension = "rds") {
  #' Get the path of an ML result. All results of a run are stored in one folder, so the results of several machines can be merged by copying.
  path <- sprintf(
    "models/classification/%s/%s_%s_k%d.%s",
    RUN_TAG,
    model_name,
    feature_set,
    k,
    extension
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  path
}

save_figure <- function(filename, plot = ggplot2::last_plot(), ...) {
  ggplot2::ggsave(
    file.path("reports/figures", filename),
    plot = plot,
    dpi = 600,
    ...
  )
}
