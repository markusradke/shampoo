rm(list = ls())
gc()
library(readr)
library(dplyr)
library(ggplot2)
library(lightgbm)
library(LiblineaR)
library(reticulate)
source("analysis/config.R")
source("analysis/helpers/RQ5/run_ml_jobs.R")

# Runs the ML jobs (tuning, final training, holdout evaluation, PFI) of one
# machine: `Rscript analysis/RQ5/RQ5d_ML_train_and_evaluate.R ML1`.
# Finished jobs are skipped, interrupted LightGBM tunings are resumed. All
# results are stored in `models/classification/<RUN_TAG>/`; copy these folders
# of all machines together and run RQ5e for the tables and figures.

# JOBS PER MACHINE ----
# balanced for ML1 (32-core CPU) and ML2 (24-core CPU, GPU for TabICL)
ML_JOBS <- bind_rows(
  tibble(
    machine = "ML1",
    model = "lgbm",
    feature_set = c("all", "no_spotify_genres", "all"),
    k = c(17, 17, 31)
  ),
  expand.grid(
    machine = "ML2",
    k = CHOSEN_KS,
    feature_set = FEATURE_SETS,
    model = c("tabicl", "linear_svm"),
    stringsAsFactors = FALSE
  ) |>
    arrange(desc(model)),
  tibble(
    machine = "ML2",
    model = "lgbm",
    feature_set = c("all", "no_spotify_genres", "no_spotify_genres"),
    k = c(5, 5, 31)
  )
)
RUN_JOB <- list(
  lgbm = run_lightgbm_job,
  linear_svm = run_linear_svm_job,
  tabicl = run_tabicl_job
)

machine <- commandArgs(trailingOnly = TRUE)[1]
if (!machine %in% ML_JOBS$machine) {
  stop(sprintf(
    "Usage: Rscript analysis/RQ5/RQ5d_ML_train_and_evaluate.R <machine>, with machine one of %s",
    paste(unique(ML_JOBS$machine), collapse = ", ")
  ))
}
options(shampoo.machine = machine)
jobs <- filter(ML_JOBS, machine == !!machine) |>
  mutate(job = sprintf("%s %s k%d", model, feature_set, k))

# PREFLIGHT ----
# fails within minutes instead of after hours of tuning
data_sets <- distinct(jobs, k, feature_set)
for (i in seq_len(nrow(data_sets))) {
  ml_data <- load_classification_data(data_sets$k[i], data_sets$feature_set[i])
  feature_names <- get_feature_names(ml_data$holdout$X_holdout)
  fold_feature_names <- lapply(
    c(ml_data$folds$X_train, ml_data$folds$X_val),
    get_feature_names
  )
  if (!all(vapply(fold_feature_names, identical, logical(1), feature_names))) {
    stop(sprintf(
      "Features of folds and holdout differ for k=%d.",
      data_sets$k[i]
    ))
  }
  get_feature_groups(feature_names) # stops if the feature groups do not match
}
rm(ml_data)
gc()
if ("lgbm" %in% jobs$model) {
  use_project_python()
  optuna <- import("optuna") # used by tune_lightgbm_model()
}
gpu <- if ("tabicl" %in% jobs$model) import_tabicl()$device_name else "not used"
send_notification(
  sprintf("Started %d jobs", nrow(jobs)),
  sprintf(
    "Preflight OK.\nWorkers: %d, trials: %d, PFI repetitions: %d, debugging: %s\nGPU: %s\n\nJobs:\n%s",
    N_WORKERS,
    N_TRIALS,
    N_REPEATS,
    DEBUGGING,
    gpu,
    paste(jobs$job, collapse = "\n")
  )
)

# JOBS ----
start_workers() # CV folds, SVM costs, and PFI feature groups run in parallel
run_start_time <- Sys.time()
jobs$status <- NA_character_
for (i in seq_len(nrow(jobs))) {
  job <- jobs[i, ]
  evaluation_path <- get_model_path(
    paste0("holdout_evaluation_", job$model),
    job$k,
    job$feature_set
  )
  if (file.exists(evaluation_path)) {
    message(sprintf("Skipping finished job %s", job$job))
    jobs$status[i] <- "skipped"
    next
  }
  job_start_time <- Sys.time()
  jobs$status[i] <- tryCatch(
    {
      RUN_JOB[[job$model]](job$k, job$feature_set)
      send_notification(
        sprintf("Finished %s", job$job),
        sprintf(
          "Duration: %.1f h\n\n%s",
          as.numeric(difftime(Sys.time(), job_start_time, units = "hours")),
          describe_job_results(job$model, job$k, job$feature_set)
        )
      )
      "done"
    },
    error = function(error) {
      send_notification(
        sprintf("FAILED %s", job$job),
        sprintf(
          "Error: %s\nCall: %s\nThe remaining jobs continue.",
          conditionMessage(error),
          paste(deparse(conditionCall(error)), collapse = " ")
        )
      )
      "failed"
    }
  )
  gc()
}
future::plan(future::sequential)

send_notification(
  sprintf("Run finished: %d failed jobs", sum(jobs$status == "failed")),
  sprintf(
    "Duration: %.1f h\nResults: models/classification/%s/\n\n%s",
    as.numeric(difftime(Sys.time(), run_start_time, units = "hours")),
    RUN_TAG,
    format_table(select(jobs, job, status))
  )
)
if (any(jobs$status == "failed")) {
  quit(status = 1)
}
