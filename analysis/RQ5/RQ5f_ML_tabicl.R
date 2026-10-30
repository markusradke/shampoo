rm(list = ls())
gc()

library(dplyr)
library(readr)
library(ggplot2)
library(reticulate)
source("analysis/config.R")
source("analysis/helpers/RQ5/tuning_and_training.R")

# TabICLv2 is a pretrained in-context learner. It is not tuned: each model is
# conditioned on the full training set (train + validation data of a fold).
# Its fit API accepts only X and y, so sample weights cannot be used.
# Requires the project's ".venv" with tabicl and a CUDA-enabled torch.
use_project_python()
tabicl <- import("tabicl", convert = FALSE)
numpy <- import("numpy", convert = FALSE)
torch <- import("torch", convert = FALSE)
if (!isTRUE(py_to_r(torch$cuda$is_available()))) {
  stop("A CUDA-enabled PyTorch installation and an available GPU are required.")
}
message(sprintf("Using CUDA device: %s", py_to_r(torch$cuda$get_device_name(0L))))

as_tabicl_matrix <- function(data, imputation) {
  #' Median-impute the features (fitted on training data, as for the linear SVM) and convert them to a float32 NumPy array.
  x <- as.matrix(data[setdiff(names(data), "track.s.id")])
  storage.mode(x) <- "double"
  numpy$array(apply_median_imputer(x, imputation), dtype = "float32")
}

fit_and_evaluate_tabicl <- function(k, folds, holdout) {
  full_training <- get_full_training_data(folds)
  class_levels <- levels(full_training$y$cat)
  imputation <- fit_median_imputer(as.matrix(
    full_training$X[setdiff(names(full_training$X), "track.s.id")]
  ))
  disk_offload_dir <- sprintf("models/classification/tabicl_disk_offload/k%d", k)
  dir.create(disk_offload_dir, recursive = TRUE, showWarnings = FALSE)
  model <- tabicl$TabICLClassifier(
    device = "cuda",
    use_amp = TRUE,
    use_fa3 = "auto",
    batch_size = 4L, # may be increased to 8L if GPU memory allows
    offload_mode = "disk",
    disk_offload_dir = disk_offload_dir,
    random_state = as.integer(RANDOM_STATE),
    n_jobs = as.integer(N_WORKERS)
  )
  message(sprintf("Fitting TabICLv2 on %d training rows for K=%d", nrow(full_training$X), k))
  model$fit(
    as_tabicl_matrix(full_training$X, imputation),
    r_to_py(as.character(full_training$y$cat))
  )

  probabilities <- as.matrix(py_to_r(
    model$predict_proba(as_tabicl_matrix(holdout$X_holdout, imputation))
  ))
  if (!identical(as.character(py_to_r(model$classes_)), class_levels)) {
    stop(sprintf("TabICL class order differs from prepared labels for K=%d.", k))
  }
  evaluation <- compute_final_evaluation(
    choose_first_max_class(probabilities, class_levels),
    holdout$y_hot_holdout
  )
  evaluation$probabilities <- probabilities
  evaluation
}

for (k in CHOSEN_KS) {
  ml_data <- load_classification_data(k)
  evaluation <- fit_and_evaluate_tabicl(k, ml_data$folds, ml_data$holdout)
  print(evaluation$metrics)
  saveRDS(evaluation, get_model_path("holdout_evaluation_tabicl", k))
  save_figure(
    sprintf("supp_cm_tabicl_k%d.png", k),
    plot = plot_confusion_matrix(evaluation),
    width = ifelse(k > 20, 12, 8),
    height = ifelse(k > 20, 10, 6)
  )
}
