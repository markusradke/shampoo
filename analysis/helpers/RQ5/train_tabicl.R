source("analysis/helpers/RQ5/train_shared.R")

# TabICLv2 is a pretrained in-context learner. It is not tuned: each model is
# conditioned on the full training set (train + validation data of a fold).
# Its fit API accepts only X and y, so sample weights cannot be used.
# Requires the project's ".venv" with tabicl and a CUDA-enabled torch.
import_tabicl <- function() {
  #' Import the Python modules of TabICL and check that a CUDA GPU is available.
  use_project_python()
  torch <- reticulate::import("torch", convert = FALSE)
  if (!isTRUE(reticulate::py_to_r(torch$cuda$is_available()))) {
    stop(
      "A CUDA-enabled PyTorch installation and an available GPU are required."
    )
  }
  list(
    tabicl = reticulate::import("tabicl", convert = FALSE),
    numpy = reticulate::import("numpy", convert = FALSE),
    device_name = reticulate::py_to_r(torch$cuda$get_device_name(0L))
  )
}

as_tabicl_matrix <- function(data, imputation, numpy) {
  #' Median-impute the features (fitted on training data, as for the linear SVM) and convert them to a float32 NumPy array.
  x <- as.matrix(data[setdiff(names(data), "track.s.id")])
  storage.mode(x) <- "double"
  numpy$array(apply_median_imputer(x, imputation), dtype = "float32")
}

fit_and_evaluate_tabicl <- function(k, feature_set, folds, holdout) {
  #' Fit TabICLv2 on the full training set and evaluate it on the holdout data.
  python <- import_tabicl()
  full_training <- get_full_training_data(folds)
  class_levels <- levels(full_training$y$cat)
  imputation <- fit_median_imputer(as.matrix(
    full_training$X[setdiff(names(full_training$X), "track.s.id")]
  ))
  disk_offload_dir <- tools::file_path_sans_ext(
    get_model_path("tabicl_disk_offload", k, feature_set)
  )
  dir.create(disk_offload_dir, recursive = TRUE, showWarnings = FALSE)
  model <- python$tabicl$TabICLClassifier(
    device = "cuda",
    use_amp = TRUE,
    use_fa3 = "auto",
    batch_size = 4L, # may be increased to 8L if GPU memory allows
    offload_mode = "disk",
    disk_offload_dir = disk_offload_dir,
    random_state = as.integer(RANDOM_STATE),
    n_jobs = as.integer(N_WORKERS)
  )
  message(sprintf(
    "Fitting TabICLv2 on %d training rows for %s, K=%d (%s)",
    nrow(full_training$X),
    feature_set,
    k,
    python$device_name
  ))
  model$fit(
    as_tabicl_matrix(full_training$X, imputation, python$numpy),
    reticulate::r_to_py(as.character(full_training$y$cat))
  )
  probabilities <- as.matrix(reticulate::py_to_r(model$predict_proba(
    as_tabicl_matrix(holdout$X_holdout, imputation, python$numpy)
  )))
  if (
    !identical(as.character(reticulate::py_to_r(model$classes_)), class_levels)
  ) {
    stop(sprintf(
      "TabICL class order differs from prepared labels for K=%d.",
      k
    ))
  }
  unlink(disk_offload_dir, recursive = TRUE)
  evaluation <- compute_final_evaluation(
    choose_first_max_class(probabilities, class_levels),
    holdout$y_hot_holdout
  )
  evaluation$probabilities <- probabilities
  evaluation
}
