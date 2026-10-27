rm(list = ls())
gc()

library(dplyr)
library(readr)
library(reticulate)
library(yardstick)

# TabICLv2 is a pretrained in-context learner. It is not tuned on the
# validation folds: each model is conditioned on train + validation data.
# The current fit API accepts only X and y, sampling weights cannot be used.
# Assumes that the virtual environment ".venv" has been created in the project folder and the required packages have been installed
python_path <- file.path(
  ".venv",
  if (.Platform$OS.type == "windows") "Scripts/python.exe" else "bin/python3"
)
Sys.setenv(RETICULATE_PYTHON = python_path)
py_config()

tabicl <- import("tabicl", convert = FALSE)
numpy <- import("numpy", convert = FALSE)
torch <- import("torch", convert = FALSE)
if (!isTRUE(py_to_r(torch$cuda$is_available()))) {
  stop("A CUDA-enabled PyTorch installation and an available GPU are required.")
}
message(sprintf(
  "Using CUDA device: %s",
  py_to_r(torch$cuda$get_device_name(0L))
))

source("analysis/helpers/RQ5/evaluate_classifiers.R")

RANDOM_STATE <- 42
PHYSICAL_CORES <- 32 # Hendrix has 32 physical cores
KS <- c(5, 15, 31)
DEBUGGING <- TRUE
N_DEBUGGING_ROWS <- 5000
TABICL_OFFLOAD_ROOT <- file.path(
  "models",
  "classification",
  "tabicl_disk_offload"
)
figure_number_lookup <- c(
  "5" = "fig09",
  "15" = "fig10",
  "31" = "fig11"
)

if (!file.exists("models/classification")) {
  dir.create("models/classification", recursive = TRUE)
}

# NumPy input is assumed to be encoded and imputed by TabICLv2. Use the same
# training-only median imputation as the linear SVM for a consistent comparison.
fit_tabicl_imputer <- function(data) {
  feature_names <- setdiff(names(data), "track.s.id")
  if (!all(vapply(data[feature_names], is.numeric, logical(1)))) {
    stop("TabICL features must be numeric after preprocessing.")
  }
  medians <- vapply(
    data[feature_names],
    median,
    numeric(1),
    na.rm = TRUE
  )
  if (anyNA(medians)) {
    stop(sprintf(
      "TabICL training features are entirely missing: %s",
      paste(names(medians)[is.na(medians)], collapse = ", ")
    ))
  }
  list(feature_names = feature_names, median = medians)
}

apply_tabicl_imputer <- function(data, imputer) {
  missing_features <- setdiff(imputer$feature_names, names(data))
  if (length(missing_features) > 0) {
    stop(sprintf(
      "Missing TabICL features: %s",
      paste(missing_features, collapse = ", ")
    ))
  }
  data <- data[, c(imputer$feature_names), drop = FALSE]
  for (feature in imputer$feature_names) {
    missing <- is.na(data[[feature]])
    if (any(missing)) {
      data[[feature]][missing] <- imputer$median[[feature]]
    }
  }
  if (anyNA(data)) {
    stop("TabICL features contain missing values after median imputation.")
  }
  data
}

as_tabicl_matrix <- function(data) {
  feature_names <- names(data)
  x <- as.matrix(data[feature_names])
  storage.mode(x) <- "double"
  if (anyNA(x)) {
    stop("TabICL features contain missing values.")
  }
  numpy$array(x, dtype = "float32")
}

fit_and_evaluate_tabicl <- function(K, folds, holdout) {
  train_data <- bind_rows(folds$X_train[[1]], folds$X_val[[1]])
  train_labels <- bind_rows(folds$y_hot_train[[1]], folds$y_hot_val[[1]])
  class_levels <- levels(train_labels$cat)

  if (!all(class_levels %in% levels(holdout$y_hot_holdout$cat))) {
    stop(sprintf(
      "Holdout labels do not contain all training classes for K=%d.",
      K
    ))
  }

  if (DEBUGGING) {
    message(sprintf(
      "DEBUGGING: Using a sample of only %d rows from training and holdout data for K=%d",
      N_DEBUGGING_ROWS,
      K
    ))
    set.seed(RANDOM_STATE)
    sample_indices_train <- sample(nrow(train_data), N_DEBUGGING_ROWS)
    train_data <- train_data[sample_indices_train, ]
    train_labels <- train_labels[sample_indices_train, ]
    sample_indices_holdout <- sample(nrow(holdout$X_holdout), N_DEBUGGING_ROWS)
    holdout$X_holdout <- holdout$X_holdout[sample_indices_holdout, ]
    holdout$y_hot_holdout <- holdout$y_hot_holdout[sample_indices_holdout, ]
  }

  imputer <- fit_tabicl_imputer(train_data)
  train_data <- apply_tabicl_imputer(train_data, imputer)
  holdout$X_holdout <- apply_tabicl_imputer(holdout$X_holdout, imputer)
  x_train <- as_tabicl_matrix(train_data)
  x_holdout <- as_tabicl_matrix(holdout$X_holdout)
  y_train <- r_to_py(as.character(train_labels$cat))

  message(sprintf(
    "Fitting TabICLv2 on %d training rows for K=%d",
    nrow(train_data),
    K
  ))
  disk_offload_dir <- file.path(
    TABICL_OFFLOAD_ROOT,
    sprintf("k%d", K)
  )
  if (!dir.exists(disk_offload_dir)) {
    dir.create(disk_offload_dir, recursive = TRUE)
  }
  model <- tabicl$TabICLClassifier(
    device = "cuda",
    use_amp = TRUE,
    use_fa3 = "auto",
    batch_size = 4L, # might be conservative, may be increased to 8L if GPU memory allows
    offload_mode = "disk",
    disk_offload_dir = disk_offload_dir,
    random_state = as.integer(RANDOM_STATE),
    n_jobs = as.integer(PHYSICAL_CORES)
  )
  model$fit(x_train, y_train)

  probabilities <- py_to_r(model$predict_proba(x_holdout))
  probabilities <- as.matrix(probabilities)
  predicted_levels <- as.character(py_to_r(model$classes_))
  if (!identical(predicted_levels, class_levels)) {
    stop(sprintf(
      "TabICL class order differs from prepared labels for K=%d.",
      K
    ))
  }

  y_pred <- choose_first_max_class(probabilities, class_levels)
  evaluation <- compute_final_evaluation(
    y_pred,
    holdout$y_hot_holdout
  )
  evaluation$probabilities <- probabilities
  evaluation$class_levels <- class_levels
  print(evaluation$metrics)
  evaluation
}

for (K in KS) {
  message(sprintf("Loading folds and holdout data for K=%d", K))
  folds <- readRDS(sprintf(
    "data/classification_ready_data/folds_k%d.rds",
    K
  ))
  holdout <- readRDS(sprintf(
    "data/classification_ready_data/holdout_k%d.rds",
    K
  ))

  evaluation <- fit_and_evaluate_tabicl(K, folds, holdout)
  saveRDS(
    evaluation,
    sprintf(
      "models/classification/holdout_evaluation_tabicl_k%d.rds",
      K
    )
  )
  # plot_path <- sprintf(
  #   "reports/figures/%s_cm_tabicl_k%d.png",
  #   figure_number_lookup[as.character(K)],
  #   K
  # )
  # ggplot2::ggsave(
  #   plot_path,
  #   plot = plot_lightgbm_confusion_matrix(evaluation),
  #   width = ifelse(K > 20, 12, 8),
  #   height = ifelse(K > 20, 10, 6),
  #   dpi = 600
  # )
}
