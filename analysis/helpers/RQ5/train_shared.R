library(dplyr)
source("analysis/helpers/RQ5/evaluate_classifiers.R")
source("analysis/helpers/RQ5/notify.R")

# Data loading and preprocessing shared by all model types.

load_classification_data <- function(
  k,
  feature_set = "all",
  n_debugging_rows = 3000
) {
  #' Load the CV folds, holdout data, and categorical columns for granularity k and a feature set of `FEATURE_SETS` (subsampled if DEBUGGING).
  folds <- readRDS(sprintf("data/classification_ready_data/folds_k%d.rds", k))
  holdout <- readRDS(sprintf(
    "data/classification_ready_data/holdout_k%d.rds",
    k
  ))
  if (DEBUGGING) {
    message(sprintf(
      "DEBUGGING: using at most %d rows per data set",
      n_debugging_rows
    ))
    subsample <- function(data, rows) data[rows, , drop = FALSE]
    set.seed(RANDOM_STATE)
    for (part in c("train", "val")) {
      for (i in seq_along(folds$X_train)) {
        rows <- sample(nrow(folds[[paste0("X_", part)]][[i]]), n_debugging_rows)
        folds[[paste0("X_", part)]][[i]] <- subsample(
          folds[[paste0("X_", part)]][[i]],
          rows
        )
        folds[[paste0("y_hot_", part)]][[i]] <- subsample(
          folds[[paste0("y_hot_", part)]][[i]],
          rows
        )
      }
    }
    rows <- sample(nrow(holdout$X_holdout), n_debugging_rows)
    holdout <- lapply(holdout, subsample, rows)
  }
  if (feature_set == "no_spotify_genres") {
    drop_spotify_genres <- function(data) select(data, -starts_with("sp_"))
    folds$X_train <- lapply(folds$X_train, drop_spotify_genres)
    folds$X_val <- lapply(folds$X_val, drop_spotify_genres)
    holdout$X_holdout <- drop_spotify_genres(holdout$X_holdout)
  } else if (feature_set != "all") {
    stop(sprintf("Unknown feature set: %s", feature_set))
  }
  categorical_columns <- readr::read_csv(
    "data/classification_ready_data/categorical_columns.csv",
    show_col_types = FALSE
  )$column
  list(
    folds = folds,
    holdout = holdout,
    categorical_columns = intersect(
      categorical_columns,
      names(holdout$X_holdout)
    )
  )
}

get_balanced_class_weights <- function(labels) {
  #' Get the inverse class prevalence N / (K * n_c) per class, i.e. weights with mean 1 over all observations.
  class_counts <- table(labels)
  length(labels) / (length(class_counts) * class_counts)
}

get_full_training_data <- function(folds) {
  #' Combine training and validation data of a fold to the full training set.
  list(
    X = bind_rows(folds$X_train[[1]], folds$X_val[[1]]),
    y = bind_rows(folds$y_hot_train[[1]], folds$y_hot_val[[1]])
  )
}

summarize_cv_metrics <- function(fold_metrics) {
  summarize(
    fold_metrics,
    F1_macro_mean = mean(F1_macro),
    F1_macro_min = min(F1_macro),
    F1_macro_max = max(F1_macro),
    F1_SE = sd(F1_macro) / sqrt(n()),
    F1_macro_vals = paste(round(F1_macro, 3), collapse = ";"),
    kappa_mean = mean(kappa),
    Kappa_min = min(kappa),
    Kappa_max = max(kappa),
    kappa_SE = sd(kappa) / sqrt(n()),
    kappa_vals = paste(round(kappa, 3), collapse = ";")
  )
}

fit_median_imputer <- function(x_train) {
  medians <- apply(x_train, 2, median, na.rm = TRUE)
  if (anyNA(medians)) {
    stop(sprintf(
      "Training features are entirely missing: %s",
      paste(names(medians)[is.na(medians)], collapse = ", ")
    ))
  }
  list(median = medians)
}

apply_median_imputer <- function(x, imputation) {
  for (feature in seq_len(ncol(x))) {
    missing <- is.na(x[, feature])
    if (any(missing)) {
      x[missing, feature] <- imputation$median[[feature]]
    }
  }
  if (anyNA(x)) {
    stop("Features contain missing values after median imputation.")
  }
  x
}
