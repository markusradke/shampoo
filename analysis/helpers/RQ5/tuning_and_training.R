library(dplyr)
source("analysis/helpers/RQ5/evaluate_classifiers.R")

# SHARED ----
load_classification_data <- function(k, n_debugging_rows = 3000) {
  #' Load the CV folds, holdout data, and categorical columns for granularity k (subsampled if DEBUGGING).
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
  list(
    folds = folds,
    holdout = holdout,
    categorical_columns = readr::read_csv(
      "data/classification_ready_data/categorical_columns.csv",
      show_col_types = FALSE
    )$column
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

get_optuna_storage <- function() {
  sprintf(
    "sqlite:///%s/models/classification/tuning/optuna_tuning.sqlite3",
    getwd()
  )
}

# LIGHTGBM ----
tune_lightgbm_model <- function(k, folds, categorical_columns, n_trials) {
  #' Tune LightGBM hyperparameters with TPE and 4-fold CV (resumes an existing study of the same run tag).
  #' @return The Optuna study. Each trial stores the mean early-stopping iteration of its folds.
  study <- optuna$create_study(
    direction = "maximize",
    study_name = sprintf("lgbm_%s_k%d", RUN_TAG, k),
    load_if_exists = TRUE,
    sampler = optuna$samplers$TPESampler(seed = as.integer(RANDOM_STATE)),
    storage = get_optuna_storage()
  )
  objective <- function(trial) {
    params <- list(
      objective = "multiclass",
      metric = "multi_logloss",
      num_class = k,
      n_estimators = trial$suggest_int("n_estimators", 100, 2000),
      learning_rate = trial$suggest_float(
        "learning_rate",
        0.001,
        0.3,
        log = TRUE
      ),
      num_leaves = trial$suggest_int("num_leaves", 20, 70),
      max_depth = trial$suggest_int("max_depth", 1, 15),
      min_data_in_leaf = trial$suggest_int("min_data_in_leaf", 1, 100),
      feature_fraction = trial$suggest_float("feature_fraction", 0.1, 1.0),
      reg_alpha = trial$suggest_float("reg_alpha", 1e-8, 10, log = TRUE),
      reg_lambda = trial$suggest_float("reg_lambda", 1e-8, 10, log = TRUE),
      seed = RANDOM_STATE,
      deterministic = FALSE # still very close to deterministic, but way faster
    )
    message(sprintf("========= K=%d, trial %d =========", k, trial$number))
    metrics <- perform_cv_for_lightgbm(params, folds, categorical_columns)
    trial$set_user_attr("mean_best_iteration", metrics$mean_best_iteration)
    append_metrics_to_file(metrics, "lgbm", k, trial$number)
    metrics$F1_macro_mean
  }
  n_trials_remaining <- n_trials - length(study$trials)
  if (n_trials_remaining > 0) {
    study$optimize(objective, n_trials = as.integer(n_trials_remaining))
  }
  study
}

perform_cv_for_lightgbm <- function(params, folds, categorical_columns) {
  #' Train and evaluate one LightGBM model per fold in parallel workers, splitting the cores between folds.
  params$num_threads <- max(1L, N_WORKERS %/% length(folds$X_train))
  fold_data <- lapply(seq_along(folds$X_train), function(i) {
    lapply(folds[c("X_train", "y_hot_train", "X_val", "y_hot_val")], `[[`, i)
  })
  fold_metrics <- future.apply::future_lapply(
    fold_data,
    train_and_evaluate_lightgbm_fold, # defined at top level, so only its arguments are sent to the workers
    params = params,
    categorical_columns = categorical_columns,
    future.seed = TRUE,
    future.packages = c("dplyr", "lightgbm")
  ) |>
    bind_rows()
  message(sprintf(
    "CV F1 macro per fold: %s",
    paste(round(fold_metrics$F1_macro, 3), collapse = ", ")
  ))
  cbind(
    summarize_cv_metrics(fold_metrics),
    mean_best_iteration = mean(fold_metrics$best_iteration)
  )
}

train_and_evaluate_lightgbm_fold <- function(
  fold,
  params,
  categorical_columns
) {
  #' Train a LightGBM model on the training data of a CV fold and evaluate it on the validation data.
  model <- train_lightgbm_model(
    fold$X_train,
    fold$y_hot_train,
    params,
    categorical_columns,
    fold$X_val,
    fold$y_hot_val
  )
  cbind(
    evaluate_lightgbm_model(model, fold$X_val, fold$y_hot_val),
    best_iteration = model$best_iter
  )
}

train_lightgbm_model <- function(
  train_data,
  train_labels,
  params,
  categorical_columns,
  val_data = NULL,
  val_labels = NULL
) {
  #' Train a class-weighted LightGBM model for `params$n_estimators` rounds, with early stopping (50 rounds) if validation data is given.
  get_dataset <- function(data, labels, weights = NULL) {
    lightgbm::lgb.Dataset(
      data = as.matrix(select(data, -track.s.id)),
      label = as.integer(labels$cat) - 1,
      categorical_feature = categorical_columns,
      weight = weights
    )
  }
  class_weights <- get_balanced_class_weights(train_labels$cat)
  train_set <- get_dataset(
    train_data,
    train_labels,
    as.numeric(class_weights[as.character(train_labels$cat)])
  )
  has_validation <- !is.null(val_data)
  lightgbm::lgb.train(
    params = params[names(params) != "n_estimators"],
    data = train_set,
    nrounds = params$n_estimators,
    valids = if (has_validation) {
      list(validation = get_dataset(val_data, val_labels))
    } else {
      list()
    },
    early_stopping_rounds = if (has_validation) 50,
    verbose = -1
  )
}

train_final_lightgbm_model <- function(k, study, folds, categorical_columns) {
  #' Retrain the best trial on the full training set with its mean early-stopping iteration from CV as number of rounds.
  best_trial <- study$best_trial
  params <- c(
    best_trial$params,
    list(
      objective = "multiclass",
      metric = "multi_logloss",
      num_class = k,
      seed = RANDOM_STATE,
      num_threads = N_WORKERS,
      deterministic = FALSE
    )
  )
  params$n_estimators <- round(best_trial$user_attrs$mean_best_iteration)
  saveRDS(params, get_model_path("lgbm_final_params", k))
  full_training <- get_full_training_data(folds)
  train_lightgbm_model(
    full_training$X,
    full_training$y,
    params,
    categorical_columns
  )
}

# LINEAR SVM ----
tune_linear_svm_model <- function(k, folds, n_costs) {
  #' Evaluate a logarithmic grid of costs between 1e-4 and 1e4 with 4-fold CV in parallel workers.
  #' @return The CV metrics per cost, also written to the tuning folder.
  costs <- exp(seq(log(1e-4), log(1e4), length.out = n_costs))
  fold_metrics <- with_worker_progress({
    progress <- progressr::progressor(length(folds$X_train) * n_costs)
    lapply(seq_along(folds$X_train), function(i) {
      message(sprintf(
        "Linear SVM K=%d: fold %d of %d",
        k,
        i,
        length(folds$X_train)
      ))
      fold <- lapply(
        folds[c("X_train", "y_hot_train", "X_val", "y_hot_val")],
        `[[`,
        i
      )
      future.apply::future_lapply(
        costs,
        evaluate_linear_svm_cost, # defined at top level, so only its arguments are sent to the workers
        fold = fold,
        progress = progress,
        future.seed = TRUE,
        future.packages = c("dplyr", "LiblineaR")
      ) |>
        bind_rows()
    })
  })
  cv_metrics <- bind_rows(fold_metrics) |>
    group_by(cost) |>
    summarize_cv_metrics()
  readr::write_csv(
    cv_metrics,
    sprintf(
      "models/classification/tuning/linear_svm_%s_k%d_tuning_metrics.csv",
      RUN_TAG,
      k
    )
  )
  cv_metrics
}

evaluate_linear_svm_cost <- function(cost, fold, progress) {
  #' Train a linear SVM with `cost` on the training data of a CV fold and get its metrics on the validation data.
  model <- train_linear_svm_model(fold$X_train, fold$y_hot_train, cost)
  metrics <- evaluate_linear_svm_model(
    model,
    fold$X_val,
    fold$y_hot_val
  )$metrics
  progress(sprintf("cost %.4g: F1 macro %.3f", cost, metrics$F1_macro))
  data.frame(cost = cost, metrics)
}

train_linear_svm_model <- function(train_data, train_labels, cost) {
  #' Train a class-weighted L2-regularized L2-loss linear SVM (primal solver) on median-imputed, standardized features.
  feature_names <- setdiff(names(train_data), "track.s.id")
  if (!all(vapply(train_data[feature_names], is.numeric, logical(1)))) {
    stop("Linear SVM features must be numeric after preprocessing.")
  }
  x_train <- as.matrix(train_data[feature_names])
  imputation <- fit_median_imputer(x_train)
  x_train <- apply_median_imputer(x_train, imputation)
  scaling <- fit_svm_scaler(x_train)
  x_train <- apply_svm_scaler(x_train, scaling)
  y_train <- factor(train_labels$cat)
  class_weights <- as.numeric(get_balanced_class_weights(y_train))
  names(class_weights) <- as.character(seq_along(class_weights))
  fit <- LiblineaR::LiblineaR(
    data = x_train,
    target = as.integer(y_train),
    type = 2,
    cost = cost,
    wi = class_weights,
    bias = 1,
    verbose = FALSE
  )
  list(
    fit = fit,
    feature_names = feature_names,
    imputation = imputation,
    scaling = scaling,
    class_levels = levels(y_train)
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

fit_svm_scaler <- function(x_train) {
  centers <- colMeans(x_train)
  scales <- apply(x_train, 2, sd)
  scales[is.na(scales) | scales == 0] <- 1
  list(center = centers, scale = scales)
}

apply_svm_scaler <- function(x, scaling) {
  sweep(sweep(x, 2, scaling$center, "-"), 2, scaling$scale, "/")
}

predict_linear_svm <- function(model, new_data) {
  missing_features <- setdiff(model$feature_names, names(new_data))
  if (length(missing_features) > 0) {
    stop(sprintf(
      "Missing linear SVM features: %s",
      paste(missing_features, collapse = ", ")
    ))
  }
  x_new <- as.matrix(new_data[model$feature_names])
  x_new <- apply_median_imputer(x_new, model$imputation)
  x_new <- apply_svm_scaler(x_new, model$scaling)
  decision_values <- predict(
    model$fit,
    x_new,
    decisionValues = TRUE
  )$decisionValues
  # LiblineaR orders the columns by first appearance of the classes in training
  class_columns <- as.character(seq_along(model$class_levels))
  choose_first_max_class(
    decision_values[, class_columns, drop = FALSE],
    model$class_levels
  )
}
