library(dplyr)
source("analysis/helpers/RQ5/evaluate_classifiers.R")

tune_and_save_lightgbm_model <- function(K, folds, n_trials = 100) {
  message(sprintf("==========\nTuning lgbm for K=%d\n=========", K))
  tpes_sampler <- optuna$samplers$TPESampler(seed = as.integer(RANDOM_STATE))
  if (!file.exists("models/classification/tuning")) {
    dir.create("models/classification/tuning", recursive = TRUE)
  }
  lgbm_study <- optuna$create_study(
    direction = "maximize",
    study_name = sprintf("lgbm_k%d", K),
    load_if_exists = TRUE,
    sampler = tpes_sampler,
    storage = sprintf(
      "sqlite:///%s/models/classification/tuning/optuna_tuning.sqlite3",
      getwd()
    )
  )
  # check if the study already has more than n_trial, and if so, skip optimization; else continue from the last trial
  n_trials_remaining <- n_trials -
    lgbm_study$trials_dataframe()$number |> length()
  if (n_trials_remaining <= 0) {
    message(sprintf(
      "Study for K=%d already has %d trials, skipping optimization",
      K,
      lgbm_study$trials_dataframe()$number |> length()
    ))
    return()
  }
  lgbm_study$optimize(objective_lightgbm, n_trials = n_trials_remaining)
  lgbm_best_params <- lgbm_study$best_params
  saveRDS(
    lgbm_best_params,
    sprintf("models/classification/tuning/lgbm_best_params_k%d.rds", K)
  )
}

retrain_and_save_best_parameters_lightgbm_model <- function(K, folds) {
  # skip if already exists
  if (
    file.exists(sprintf(
      "models/classification/final_lgbm_model_k%d.rds",
      K
    ))
  ) {
    message(sprintf(
      "Final lgbm model for K=%d already exists, skipping retraining",
      K
    ))
    return()
  }
  message(sprintf("Retraining best lgbm on full training data for K=%d", K))
  train_data <- folds$X_train[[1]]
  train_labels <- folds$y_hot_train[[1]]
  val_data <- folds$X_val[[1]] # necessary for early stopping
  val_labels <- folds$y_hot_val[[1]]
  best_params_lgbm <- readRDS(sprintf(
    "models/classification/tuning/lgbm_best_params_k%d.rds",
    K
  ))
  best_params_lgbm <- c(
    best_params_lgbm,
    list(
      objective = "multiclass",
      metric = "multi_logloss",
      num_class = K,
      seed = RANDOM_STATE,
      num_threads = PHYSICAL_CORES,
      deterministic = FALSE # still very close to deterministic, but way faster
    )
  )
  final_lgbm_model <- train_lightgbm_model_with_early_stopping(
    train_data,
    train_labels,
    val_data,
    val_labels,
    best_params_lgbm
  )
  saveRDS(
    final_lgbm_model,
    sprintf("models/classification/final_lgbm_model_k%d.rds", K)
  )
}

tune_and_save_linear_svm_model <- function(K, folds, n_trials = 100) {
  message(sprintf("==========\nTuning linear SVM for K=%d\n=========", K))
  cost_grid <- exp(seq(log(1e-4), log(1e4), length.out = n_trials))
  grid_sampler <- optuna$samplers$GridSampler(
    search_space = list(cost = cost_grid)
  )
  if (!file.exists("models/classification/tuning")) {
    dir.create("models/classification/tuning", recursive = TRUE)
  }
  svm_study <- optuna$create_study(
    direction = "maximize",
    study_name = sprintf("linear_svm_grid_k%d", K),
    load_if_exists = TRUE,
    sampler = grid_sampler,
    storage = sprintf(
      "sqlite:///%s/models/classification/tuning/optuna_tuning.sqlite3",
      getwd()
    )
  )
  n_trials_remaining <- n_trials -
    svm_study$trials_dataframe()$number |> length()
  if (n_trials_remaining <= 0) {
    message(sprintf(
      "Study for linear SVM K=%d already has %d trials, skipping optimization",
      K,
      svm_study$trials_dataframe()$number |> length()
    ))
    return()
  }
  svm_study$optimize(objective_linear_svm, n_trials = n_trials_remaining)
  saveRDS(
    svm_study$best_params,
    sprintf(
      "models/classification/tuning/linear_svm_grid_best_params_k%d.rds",
      K
    )
  )
}

retrain_and_save_best_parameters_linear_svm_model <- function(K, folds) {
  model_path <- sprintf(
    "models/classification/final_linear_svm_model_k%d.rds",
    K
  )
  if (file.exists(model_path)) {
    message(sprintf(
      "Final linear SVM model for K=%d already exists, skipping retraining",
      K
    ))
    return()
  }
  message(sprintf("Retraining best linear SVM for K=%d", K))
  best_params <- readRDS(sprintf(
    "models/classification/tuning/linear_svm_grid_best_params_k%d.rds",
    K
  ))
  model <- train_linear_svm_model(
    rbind(folds$X_train[[1]], folds$X_val[[1]]),
    rbind(folds$y_hot_train[[1]], folds$y_hot_val[[1]]),
    best_params
  )
  saveRDS(model, model_path)
}

objective_linear_svm <- function(trial) {
  params <- list(
    cost = trial$suggest_float("cost", 1e-4, 1e4, log = TRUE)
  )
  message(sprintf("=========\nLinear SVM trial %d\n=========", trial$number))
  metrics <- perform_cv_for_linear_svm(params, folds)
  append_metrics_to_file(
    metrics,
    model = "linear_svm",
    trial_number = trial$number
  )
  metrics$F1_macro_mean
}

perform_cv_for_linear_svm <- function(params, folds) {
  folds_metrics <- data.frame()
  for (i in seq_along(folds$X_train)) {
    message(sprintf("Performing linear SVM CV for fold %d/%d", i, N_FOLDS))
    model <- train_linear_svm_model(
      folds$X_train[[i]],
      folds$y_hot_train[[i]],
      params
    )
    metrics <- evaluate_linear_svm_model(
      model,
      folds$X_val[[i]],
      folds$y_hot_val[[i]]
    )
    message(sprintf(
      "Fold %d validation metrics: F1_macro = %.3f, kappa = %.3f",
      i,
      metrics$metrics$F1_macro,
      metrics$metrics$kappa
    ))
    folds_metrics <- bind_rows(folds_metrics, metrics$metrics)
  }
  summarize(
    folds_metrics,
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

train_linear_svm_model <- function(train_data, train_labels, params) {
  feature_names <- setdiff(names(train_data), "track.s.id")
  if (!all(vapply(train_data[feature_names], is.numeric, logical(1)))) {
    stop("Linear SVM features must be numeric after preprocessing.")
  }
  x_train <- as.matrix(train_data[feature_names])
  imputation <- fit_svm_imputer(x_train)
  x_train <- apply_svm_imputer(x_train, imputation)
  scaling <- fit_svm_scaler(x_train)
  x_train <- apply_svm_scaler(x_train, scaling)
  y_train <- factor(train_labels$cat)
  class_counts <- table(y_train)
  class_weights <- 1 / as.numeric(class_counts)
  names(class_weights) <- as.character(seq_along(class_weights))
  fit <- LiblineaR::LiblineaR(
    data = x_train,
    target = as.integer(y_train),
    type = 1,
    cost = params$cost,
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

fit_svm_imputer <- function(x_train) {
  medians <- apply(x_train, 2, median, na.rm = TRUE)
  if (anyNA(medians)) {
    stop(sprintf(
      "Linear SVM training features are entirely missing: %s",
      paste(names(medians)[is.na(medians)], collapse = ", ")
    ))
  }
  list(median = medians)
}

apply_svm_imputer <- function(x, imputation) {
  for (feature in seq_len(ncol(x))) {
    missing <- is.na(x[, feature])
    if (any(missing)) {
      x[missing, feature] <- imputation$median[[feature]]
    }
  }
  if (anyNA(x)) {
    stop("Linear SVM features contain missing values after median imputation.")
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
  if (!all(vapply(new_data[model$feature_names], is.numeric, logical(1)))) {
    stop("Linear SVM features must be numeric after preprocessing.")
  }
  x_new <- as.matrix(new_data[model$feature_names])
  x_new <- apply_svm_imputer(x_new, model$imputation)
  x_new <- apply_svm_scaler(x_new, model$scaling)
  predictions <- predict(model$fit, x_new)$predictions
  factor(
    model$class_levels[as.integer(predictions)],
    levels = model$class_levels
  )
}

objective_lightgbm <- function(trial) {
  params <- list(
    objective = "multiclass",
    metric = "multi_logloss",
    num_class = K,
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
    deterministic = FALSE, # still very close to deterministic, but way faster
    num_threads = PHYSICAL_CORES
  )
  message(sprintf("=========\nTrial %d\n=========", trial$number))
  metrics <- perform_cv_for_lightgbm_params(params, folds)
  append_metrics_to_file(metrics, model = "lgbm", trial_number = trial$number)
  metrics$F1_macro_mean
}


perform_cv_for_lightgbm_params <- function(params, folds) {
  folds_metrics <- data.frame()
  for (i in seq_along(folds$X_train)) {
    message(sprintf("Performing CV for fold %d/%d", i, N_FOLDS))
    train_data <- folds$X_train[[i]]
    train_labels <- folds$y_hot_train[[i]]
    val_data <- folds$X_val[[i]]
    val_labels_hot <- folds$y_hot_val[[i]]
    # pass validation data for early stopping
    res_model <- train_lightgbm_model_with_early_stopping(
      train_data,
      train_labels,
      val_data,
      val_labels_hot,
      params
    )
    metrics <- evaluate_lightgbm_model(
      res_model,
      val_data,
      val_labels_hot
    )
    message(sprintf(
      "Fold %d validation metrics: F1_macro = %.3f, kappa = %.3f, mn_log_loss = %.3f",
      i,
      metrics$F1_macro,
      metrics$kappa,
      metrics$mn_log_loss
    ))
    folds_metrics <- bind_rows(folds_metrics, metrics)
  }
  metrics_summary <- summarize(
    folds_metrics,
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
  metrics_summary
}

train_lightgbm_model_with_early_stopping <- function(
  train_data,
  train_labels,
  val_data,
  val_labels,
  params
) {
  class_weights <- train_labels |>
    group_by(cat) |>
    summarize(n = n()) |>
    mutate(weight = 1 / n) |>
    select(cat, weight)
  training_weights <- train_labels |>
    left_join(class_weights, by = "cat") |>
    pull(weight)
  data <- lightgbm::lgb.Dataset(
    data = as.matrix(select(train_data, -track.s.id)),
    label = as.integer(train_labels$cat) - 1,
    categorical_feature = CATEGORICAL_COLUMNS,
    weight = training_weights
  )
  val_data <- lightgbm::lgb.Dataset(
    data = as.matrix(select(val_data, -track.s.id)),
    label = as.integer(val_labels$cat) - 1,
    categorical_feature = CATEGORICAL_COLUMNS
  )
  fit <- lightgbm::lgb.train(
    params = params,
    data = data,
    nrounds = params$n_estimators,
    valids = list(validation = val_data),
    early_stopping_rounds = 50,
    verbose = -1
  )
  fit
}

median_mode_imputation <- function(data, imputation_vals) {
  for (col in names(imputation_vals)) {
    if (col %in% colnames(data)) {
      data[[col]][is.na(data[[col]])] <- imputation_vals[[col]]
    }
  }
  data
}
