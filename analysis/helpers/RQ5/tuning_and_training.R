library(dplyr)
source("analysis/helpers/RQ5/evaluate_classifiers.R")

tune_and_save_lgbm_model <- function(K, folds, n_trials = 100) {
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
  lgbm_study$optimize(objective_lgbm, n_trials = n_trials_remaining)
  lgbm_best_params <- lgbm_study$best_params
  saveRDS(
    lgbm_best_params,
    sprintf("models/classification/tuning/lgbm_best_params_k%d.rds", K)
  )
}

retrain_and_save_best_parameters_lgbm_model <- function(K, folds) {
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
  final_lgbm_model <- train_lgbm_model_with_early_stopping(
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

objective_lgbm <- function(trial) {
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
  metrics <- perform_cv_for_params(params, folds)
  append_metrics_to_file(metrics, model = "lgbm", trial_number = trial$number)
  metrics$F1_macro_mean
}


perform_cv_for_params <- function(params, folds) {
  folds_metrics <- data.frame()
  for (i in seq_along(folds$X_train)) {
    message(sprintf("Performing CV for fold %d/%d", i, N_FOLDS))
    train_data <- folds$X_train[[i]]
    train_labels <- folds$y_hot_train[[i]]
    val_data <- folds$X_val[[i]]
    val_labels_hot <- folds$y_hot_val[[i]]
    # pass validation data for early stopping
    res_model <- train_lgbm_model_with_early_stopping(
      train_data,
      train_labels,
      val_data,
      val_labels_hot,
      params
    )
    metrics <- evaluate_lgbm_model(
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

train_lgbm_model_with_early_stopping <- function(
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
