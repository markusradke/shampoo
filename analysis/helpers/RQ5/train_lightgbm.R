source("analysis/helpers/RQ5/train_shared.R")

get_optuna_storage <- function(k, feature_set) {
  #' Get one SQLite storage per study, so the studies of several machines can be merged by copying.
  sprintf(
    "sqlite:///%s/%s",
    getwd(),
    get_model_path("tuning/optuna_lgbm", k, feature_set, "sqlite3")
  )
}

tune_lightgbm_model <- function(
  k,
  feature_set,
  folds,
  categorical_columns,
  n_trials,
  progress_every = if (DEBUGGING) 1L else 25L
) {
  #' Tune LightGBM hyperparameters with TPE and 4-fold CV (resumes an existing study of the same run tag), notifying the progress every `progress_every` trials.
  #' @return The Optuna study. Each trial stores the mean early-stopping iteration of its folds.
  study <- optuna$create_study(
    direction = "maximize",
    study_name = sprintf("lgbm_%s_%s_k%d", RUN_TAG, feature_set, k),
    load_if_exists = TRUE,
    sampler = optuna$samplers$TPESampler(seed = as.integer(RANDOM_STATE)),
    storage = get_optuna_storage(k, feature_set)
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
    message(sprintf(
      "========= %s, K=%d, trial %d =========",
      feature_set,
      k,
      trial$number
    ))
    metrics <- perform_cv_for_lightgbm(params, folds, categorical_columns)
    trial$set_user_attr("mean_best_iteration", metrics$mean_best_iteration)
    append_metrics_to_file(metrics, "lgbm", k, feature_set, trial$number)
    metrics$F1_macro_mean
  }
  n_trials_remaining <- n_trials - length(study$trials)
  start_time <- Sys.time()
  notify_progress <- function(study, trial) {
    n_trials_done <- trial$number + 1
    if (n_trials_done %% progress_every != 0) {
      return(invisible(NULL))
    }
    n_trials_this_session <- n_trials_done - (n_trials - n_trials_remaining)
    elapsed_hours <- as.numeric(difftime(
      Sys.time(),
      start_time,
      units = "hours"
    ))
    send_notification(
      sprintf(
        "LightGBM %s k%d: trial %d of %d",
        feature_set,
        k,
        n_trials_done,
        n_trials
      ),
      sprintf(
        "Best CV F1 macro so far: %.4f\nElapsed: %.1f h\nRemaining tuning time: %.1f h",
        study$best_value,
        elapsed_hours,
        elapsed_hours / n_trials_this_session * (n_trials - n_trials_done)
      )
    )
  }
  if (n_trials_remaining > 0) {
    study$optimize(
      objective,
      n_trials = as.integer(n_trials_remaining),
      callbacks = list(notify_progress)
    )
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

train_final_lightgbm_model <- function(
  k,
  feature_set,
  study,
  folds,
  categorical_columns
) {
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
  saveRDS(params, get_model_path("lgbm_final_params", k, feature_set))
  full_training <- get_full_training_data(folds)
  train_lightgbm_model(
    full_training$X,
    full_training$y,
    params,
    categorical_columns
  )
}
