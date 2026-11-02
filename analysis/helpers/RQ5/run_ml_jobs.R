source("analysis/helpers/RQ5/train_lightgbm.R")
source("analysis/helpers/RQ5/train_linear_svm.R")
source("analysis/helpers/RQ5/train_tabicl.R")
source("analysis/helpers/RQ5/calculate_pfi.R")

# Each job tunes (if applicable), trains, and evaluates one model for one k and
# feature set. The holdout evaluation is saved last and marks a finished job.

get_feature_names <- function(data) setdiff(names(data), "track.s.id")

run_lightgbm_job <- function(k, feature_set) {
  ml_data <- load_classification_data(k, feature_set)
  feature_groups <- get_feature_groups(get_feature_names(
    ml_data$holdout$X_holdout
  ))
  study <- tune_lightgbm_model(
    k,
    feature_set,
    ml_data$folds,
    ml_data$categorical_columns,
    N_TRIALS
  )
  final_model <- train_final_lightgbm_model(
    k,
    feature_set,
    study,
    ml_data$folds,
    ml_data$categorical_columns
  )
  model_path <- get_model_path("final_lgbm_model", k, feature_set)
  saveRDS(final_model, model_path)
  evaluation <- evaluate_lightgbm_model(
    final_model,
    ml_data$holdout$X_holdout,
    ml_data$holdout$y_hot_holdout,
    mode = "final"
  )
  get_holdout_permutation_feature_importance(
    model_path,
    ml_data$holdout,
    feature_groups,
    n_rep = N_REPEATS,
    random_state = RANDOM_STATE
  ) |>
    mutate(k = k, feature_set = feature_set) |>
    saveRDS(get_model_path("holdout_pfi_lgbm", k, feature_set))
  saveRDS(evaluation, get_model_path("holdout_evaluation_lgbm", k, feature_set))
}

run_linear_svm_job <- function(k, feature_set) {
  ml_data <- load_classification_data(k, feature_set)
  cv_metrics <- tune_linear_svm_model(k, feature_set, ml_data$folds, N_TRIALS)
  best_cost <- cv_metrics$cost[which.max(cv_metrics$F1_macro_mean)]
  message(sprintf(
    "Linear SVM %s K=%d: best cost %.4g",
    feature_set,
    k,
    best_cost
  ))
  full_training <- get_full_training_data(ml_data$folds)
  final_model <- train_linear_svm_model(
    full_training$X,
    full_training$y,
    best_cost
  )
  saveRDS(final_model, get_model_path("final_linear_svm_model", k, feature_set))
  evaluation <- evaluate_linear_svm_model(
    final_model,
    ml_data$holdout$X_holdout,
    ml_data$holdout$y_hot_holdout,
    mode = "final"
  )
  saveRDS(
    evaluation,
    get_model_path("holdout_evaluation_linear_svm", k, feature_set)
  )
}

run_tabicl_job <- function(k, feature_set) {
  ml_data <- load_classification_data(k, feature_set)
  evaluation <- fit_and_evaluate_tabicl(
    k,
    feature_set,
    ml_data$folds,
    ml_data$holdout
  )
  saveRDS(
    evaluation,
    get_model_path("holdout_evaluation_tabicl", k, feature_set)
  )
}

describe_job_results <- function(model_name, k, feature_set) {
  #' Summarize the saved results of a finished job as text for a notification.
  cv_metrics <- get_best_cv_metrics(model_name, k, feature_set)
  holdout_metrics <- readRDS(get_model_path(
    paste0("holdout_evaluation_", model_name),
    k,
    feature_set
  ))$metrics
  description <- c(
    "Best CV metrics:",
    if (is.null(cv_metrics)) "none (not tuned)" else format_table(cv_metrics),
    "",
    "Holdout metrics:",
    format_table(holdout_metrics)
  )
  pfi_path <- get_model_path("holdout_pfi_lgbm", k, feature_set)
  if (model_name == "lgbm" && file.exists(pfi_path)) {
    top_feature_groups <- readRDS(pfi_path) |>
      group_by(feature_group) |>
      summarize(mean_pfi = mean(pfi_f1_group)) |>
      slice_max(mean_pfi, n = 3)
    description <- c(
      description,
      "",
      "Top 3 feature groups (PFI):",
      format_table(top_feature_groups)
    )
  }
  paste(description, collapse = "\n")
}
