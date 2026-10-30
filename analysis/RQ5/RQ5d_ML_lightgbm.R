rm(list = ls())
gc()
library(readr)
library(dplyr)
library(ggplot2)
library(lightgbm)
library(reticulate)
source("analysis/config.R")
use_project_python()
optuna <- import("optuna")
source("analysis/helpers/RQ5/tuning_and_training.R")
source("analysis/helpers/RQ5/calculate_pfi.R")

# confusion matrix figures in the order of CHOSEN_KS
CONFUSION_MATRIX_FIGURES <- c("fig10", "fig11", "fig12")

# TUNING, FINAL TRAINING, HOLDOUT EVALUATION, PERMUTATION FEATURE IMPORTANCE ----
start_workers() # CV folds and feature groups are processed in parallel
pfi <- list()
for (i in seq_along(CHOSEN_KS)) {
  k <- CHOSEN_KS[i]
  ml_data <- load_classification_data(k)
  study <- tune_lightgbm_model(
    k,
    ml_data$folds,
    ml_data$categorical_columns,
    N_TRIALS
  )
  final_model <- train_final_lightgbm_model(
    k,
    study,
    ml_data$folds,
    ml_data$categorical_columns
  )
  model_path <- get_model_path("final_lgbm_model", k)
  saveRDS(final_model, model_path)

  evaluation <- evaluate_lightgbm_model(
    final_model,
    ml_data$holdout$X_holdout,
    ml_data$holdout$y_hot_holdout,
    mode = "final"
  )
  print(evaluation$metrics)
  saveRDS(evaluation, get_model_path("holdout_evaluation_lgbm", k))
  save_figure(
    sprintf("%s_cm_lgbm_k%d.png", CONFUSION_MATRIX_FIGURES[i], k),
    plot = plot_confusion_matrix(evaluation),
    width = ifelse(k > 20, 12, 8),
    height = ifelse(k > 20, 10, 6)
  )

  pfi[[i]] <- get_holdout_permutation_feature_importance(
    model_path,
    ml_data$holdout,
    feature_groups,
    n_rep = N_REPEATS,
    random_state = RANDOM_STATE
  ) |>
    mutate(k = k)
}
future::plan(future::sequential)

pfi <- bind_rows(pfi)
saveRDS(
  pfi,
  sprintf(
    "models/classification/holdout_permutation_feature_importance_lgbm_%s.rds",
    RUN_TAG
  )
)
plot_pfi(get_pfi_statistics(pfi))
save_figure("fig13_pfi.png", width = 10, height = 8)
