rm(list = ls())
gc()
library(readr)
library(dplyr)
library(ggplot2)
library(LiblineaR)
source("analysis/config.R")
source("analysis/helpers/RQ5/tuning_and_training.R")

# GRID SEARCH, FINAL TRAINING, HOLDOUT EVALUATION ----
start_workers() # costs of a CV fold are evaluated in parallel
for (k in CHOSEN_KS) {
  ml_data <- load_classification_data(k)
  cv_metrics <- tune_linear_svm_model(k, ml_data$folds, N_TRIALS)
  best_cost <- cv_metrics$cost[which.max(cv_metrics$F1_macro_mean)]
  message(sprintf("Linear SVM K=%d: best cost %.4g", k, best_cost))

  full_training <- get_full_training_data(ml_data$folds)
  final_model <- train_linear_svm_model(
    full_training$X,
    full_training$y,
    best_cost
  )
  saveRDS(final_model, get_model_path("final_linear_svm_model", k))

  evaluation <- evaluate_linear_svm_model(
    final_model,
    ml_data$holdout$X_holdout,
    ml_data$holdout$y_hot_holdout,
    mode = "final"
  )
  print(evaluation$metrics)
  saveRDS(evaluation, get_model_path("holdout_evaluation_linear_svm", k))
  save_figure(
    sprintf("supp_cm_linear_svm_k%d.png", k),
    plot = plot_confusion_matrix(evaluation),
    width = ifelse(k > 20, 12, 8),
    height = ifelse(k > 20, 10, 6)
  )
}
future::plan(future::sequential)
