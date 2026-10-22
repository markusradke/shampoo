rm(list = ls())
gc()
library(readr)
library(dplyr)
library(yardstick)
library(LiblineaR)
# Assumes that the virtual environment ".venv" has been created in the project folder and the required packages have been installed
Sys.setenv(
  RETICULATE_PYTHON = ".venv/Scripts/python.exe" # Windows
  # RETICULATE_PYTHON = ".venv/bin/python3" # Linux
)
library(reticulate)
py_config()
optuna <- import("optuna")

source("analysis/helpers/RQ5/tuning_and_training.R")
source("analysis/helpers/RQ5/evaluate_classifiers.R")
source("analysis/helpers/RQ5/calculate_pfi.R")

# ATTENTION: THESE ACT AS GLOBAL VARIABLES FOR THE OBJECTIVE FUNCTION
N_TRIALS <- 100
RANDOM_STATE <- 42
PHYSICAL_CORES <- 32 # Hendrix has 32 physical cores
KS <- c(5, 15, 31)
N_REP <- 20 # PFI
CATEGORICAL_COLUMNS <- readr::read_csv(
  "data/classification_ready_data/categorical_columns.csv"
)$column

# TUNING, TRAINING, EVALUATION (METRICS + PFI) ----
for (K in KS) {
  message(sprintf("Loading folds and holdout data for K=%d", K))
  folds <- readRDS(sprintf("data/classification_ready_data/folds_k%d.rds", K))
  N_FOLDS <- length(folds$X_train)
  holdout <- readRDS(sprintf(
    "data/classification_ready_data/holdout_k%d.rds",
    K
  ))
  tune_and_save_lightgbm_model(K, folds, n_trials = N_TRIALS) # will skip if already exists
  retrain_and_save_best_parameters_lightgbm_model(K, folds) # will skip if already exists
  tune_and_save_linear_svm_model(K, folds, n_trials = N_TRIALS)
  retrain_and_save_best_parameters_linear_svm_model(K, folds)
  evaluation <- evaluate_final_lightgbm_model_on_holdout(K, folds, holdout)
  saveRDS(
    evaluation$holdout_evaluation,
    sprintf("models/classification/holdout_evaluation_lgbm_k%d.rds", K)
  )

  figure_number_lookup <- c(
    "5" = "fig09",
    "15" = "fig10",
    "31" = "fig11"
  )
  plot_path <- sprintf(
    "reports/figures/%s_cm_lgbm_k%d.png",
    figure_number_lookup[as.character(K)],
    K
  )

  ggplot2::ggsave(
    plot_path,
    plot = evaluation$holdout_confusion_matrix_plot,
    width = ifelse(K > 20, 12, 8),
    height = ifelse(K > 20, 10, 6),
    dpi = 600
  )

  svm_evaluation <- evaluate_final_linear_svm_model_on_holdout(K, holdout)
  saveRDS(
    svm_evaluation$holdout_evaluation,
    sprintf("models/classification/holdout_evaluation_linear_svm_k%d.rds", K)
  )

  plot_path <- sprintf(
    "reports/figures/%s_cm_linear_svm_k%d.png",
    figure_number_lookup[as.character(K)],
    K
  )
  ggplot2::ggsave(
    plot_path,
    plot = svm_evaluation$holdout_confusion_matrix_plot,
    width = ifelse(K > 20, 12, 8),
    height = ifelse(K > 20, 10, 6),
    dpi = 600
  )
}

evaluate_and_save_grouped_pfi_for_all_lightgbm_models(
  ks = KS,
  feature_groups = feature_groups,
  n_rep = N_REP,
  random_state = RANDOM_STATE
)
