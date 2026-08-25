rm(list = ls())
gc()
library(readr)
library(dplyr)
library(yardstick)
# Assumes that the virtual environment " ".venv"has been created in the project folder and the required packages have been installed
Sys.setenv(
  RETICULATE_PYTHON = ".venv/Scripts/python.exe" # Windows
  # RETICULATE_PYTHON = ".venv/bin/python3" # Linux
)
library(reticulate)
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
# for (K in KS) {
#   message(sprintf("Loading folds and holdout data for K=%d", K))
#   folds <- readRDS(sprintf("data/classification_ready_data/folds_k%d.rds", K))
#   N_FOLDS <- length(folds$X_train)
#   holdout <- readRDS(sprintf(
#     "data/classification_ready_data/holdout_k%d.rds",
#     K
#   ))
#   tune_and_save_lgbm_model(K, folds, n_trials = N_TRIALS) # will skip if already exists
#   retrain_and_save_best_parameters_lgbm_model(K, folds) # will skip if already exists
#   evaluate_final_model_on_holdout(K, folds, holdout)
# }

evaluate_and_save_grouped_pfi_for_all_k_models(
  ks = KS,
  feature_groups = feature_groups,
  n_rep = N_REP,
  random_state = RANDOM_STATE
)
