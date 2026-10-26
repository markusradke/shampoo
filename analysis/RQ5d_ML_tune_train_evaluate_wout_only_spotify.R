rm(list = ls())
gc()
library(readr)
library(dplyr)
library(yardstick)
library(lightgbm)

source("analysis/helpers/RQ5/tuning_and_training.R")
source("analysis/helpers/RQ5/evaluate_classifiers.R")

RANDOM_STATE <- 42
PHYSICAL_CORES <- 32 # Hendrix has 32 physical cores
KS <- c(5, 15, 31)

# CATEGORICAL_COLUMNS <- readr::read_csv(
#   "data/classification_ready_data/categorical_columns.csv",
#   show_col_types = FALSE
# )$column

# remove_spotify_genre_features <- function(data) {
#   data |>
#     select(-any_of(names(data)[startsWith(names(data), "sp_")]))
# }

# remove_spotify_genre_features_from_folds <- function(folds) {
#   folds$X_train <- lapply(folds$X_train, remove_spotify_genre_features)
#   folds$X_val <- lapply(folds$X_val, remove_spotify_genre_features)
#   folds
# }

# CATEGORICAL_COLUMNS <- CATEGORICAL_COLUMNS[
#   !startsWith(CATEGORICAL_COLUMNS, "sp_")
# ]

# for (K in KS) {
#   message(sprintf("Loading folds and holdout data for K=%d", K))
#   folds <- readRDS(sprintf(
#     "data/classification_ready_data/folds_k%d.rds",
#     K
#   )) |>
#     remove_spotify_genre_features_from_folds()
#   holdout <- readRDS(sprintf(
#     "data/classification_ready_data/holdout_k%d.rds",
#     K
#   ))
#   holdout$X_holdout <- remove_spotify_genre_features(holdout$X_holdout)

#   train_data <- folds$X_train[[1]]
#   train_labels <- folds$y_hot_train[[1]]
#   val_data <- folds$X_val[[1]]
#   val_labels <- folds$y_hot_val[[1]]

#   best_params_lgbm <- readRDS(sprintf(
#     "models/classification/tuning/lgbm_best_params_k%d.rds",
#     K
#   ))
#   best_params_lgbm <- c(
#     best_params_lgbm,
#     list(
#       objective = "multiclass",
#       metric = "multi_logloss",
#       num_class = K,
#       seed = RANDOM_STATE,
#       num_threads = PHYSICAL_CORES,
#       deterministic = FALSE
#     )
#   )

#   message(sprintf(
#     "Retraining Spotify-genre ablation lgbm for K=%d",
#     K
#   ))
#   final_lgbm_model <- train_lightgbm_model_with_early_stopping(
#     train_data,
#     train_labels,
#     val_data,
#     val_labels,
#     best_params_lgbm
#   )
#   saveRDS(
#     final_lgbm_model,
#     sprintf(
#       "models/classification/final_lgbm_model_no_spotify_k%d.rds",
#       K
#     )
#   )

#   evaluation <- evaluate_lightgbm_model(
#     final_lgbm_model,
#     holdout$X_holdout,
#     holdout$y_hot_holdout,
#     mode = "final"
#   )
#   print(evaluation$metrics)
#   saveRDS(
#     evaluation,
#     sprintf(
#       "models/classification/holdout_evaluation_lgbm_no_spotify_k%d.rds",
#       K
#     )
#   )
# }

# ONLY SPOTIFY FEATURES
select_only_spotify_genre_features <- function(data) {
  data |>
    select(
      all_of(names(data)[startsWith(names(data), "sp_")]),
      contains(".s."),
      track.is.dach_TRUE,
      is.major.label_FALSE,
      contains("popularity")
    )
}

select_only_spotify_genre_features_from_folds <- function(folds) {
  folds$X_train <- lapply(folds$X_train, select_only_spotify_genre_features)
  folds$X_val <- lapply(folds$X_val, select_only_spotify_genre_features)
  folds
}


CATEGORICAL_COLUMNS <- readr::read_csv(
  "data/classification_ready_data/categorical_columns.csv",
  show_col_types = FALSE
)$column
remaining_cols <- readRDS(
  "data/classification_ready_data/holdout_k5.rds"
)$X_holdout |>
  select_only_spotify_genre_features() |>
  colnames()
CATEGORICAL_COLUMNS <- CATEGORICAL_COLUMNS[
  CATEGORICAL_COLUMNS %in% remaining_cols
]

for (K in KS) {
  message(sprintf("Loading folds and holdout data for K=%d", K))
  folds <- readRDS(sprintf(
    "data/classification_ready_data/folds_k%d.rds",
    K
  )) |>
    select_only_spotify_genre_features_from_folds()
  holdout <- readRDS(sprintf(
    "data/classification_ready_data/holdout_k%d.rds",
    K
  ))
  holdout$X_holdout <- select_only_spotify_genre_features(holdout$X_holdout)

  train_data <- folds$X_train[[1]]
  train_labels <- folds$y_hot_train[[1]]
  val_data <- folds$X_val[[1]]
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
      deterministic = FALSE
    )
  )

  message(sprintf(
    "Retraining Spotify features only ablation lgbm for K=%d",
    K
  ))
  final_lgbm_model <- train_lightgbm_model_with_early_stopping(
    train_data,
    train_labels,
    val_data,
    val_labels,
    best_params_lgbm
  )
  saveRDS(
    final_lgbm_model,
    sprintf(
      "models/classification/final_lgbm_model_only_spotifyfeatures_k%d.rds",
      K
    )
  )

  evaluation <- evaluate_lightgbm_model(
    final_lgbm_model,
    holdout$X_holdout,
    holdout$y_hot_holdout,
    mode = "final"
  )
  print(evaluation$metrics)
  saveRDS(
    evaluation,
    sprintf(
      "models/classification/holdout_evaluation_lgbm_only_spotifyfeatures_k%d.rds",
      K
    )
  )
}
