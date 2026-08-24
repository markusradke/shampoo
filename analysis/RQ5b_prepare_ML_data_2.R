# PREPARE FEATURES AND CREATE FOLDS ----
rm(list = ls())
gc()
library(dplyr)
library(readr)
devtools::load_all()
source("analysis/helpers/RQ5/prepare_classification_data.R")

CATEGORICAL_FEATURES <- c(
  "track.s.key",
  "track.s.mode",
  "track.s.timesignature",
  "track.s.explicitlyrics",
  "track.dz.album.explicitlyrics",
  "artist.mb.type",
  "artist.mb.gender",
  "artist.mb.area",
  "artist.mb.dead",
  "artist.mb.origin",
  "track.ab.tonal.key",
  "track.ab.tonal.mode",
  "track.language",
  "track.is.instrumental",
  "track.is.dach",
  "is.major.label"
)
N_FOLDS <- 4
K <- 31 # highest K for classification task (split at highest granularity)
OTHER_Ks <- c(5, 15)


# LOAD AND JOIN DATA ----
labels <- readRDS("data/splits/labels_train_holdout.rds")
train_ids <- labels[[as.character(K)]]$train |>
  select(track.s.id)
holdout_ids <- labels[[as.character(K)]]$holdout |>
  select(track.s.id)

spotify_train <- readr::read_csv(
  "data/modeled_features/spotify_genres_mapping_train.csv"
)
spotify_holdout <- readr::read_csv(
  "data/modeled_features/spotify_genres_mapping_holdout.csv"
)

expressions_train <- readr::read_csv(
  "data/modeled_features/lyrics/expressiontypes_train_no_ref_type.csv"
)
expressions_holdout <- readr::read_csv(
  "data/modeled_features/lyrics/expressiontypes_holdout_no_ref_type.csv"
)

poptrag <- readRDS("data-raw/poptrag.rds")
poptrag_features <- readRDS("data-raw/classification_prep_features.rds")
poptrag_features <- poptrag_features[
  !poptrag_features %in% c("artist.s.genres")
]
# drops rows with >40% missing features
# Training: Retained 115,643 samples (85%) after dropping rows with >40% missing features
# Holdout: Retained 29,097 samples (84%) after dropping rows with >40% missing features
joined_train <- get_joined_classification_features(
  poptrag,
  poptrag_features,
  spotify = spotify_train,
  expressions = expressions_train,
  labels = train_ids
)
joined_holdout <- get_joined_classification_features(
  poptrag,
  poptrag_features,
  spotify = spotify_holdout,
  expressions = expressions_holdout,
  labels = holdout_ids
)

# CREATE SPLITS AND DUMMY CODE CATEGORICALS ----
y_train <- labels[[as.character(K)]]$train
y_holdout <- labels[[as.character(K)]]$holdout
artists_train <- y_train |>
  select(track.s.id) |>
  left_join(
    select(poptrag, track.s.id, track.s.firstartist.id),
    by = "track.s.id"
  ) |>
  filter(track.s.id %in% joined_train$track.s.id)

splits_holdout_dummy_initial <- get_splits_with_dummy_coded_features(
  joined_train = joined_train,
  y_train = y_train,
  artist_train = artists_train,
  joined_holdout = joined_holdout,
  y_holdout = y_holdout,
  categorical_features = CATEGORICAL_FEATURES,
  n_folds = N_FOLDS,
  seed = RANDOM_STATE
)

# lump small observations in any split (< threshodl) in dummy coded features to "other" category in every split and in holdout
lumped_dummies <- lump_small_observations_dummy_cols(
  splits_with_dummies = splits_holdout_dummy_initial$splits,
  holdout_initial = splits_holdout_dummy_initial$holdout,
  threshold = 100,
  categorical_features = CATEGORICAL_FEATURES
)
categorical_columns <- get_categorical_dummy_cols(
  colnames(lumped_dummies$splits[[1]]),
  CATEGORICAL_FEATURES
)
if (!file.exists("data/classification_ready_data")) {
  dir.create("data/classification_ready_data")
}
write_csv(
  data.frame(column = categorical_columns),
  "data/classification_ready_data/categorical_columns.csv"
)

# create training folds
folds <- get_folds_from_splits(lumped_dummies$splits)
holdout <- lumped_dummies$holdout


median_mode_imputation_vals <- get_median_mode_imputation_values(
  folds,
  CATEGORICAL_FEATURES
)
saveRDS(
  median_mode_imputation_vals,
  "data/classification_ready_data/median_mode_imputation_values.rds"
)

# align with labels for different Ks and save folds and holdout data for each K
for (k in c(OTHER_Ks, K)) {
  y_train_k <- labels[[as.character(k)]]$train
  y_holdout_k <- labels[[as.character(k)]]$holdout
  hot_levels <- sort(unique(union(y_train_k$cat, y_holdout_k$cat)))
  y_train_k$cat <- factor(y_train_k$cat, levels = hot_levels)
  y_holdout_k$cat <- factor(y_holdout_k$cat, levels = hot_levels)

  folds_labels <- add_aligned_labels_to_folds(folds, y_train_k)
  holdout_labels <- get_aligned_holdout_labels(holdout, y_holdout_k)
  saveRDS(
    folds_labels,
    sprintf(
      "data/classification_ready_data/folds_k%d.rds",
      k
    )
  )
  saveRDS(
    holdout_labels,
    sprintf(
      "data/classification_ready_data/holdout_k%d.rds",
      k
    )
  )
}
