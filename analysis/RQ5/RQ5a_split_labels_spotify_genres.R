rm(list = ls())
gc()

library(dplyr)
library(data.table)
devtools::load_all()
source("analysis/config.R")
source("analysis/helpers/RQ5/artist_grouped_split.R")
source("analysis/helpers/RQ5/spotify_shampoo_genres.R")

TRAIN_SIZE <- 0.8
MIN_ARTIST_PREV_SPOTIFY <- 0.001

# LOAD DATA----
mb <- fread("data/long_data/mb_long.csv")
spotify <- fread("data/long_data/s_long.csv")
cat_states <- readRDS(
  sprintf("models/dag/cat_states_full_%d-lambda.rds", round(LAMBDA * 100))
)

# PERFORM SPLIT (STRATIFIED AT HIGHEST GRANULARITY) AND ASSIGN LABELS ----
train_holdout <- split_artist_level_genres(
  mb = mb,
  cat_states = cat_states,
  chosen_k = max(CHOSEN_KS),
  train_size = TRAIN_SIZE,
  seed = RANDOM_STATE
)
readr::write_csv(
  data.frame(track.s.id = train_holdout$train_tracks),
  "data/splits/train_tracks.csv"
)
readr::write_csv(
  data.frame(track.s.id = train_holdout$test_tracks),
  "data/splits/holdout_tracks.csv"
)

labels <- get_corpus_labels(
  mb = mb,
  cat_states = cat_states,
  train_ids = train_holdout$train_tracks,
  holdout_ids = train_holdout$test_tracks,
  ks = CHOSEN_KS
)
saveRDS(labels, "data/splits/labels_train_holdout.rds")

# GET SPOTIFY DISTRIBUTION GENRES ----
spotify_features <- get_spotify_shampoo_map(
  spotify,
  train_holdout,
  MIN_ARTIST_PREV_SPOTIFY,
  LAMBDA
)
readr::write_csv(
  spotify_features$train,
  "data/modeled_features/spotify_genres_mapping_train.csv"
)
readr::write_csv(
  spotify_features$holdout,
  "data/modeled_features/spotify_genres_mapping_holdout.csv"
)
# Next: lyrics features (RQ5b_lyrics_* scripts), or use the features provided
# in the data repository (https://doi.org/10.14279/depositonce-26469).
