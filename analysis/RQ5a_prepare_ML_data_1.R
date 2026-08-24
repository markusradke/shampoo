rm(list = ls())
gc()

library(dplyr)
library(data.table)
library(reticulate)
devtools::load_all()
source("analysis/helpers/RQ5/artist_grouped_split.R")
source("analysis/helpers/RQ5/spotify_shampoo_genres.R")
source("analysis/helpers/shared_helpers.R")

LAMBDA_SPOTIFY <- 0.5
K_LOW <- 5
K_MEDIUM <- 15
K_HIGH <- 31
RANDOM_STATE <- 42
TRAIN_SIZE <- 0.8
MIN_ARTIST_PREV_MODEL <- 0.0005

# LOAD DATA----
mb <- fread("data/long_data/mb_long.csv")
spotify <- fread("data/long_data/s_long.csv")
cat_states <- readRDS("models/dag/cat_states_full_50-lambda.rds")


# PERFORM SPLIT, AND ASSIGN LABELS ----
train_holdout <- split_artist_level_genres(
  mb = mb,
  cat_states = cat_states,
  chosen_k = K_HIGH,
  train_size = TRAIN_SIZE,
  seed = RANDOM_STATE
)
readr::write_csv(
  data.frame(track.s.id = train_holdout$train_tracks) |> distinct(),
  "data/splits/train_tracks.csv"
)
readr::write_csv(
  data.frame(track.s.id = train_holdout$test_tracks) |> distinct(),
  "data/splits/holdout_tracks.csv"
)

labels <- get_corpus_labels(
  mb = mb,
  cat_states = cat_states,
  train_ids = train_holdout$train_tracks,
  holdout_ids = train_holdout$test_tracks,
  ks = c(K_LOW, K_MEDIUM, K_HIGH)
)
saveRDS(labels, "data/splits/labels_train_holdout.rds")

# GET SPOTIFY MAP -----
spotify_features <- get_spotify_shampoo_map(
  spotify,
  train_holdout,
  MIN_ARTIST_PREV_MODEL,
  LAMBDA_SPOTIFY
)
readr::write_csv(
  spotify_features$train,
  "data/modeled_features/spotify_genres_mapping_train.csv"
)
readr::write_csv(
  spotify_features$holdout,
  "data/modeled_features/spotify_genres_mapping_holdout.csv"
)
# LYRICS FEATURES ----
# To get the lyrics features, please follow the helpers/RQ5/lyrics_ananlysis flow by running the scripts in order. The final output will be two csv files, one for train and one for holdout, which can be found in data/modeled_features/lyrics/.
