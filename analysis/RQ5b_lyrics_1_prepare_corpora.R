rm(list = ls())
gc()
library(dplyr)
library(readr)
library(purrr)
library(fastText)
source("analysis/config.R")
source("analysis/helpers/RQ5/lyrics_analysis/helpers/preprocess_lyrics.R")

# requirements:
# raw_corpus: Data frame with columns track.s.id, track.g.lyrics, track.language, track.is.deepltranslation, album.s.title
# labels: training labels (track.s.id, cat) at the highest granularity
# holdout: Data frame with column track.s.id (holdout tracks)
raw_corpus <- readRDS("data-raw/poptrag_lyrics_w_translations_20260430.rds")
labels <- readRDS("data/splits/labels_train_holdout.rds")
genres <- labels[[as.character(max(CHOSEN_KS))]]$train
holdout <- read_csv("data/splits/holdout_tracks.csv") |> distinct()

prepare_lyrics_corpus <- function(annotated) {
  #' Keep German and English lyrics with at least two lines whose language is confirmed by fastText, and preprocess them.
  german_english_only <- annotated |>
    filter(!is.na(track.is.deepltranslation)) |>
    mutate(n_lines = sapply(track.g.lyrics, nrow)) |>
    filter(n_lines > 1) |>
    # contains only "woof"
    filter(album.s.title != "No Grave but the Sea (Deluxe Edition)") |>
    filter(track.language %in% c("German", "English"))
  german <- get_language_confidence(
    build_german_corpus(german_english_only),
    "lyrics_de",
    target_language = "de"
  )
  english <- get_language_confidence(
    build_english_corpus(german_english_only),
    "lyrics",
    target_language = "en"
  )
  if (interactive()) {
    # manually inspect confidence scores
    View(select(arrange(german, share_target), share_target, mean_prob_target, lyrics_de))
    View(select(arrange(english, share_target), share_target, mean_prob_target, lyrics))
  }

  # at least 35% German lines and mean confidence above 0 (keeps dialect and
  # bilingual expressions); stricter for English, as there are more false
  # positives (e.g., German songs with many English lines)
  german_filtered <- german |>
    filter(share_target > 0.35, mean_prob_target > 0.0)
  english_filtered <- english |>
    filter(share_target > 0.75, mean_prob_target > 0.4)
  german_filtered$lyrics <- preprocess_lyrics(german_filtered$lyrics_en)
  english_filtered$lyrics <- preprocess_lyrics(english_filtered$lyrics)
  bind_rows(german_filtered, english_filtered) |>
    select(-any_of(c("lyrics_de", "lyrics_en")))
}

# PREPARE TRAIN CORPUS ----
train_filtered <- inner_join(raw_corpus, genres, by = "track.s.id") |>
  prepare_lyrics_corpus()
message(sprintf(
  "Lyrics available for %d%% of tracks in the train data set.",
  round(nrow(train_filtered) / nrow(genres) * 100)
))
write_csv(train_filtered, "data/modeled_features/lyrics/lyrics_train_corpus.csv")

# PREPARE HOLDOUT CORPUS ----
holdout_filtered <- inner_join(raw_corpus, holdout, by = "track.s.id") |>
  mutate(cat = "holdout") |> # dummy for preprocessing function
  prepare_lyrics_corpus()
message(sprintf(
  "Lyrics available for %d%% of tracks in the holdout data set.",
  round(nrow(holdout_filtered) / nrow(holdout) * 100)
))
write_csv(
  holdout_filtered,
  "data/modeled_features/lyrics/lyrics_holdout_corpus.csv"
)
