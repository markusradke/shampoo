rm(list = ls())
gc()
library(dplyr)
library(readr)
library(purrr)
library(fastText)
source("analysis/helpers/RQ5/lyrics_analysis/helpers/preprocess_lyrics.R")

K <- 31 # choose highest detail K

# MAIN ----
# requirments:
# raw_corpus: Data frame with columns track.s.id, track.g.lyrics, track.language, track.is.deepltranslation, album.s.title
# genres: Data frame with columns track.s.id, cat (genre label)
# holdout: Data frame with column track.s.id (holdout tracks)
raw_corpus <- readRDS("data-raw/poptrag_lyrics_w_translations_20260430.rds")
labels <- readRDS("data/splits/labels_train_holdout.rds")
genres <- labels[[as.character(K)]]$train
holdout <- read_csv("data/splits/holdout_tracks.csv") |> distinct()

# PREPARE TRAIN CORPUS ----
train_annotated <- inner_join(
  raw_corpus,
  genres,
  by = "track.s.id"
)

has_lyrics <- train_annotated |> filter(!is.na(track.is.deepltranslation))
has_lyrics$n_lines <- sapply(has_lyrics$track.g.lyrics, nrow)
min_two_lines <- has_lyrics |> filter(n_lines > 1)
without_invalid <- min_two_lines |>
  filter(album.s.title != "No Grave but the Sea (Deluxe Edition)") # contains only "woof"
german_english_only <- without_invalid |>
  filter(track.language %in% c("German", "English"))
german <- build_german_corpus(german_english_only)
english <- build_english_corpus(german_english_only)

german_w_confidence <- get_language_confidence(
  german,
  "lyrics_de",
  target_language = "de"
)
english_w_confidence <- get_language_confidence(
  english,
  "lyrics",
  target_language = "en"
)

# manually inspect confidence scores
german_w_confidence |>
  arrange(share_target) |>
  select(share_target, mean_prob_target, lyrics_de) |>
  View()

english_w_confidence |>
  arrange(share_target) |>
  select(share_target, mean_prob_target, lyrics) |>
  View()

# keep only songs where at least 35% of the lines are classified as German and the mean confidence for German is above 0.0 (saves dialect, bilingual expressions in German lyrics)
german_filtered <- german_w_confidence |>
  filter(share_target > 0.35, mean_prob_target > 0.0)
# higher threshold for English, as there are more false positives (e.g., German songs with many English lines)
english_filtered <- english_w_confidence |>
  filter(share_target > 0.75, mean_prob_target > 0.4)


german_filtered$lyrics <- preprocess_lyrics(german_filtered$lyrics_en)
english_filtered$lyrics <- preprocess_lyrics(english_filtered$lyrics)
train_filtered <- bind_rows(german_filtered, english_filtered)
message(sprintf(
  "Lyrics available for %d%% of tracks in the train dat set.",
  round(nrow(train_filtered) / nrow(genres) * 100)
))

train_filtered$lyrics_de <- NULL
train_filtered$lyrics_en <- NULL
write_csv(
  train_filtered,
  "data/modeled_features/lyrics/lyrics_train_corpus.csv"
)

# PREPARE HOLDOUT CORPUS ----
holdout_annotated <- inner_join(
  raw_corpus,
  distinct(holdout, track.s.id),
  by = "track.s.id"
)
holdout_annotated$cat <- "holdout" # dummy for preprocessing function
has_lyrics <- holdout_annotated |> filter(!is.na(track.is.deepltranslation))
has_lyrics$n_lines <- sapply(has_lyrics$track.g.lyrics, nrow)
min_two_lines <- has_lyrics |> filter(n_lines > 1)
without_invalid <- min_two_lines |>
  filter(album.s.title != "No Grave but the Sea (Deluxe Edition)") # contains only "woof"
german_english_only <- without_invalid |>
  filter(track.language %in% c("German", "English"))
german <- build_german_corpus(german_english_only)
english <- build_english_corpus(german_english_only)
german_w_confidence <- get_language_confidence(
  german,
  "lyrics_de",
  target_language = "de"
)
english_w_confidence <- get_language_confidence(
  english,
  "lyrics",
  target_language = "en"
)
# keep only songs where at least 35% of the lines are classified as German and the mean confidence for German is above 0.0 (saves dialect, bilingual expressions in German lyrics)
german_filtered <- german_w_confidence |>
  filter(share_target > 0.35, mean_prob_target > 0.0)
# higher threshold for English, as there are more false positives (e.g., German songs with many English lines)
english_filtered <- english_w_confidence |>
  filter(share_target > 0.75, mean_prob_target > 0.4)

german_filtered$lyrics <- preprocess_lyrics(german_filtered$lyrics_en)
english_filtered$lyrics <- preprocess_lyrics(english_filtered$lyrics)
holdout_filtered <- bind_rows(german_filtered, english_filtered)
message(sprintf(
  "Lyrics available for %d%% of tracks in the holdout dat set.",
  round(nrow(holdout_filtered) / length(unique(holdout$track.s.id)) * 100)
))
holdout_filtered$lyrics_de <- NULL
holdout_filtered$lyrics_en <- NULL
write_csv(
  holdout_filtered,
  "data/modeled_features/lyrics/lyrics_holdout_corpus.csv"
)
