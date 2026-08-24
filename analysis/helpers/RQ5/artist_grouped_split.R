source("analysis/helpers/shared_helpers.R")

split_artist_level_genres <- function(
  mb,
  cat_states,
  chosen_k,
  train_size = 0.8,
  n_bins = 5,
  seed = 42
) {
  #' Split a long data frame of music tracks into train and test sets at the artist level, stratified by popularity and release year.
  #' @param mb A long data frame of music tracks with columns for artist ID, popularity, and release year.
  #' @param test_size The proportion of the data to include in the test set (default is 0.8).
  #' @param seed A random seed for reproducibility (default is 42).
  #' @return A list containing two elements: train_tracks and test_tracks, which are vectors of track IDs for the training and testing sets, respectively.
  artist_strata <- get_artist_strata_genres(mb, cat_states, chosen_k)
  split <- get_artist_level_strata_split(artist_strata, mb, train_size, seed)
  split_tracks <- extract_split_tracks(mb, split$train, split$test)
  split_tracks
}


get_artist_strata_genres <- function(mb, cat_states, chosen_k) {
  P <- get_normalized_wide_vote_matrix(mb)
  trackmap <- get_track_category_probabilities(P, cat_states, chosen_k)
  trackmap_ids <- add_track_map_to_long(mb, trackmap) |>
    distinct(track.s.id, track.s.firstartist.id, cat)

  cat_tie_order <- count(trackmap_ids, cat) |>
    arrange(n) |>
    pull(cat)
  artist_genres <- trackmap_ids |>
    group_by(track.s.firstartist.id) |>
    summarize(
      strata = get_mode_with_tie_breaker(cat, cat_tie_order),
      .groups = "drop"
    )
  artist_genres
}

get_corpus_ids_with_labels <- function(
  ids,
  mb,
  cat_states,
  chosen_k
) {
  corpus <- mb |>
    filter(track.s.id %in% ids)
  P <- get_normalized_wide_vote_matrix(corpus)
  trackmap <- get_track_category_probabilities(P, cat_states, chosen_k)
  trackmap_ids <- add_track_map_to_long(corpus, trackmap) |>
    distinct(track.s.id, cat, cat_prob) |>
    as.data.frame()
  trackmap_ids
}

get_corpus_labels <- function(mb, cat_states, train_ids, holdout_ids, ks) {
  labels <- list()
  for (k in ks) {
    train <- get_corpus_ids_with_labels(
      train_ids,
      mb,
      cat_states,
      k
    )
    holdout <- get_corpus_ids_with_labels(
      holdout_ids,
      mb,
      cat_states,
      k
    )
    labels[[as.character(k)]] <- list(
      train = train,
      holdout = holdout
    )
  }
  labels
}
