source("analysis/helpers/shared_helpers.R")

split_artist_level_genres <- function(
  mb,
  cat_states,
  chosen_k,
  train_size = 0.8,
  n_bins = 5,
  seed = 42
) {
  #' Split a long data frame of music tracks into train and test sets at the artist level, stratified by the artists' modal SHAMPOO category (ties go to the rarest category).
  #' @param mb A long data frame of music tracks with columns id, track.s.firstartist.id, tag_name, and tag_count.
  #' @param train_size The proportion of artists in the training set (default is 0.8).
  #' @param seed A random seed for reproducibility (default is 42).
  #' @return A list containing two elements: train_tracks and test_tracks, which are vectors of track IDs for the training and testing sets, respectively.
  artist_strata <- get_artist_strata_genres(mb, cat_states, chosen_k)
  split <- get_artist_level_strata_split(artist_strata, mb, train_size, seed)
  extract_split_tracks(mb, split$train, split$test)
}


get_artist_strata_genres <- function(mb, cat_states, chosen_k) {
  trackmap <- shampoo::assign_genre_categories(
    mb,
    cat_states,
    chosen_k,
    method = "hard"
  )
  trackmap_ids <- distinct(mb, id, track.s.firstartist.id) |>
    inner_join(trackmap, by = "id") |>
    distinct(id, track.s.firstartist.id, cat)

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

get_corpus_ids_with_labels <- function(ids, mb, cat_states, chosen_k) {
  shampoo::assign_genre_categories(
    filter(mb, id %in% ids),
    cat_states,
    chosen_k,
    method = "hard"
  ) |>
    select(track.s.id = id, cat, cat_prob)
}

get_corpus_labels <- function(mb, cat_states, train_ids, holdout_ids, ks) {
  labels <- lapply(ks, function(k) {
    list(
      train = get_corpus_ids_with_labels(train_ids, mb, cat_states, k),
      holdout = get_corpus_ids_with_labels(holdout_ids, mb, cat_states, k)
    )
  })
  stats::setNames(labels, ks)
}
