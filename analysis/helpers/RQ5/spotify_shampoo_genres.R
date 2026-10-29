source("analysis/helpers/shared_helpers.R")

get_spotify_shampoo_map <- function(
  spotify,
  train_holdout,
  min_artist_prev,
  lambda = 0.5,
  max_k = 30
) {
  #' Build a SHAMPOO mapping of Spotify artist genres to categories using the training data and apply it to the holdout data.
  #' @param spotify A data.table containing Spotify genre information. Columns should include id, tag_name, and tag_count.
  #' @param train_holdout A list containing character vectors with training and holdout track IDs.
  #' @param min_artist_prev A numeric value specifying the minimum prevalence of a genre tag on unique artist level in the training data.
  #' @param lambda A numeric value specifying the lambda parameter for the SHAMPOO algorithm.
  #' @param max_k The maximum number of distribution genre categories.
  #' @return A list containing data frames with the training and holdout features with Spotify genre mappings.
  #' @details This function processes Spotify artist genre data with the SHAMPOO algorithm to create a mapping of genres to categories based on the training data. The number of categories is the local Gini minimum (at most `max_k`) with the lowest Gini. The mapping is applied to the holdout data, ensuring that only genres present in the training data are considered for the holdout set. The function also filters out binary genres and low mean probability genres (< 0.001 in the training data).
  spotify_train <- build_spotify_train(
    spotify = spotify,
    train_tracks = train_holdout$train_tracks,
    min_artist_prev
  )

  # filter out binary genres (present in either all or none of the training tracks)
  binary_cols <- get_binary_columns(
    shampoo::create_genre_dag(spotify_train)$tag_matrix
  )
  spotify_train_filtered <- filter(spotify_train, !tag_name %in% binary_cols)
  spotify_train_dag <- shampoo::create_genre_dag(spotify_train_filtered)
  spotify_holdout <- spotify |>
    filter(
      id %in% train_holdout$test_tracks,
      tag_name %in% spotify_train_dag$tags
    )

  cat_states_s <- shampoo::aggregate_genre_categories(
    spotify_train_dag,
    lambda = lambda
  )
  saveRDS(cat_states_s, "models/dag/cat_states_spotify.rds")
  chosen_k <- choose_k_from_gini(cat_states_s$metrics, max_k)

  s_trackmap_train <- shampoo::assign_genre_categories(
    spotify_train_filtered,
    cat_states_s,
    chosen_k
  )
  to_remove <- get_low_mean_cols(s_trackmap_train)
  s_trackmap_holdout <- shampoo::assign_genre_categories(
    spotify_holdout,
    cat_states_s,
    chosen_k
  )
  message(sprintf(
    "Using %d Spotify distribution genre categories.",
    chosen_k - length(to_remove)
  ))

  list(
    train = build_track_features(s_trackmap_train, to_remove),
    holdout = build_track_features(s_trackmap_holdout, to_remove)
  )
}

build_spotify_train <- function(spotify, train_tracks, min_artist_prev) {
  spotify |>
    filter(id %in% train_tracks) |>
    filter_artist_prevalence(
      min_prevalence = min_artist_prev,
      message_template = "Filtering Spotify training data to artists with at least %d tracks (%.4f prevalence)"
    ) |>
    as.data.table()
}

get_binary_columns <- function(P_s_train) {
  binary_mask <- apply(P_s_train, 2, function(x) all(x %in% c(0, 1)))
  colnames(P_s_train)[binary_mask]
}

choose_k_from_gini <- function(metrics, max_k) {
  #' Choose the number of categories with the lowest Gini among the local Gini minima up to `max_k`.
  ginis <- metrics[order(metrics$k), ]
  local_min <- which(diff(sign(diff(ginis$gini))) == 2) + 1
  local_min <- local_min[ginis$k[local_min] <= max_k]
  ginis$k[local_min[which.min(ginis$gini[local_min])]]
}

get_low_mean_cols <- function(s_trackmap_train, min_mean_prob = 0.001) {
  mean_probs <- colMeans(s_trackmap_train[get_category_columns(
    s_trackmap_train
  )])
  names(mean_probs)[mean_probs < min_mean_prob]
}

build_track_features <- function(trackmap, to_remove) {
  #' Build one row of Spotify distribution genre probabilities per track, prefixed with "sp_" and with syntactic column names.
  trackmap[c("id", setdiff(get_category_columns(trackmap), to_remove))] |>
    rename(track.s.id = id) |>
    rename_with(~ paste0("sp_", gsub("[ &-]", "_", .x)), -track.s.id)
}
