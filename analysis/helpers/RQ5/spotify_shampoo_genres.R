source("analysis/helpers/shared_helpers.R")

get_spotify_shampoo_map <- function(
  spotify,
  train_holdout,
  min_artist_prev,
  lambda = 0.5
) {
  #' Build a SHAMPOO mapping of Spotify artist genres to categories using the training data and apply it to the holdout data.
  #' @param spotify A data.table containing Spotify genre information. Columns should include track.s.id, tag_name, and tag_count.
  #' @param train_holdout A list containing character vectors with training and holdout track IDs.
  #' @param min_artist_prev A numeric value specifying the minimum prevalence of a genre tag on unique artist level in the training data.
  #' @param lambda A numeric value specifying the lambda parameter for the SHAMPOO algorithm.
  #' @return A list containing data frames with the training and holdout features with Spotify genre mappings.
  #' @details This function processes Spotify artist genre data with the SHAMPOO algorithm to create a mapping of genres to categories based on the training data. It then applies this mapping to the holdout data, ensuring that only genres present in the training data are considered for the holdout set. The function also filters out binary genres and low mean probability genres (< 0.001 in the training data). If the SHAMPOO mapping has already been computed and saved, it will load the existing mapping instead of recomputing it; if you wish to recompute the mapping, delete the file "models/dag/cat_states_spotify.rds" before running this function.
  spotify_train <- build_spotify_train(
    spotify = spotify,
    train_tracks = train_holdout$train_tracks,
    min_artist_prev
  )
  P_s_train <- get_normalized_wide_vote_matrix(spotify_train)
  binary_cols <- get_binary_columns(P_s_train)
  spotify_train_filtered <- filter_binary_genres(
    spotify_train = spotify_train,
    binary_columns = binary_cols
  )

  P_s_train_filtered <- get_normalized_wide_vote_matrix(
    spotify_train_filtered
  )

  spotify_holdout <- build_spotify_holdout(
    spotify = spotify,
    test_tracks = train_holdout$test_tracks,
    train_tag_cols = colnames(P_s_train_filtered)
  )

  dag_s_train <- get_tag_dag_from_normalized_votes(P_s_train_filtered)
  cat_states_s <- get_or_build_cat_states_spotify(
    dag_s_train = dag_s_train,
    P_s_train = P_s_train_filtered,
    lambda
  )

  ginis <- build_gini_frame(cat_states_s)
  chosen_k <- choose_k_from_gini(ginis)

  s_trackmap_train <- get_track_category_probabilities(
    P_s_train_filtered,
    cat_states_s,
    chosen_k
  )

  to_remove <- get_low_mean_cols(s_trackmap_train)
  s_features_train <- build_track_features(
    spotify_train_filtered,
    s_trackmap_train,
    to_remove
  )

  P_s_holdout <- get_normalized_wide_vote_matrix(spotify_holdout)
  P_s_holdout_padded <- pad_vote_matrix(
    P_s_holdout,
    colnames(P_s_train_filtered)
  )

  s_trackmap_holdout <- get_track_category_probabilities(
    P_s_holdout_padded,
    cat_states_s,
    chosen_k
  )

  s_features_holdout <- build_track_features(
    spotify_holdout,
    s_trackmap_holdout,
    to_remove
  )

  list(
    train = s_features_train,
    holdout = s_features_holdout
  )
}

build_spotify_train <- function(spotify, train_tracks, min_artist_prev) {
  spotify |>
    filter(track.s.id %in% train_tracks) |>
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

filter_binary_genres <- function(spotify_train, binary_columns) {
  spotify_train |>
    filter(!tag_name %in% binary_columns)
}

build_spotify_holdout <- function(spotify, test_tracks, train_tag_cols) {
  spotify |>
    filter(track.s.id %in% test_tracks) |>
    filter(tag_name %in% train_tag_cols)
}

get_or_build_cat_states_spotify <- function(
  dag_s_train,
  P_s_train,
  lambda = 0.5
) {
  if (file.exists("models/dag/cat_states_spotify.rds")) {
    readRDS("models/dag/cat_states_spotify.rds")
  } else {
    cat_states_s <- get_genre_categories_from_graph(
      dag_s_train,
      P_s_train,
      lambda = lambda
    )
    saveRDS(cat_states_s, "models/dag/cat_states_spotify.rds")
    cat_states_s
  }
}

build_gini_frame <- function(cat_states_s) {
  data.frame(
    k = rev(cat_states_s$metrics$k),
    gini = rev(cat_states_s$metrics$gini)
  )
}

choose_k_from_gini <- function(ginis) {
  local_min <- which(diff(sign(diff(ginis$gini))) == 2) + 1
  local_min <- local_min[local_min <= 30]
  chosen_k <- local_min[which.min(ginis$gini[local_min])]
  chosen_k
}

get_low_mean_cols <- function(s_trackmap_train) {
  meanprobs <- s_trackmap_train |>
    select(-cat, -cat_prob) |>
    as.matrix() |>
    colMeans() |>
    sort(decreasing = TRUE)

  to_remove <- names(meanprobs[meanprobs < 0.001])
  if (length(to_remove) < 0) {
    to_remove <- names(max(meanprobs))
  }
  to_remove
}

build_track_features <- function(
  spotify_long,
  trackmap,
  to_remove
) {
  add_track_map_to_long(
    spotify_long,
    trackmap
  ) |>
    select(track.s.id, all_of(colnames(trackmap))) |>
    select(-cat, -cat_prob) |>
    select(-all_of(to_remove)) |>
    distinct(track.s.id, .keep_all = TRUE) |>
    rename_with(~ paste0("sp_", .), -track.s.id) |>
    rename_with(~ gsub(" ", "_", .), -track.s.id) |>
    rename_with(~ gsub("&", "_", .), -track.s.id) |>
    rename_with(~ gsub("-", "_", .), -track.s.id)
}


pad_vote_matrix <- function(P_s_holdout, colnames_train) {
  missing_cols <- setdiff(colnames_train, colnames(P_s_holdout))
  P_s_padded <- cbind(
    P_s_holdout,
    matrix(0, nrow = nrow(P_s_holdout), ncol = length(missing_cols))
  )
  colnames(P_s_padded) <- c(colnames(P_s_holdout), missing_cols)
  P_s_padded[, colnames_train]
}
