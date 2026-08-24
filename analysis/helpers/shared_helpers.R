get_artist_level_strata_split <- function(artist_strata, mb, test_size, seed) {
  set.seed(seed)
  split <- rsample::initial_split(
    artist_strata,
    prop = test_size,
    strata = "strata",
    pool = 0
  )
  train <- rsample::training(split) |>
    dplyr::select(track.s.firstartist.id, strata)
  test <- rsample::testing(split) |>
    dplyr::select(track.s.firstartist.id, strata)
  return(list(train = train, test = test))
}

extract_split_tracks <- function(mb, train, test) {
  train_tracks <- mb |>
    dplyr::filter(track.s.firstartist.id %in% train$track.s.firstartist.id) |>
    dplyr::pull(track.s.id)
  test_tracks <- mb |>
    dplyr::filter(track.s.firstartist.id %in% test$track.s.firstartist.id) |>
    dplyr::pull(track.s.id)
  return(list(train_tracks = train_tracks, test_tracks = test_tracks))
}

get_mode_with_tie_breaker <- function(x, tie_order) {
  #' Get the mode of a vector with a tie-breaking order.
  #' @param x A vector of values.
  #' @param tie_order A vector specifying the order of preference for tie-breaking.
  #' @return The mode of the vector, with ties broken according to the specified order.
  tab <- table(x)
  max_count <- max(tab)
  modes <- names(tab[tab == max_count])
  if (length(modes) == 1) {
    return(modes)
  } else {
    # break ties using the specified order
    for (tie in tie_order) {
      if (tie %in% modes) {
        return(tie)
      }
    }
  }
}


filter_artist_prevalence <- function(tags, min_prevalence, message_template) {
  thresh <- nrow(dplyr::distinct(tags, track.s.firstartist.id, tag_name)) *
    min_prevalence
  if (!is.null(message_template)) {
    message(sprintf(message_template, round(thresh), min_prevalence))
  }
  tags |>
    dplyr::group_by(tag_name) |>
    dplyr::filter(n_distinct(track.s.firstartist.id) >= thresh) |>
    dplyr::ungroup()
}
