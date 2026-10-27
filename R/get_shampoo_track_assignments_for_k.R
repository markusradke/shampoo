get_track_category_probabilities <- function(
  P,
  res_list,
  chosen_k,
  tiebreak_order = NULL,
  include_probabilities = TRUE
) {
  #' Get the probabilities of each track belonging to each genre category based on the final mapping from the genre category extraction process.
  #' @param P A normalized wide vote matrix where rows correspond to tracks and columns correspond to genre tags. Each entry represents the normalized vote count for a tag for a given track. Rows sum to 1, representing the distribution of votes across tags for each track. To obtain this matrix, use the \link{get_normalized_wide_vote_matrix} function with the appropriate parameters.
  #' @param res_list A list containing the results of the genre category extraction process, including mappings, weights, sizes, and processing order. To obtain this list, use the \link{get_genre_categories_from_graph} function with the appropriate parameters.
  #' @param chosen_k An integer specifying the number of genre categories to consider in the final mapping. The function will use the final mapping corresponding to the chosen_k value to calculate the probabilities of each track belonging to each genre category.
  #' @return A data frame containing the probabilities of each track belonging to each genre category. The data frame also contains the most likely genre category for each track and the corresponding probability of that category. As a tiebreaker and favoring a more nuanced picture, the function will choose the genre that is mentioned least often in the dataset if there is a tie in probabilities for a track across multiple categories.
  #' @seealso \link{get_normalized_wide_vote_matrix} and \link{get_genre_categories_from_graph} for obtaining the required input parameters.
  if (chosen_k < 1 || chosen_k > nrow(res_list$mappings[[1]])) {
    stop("chosen_k must correspond to an available category state.")
  }
  if (chosen_k == 1) {
    # if chosen_k is 1, return the most relevant genre for all tracks
    most_relevant_genre <- res_list$relevance_order[1]
    prob_track_map <- data.frame(
      cat = rep(most_relevant_genre, nrow(P)),
      cat_prob = rep(1, nrow(P))
    )
    if (!include_probabilities) {
      rownames(prob_track_map) <- rownames(P)
      return(prob_track_map)
    }
    prob_track_map[most_relevant_genre] <- 1
    return(prob_track_map)
  }
  P <- Matrix::Matrix(P, sparse = TRUE)
  if (is.null(tiebreak_order)) {
    tiebreak_order <- get_tag_name_ranks_for_size_tiebreaking(P)
  }
  G <- nrow(res_list$mappings[[1]])
  final_mapping <- res_list$mappings[[G - chosen_k + 1]]
  prob_track_map <- P %*% t(final_mapping)
  colnames(prob_track_map) <- colnames(final_mapping)

  final_genres <- res_list$final_genres[[G - chosen_k + 1]]
  prob_track_map <- prob_track_map[, final_genres, drop = FALSE]

  probability_matrix <- as.matrix(prob_track_map)
  cat_indices <- max.col(probability_matrix, ties.method = "first")
  cat_prob <- probability_matrix[
    cbind(seq_len(nrow(probability_matrix)), cat_indices)
  ]
  ties <- which(rowSums(
    probability_matrix == cat_prob
  ) > 1L)
  if (length(ties) > 0L) {
    tiebreak_indices <- match(
      colnames(probability_matrix),
      names(tiebreak_order)
    )
    for (row_index in ties) {
      tied <- which(
        probability_matrix[row_index, ] == cat_prob[row_index]
      )
      cat_indices[row_index] <- tied[which.min(tiebreak_indices[tied])]
    }
  }
  cat <- colnames(probability_matrix)[cat_indices]
  prob_track_map <- as.data.frame(probability_matrix)
  prob_track_map$cat <- cat
  prob_track_map$cat_prob <- cat_prob
  if (!include_probabilities) {
    return(data.frame(
      cat = cat,
      cat_prob = cat_prob,
      row.names = rownames(P)
    ))
  }
  prob_track_map
}


add_track_map_to_long <- function(long_dt, track_map) {
  #' Add mapped genre assignment probabilities to long genre tag data frame based on the most likely genre category for each track.
  #' @param long_dt A long data.table object containing columns: id, tag_name, tag
  #' count. Each row represents a track-tag pair, where id is the track identifier, tag_name is the genre tag name, and tag_count is the count of votes for that tag for the given track. This should be the same long data frame used to create the normalized wide vote matrix and the directed acyclic graph (DAG) of genre tags in the previous steps.
  #' @param track_map A data frame containing the probabilities of each track belonging to each genre category, as well as the most likely genre category and the corresponding probability for each track. The data frame should have columns: id, cat, cat_prob, and one column for each genre category representing the probability of that category for the given track. To obtain this data frame, use the \link{get_track_category_probabilities} function with the appropriate parameters.
  #' @return A long data frame containing the original track-tag pairs along with the mapped genre assignment probabilities for each track. The resulting data frame will have columns: id, tag_name, tag_count, cat, cat_prob, and one column for each genre category representing the probability of that category for the given track. It will also contain all original columns from the long_dt.
  #' @seealso \link{get_track_category_probabilities} for obtaining the required track mapping data frame.
  long_dt <- prepare_long_tag_data(long_dt)
  wide <- data.table::dcast(
    long_dt,
    id ~ tag_name,
    value.var = "tag_count",
    fill = 0
  )
  mapping <- cbind(id = wide$id, track_map)
  long_dt <- merge(long_dt, mapping, by = "id", all.x = TRUE)
  long_dt
}

get_tag_name_ranks_for_size_tiebreaking <- function(P) {
  P_binary <- as.matrix(P)
  P_binary[P_binary > 0] <- 1 # convert to binary matrix for number of appearances
  tag_name_counts <- colSums(as.matrix(P_binary))
  rank(tag_name_counts) |> sort()
}
