validate_assignment_inputs <- function(categories, k, k_supplied = TRUE) {
  if (is.null(categories$mappings) || is.null(categories$final_genres)) {
    stop("categories must be returned by aggregate_genre_categories().")
  }
  if (!is.null(categories$target_set)) {
    target_k <- length(categories$target_set)
    if (k_supplied) {
      warning(
        sprintf(
          "k was ignored because this category object uses target_set; using k = %d.",
          target_k
        )
      )
    }
    return(target_k)
  }
  if (
    !is.numeric(k) ||
      any(!is.finite(k)) ||
      any(k != as.integer(k)) ||
      any(k < 1L) ||
      any(k > nrow(categories$mappings[[1]]))
  ) {
    stop("k must contain available positive integer category counts.")
  }
  terminal_nodes <- categories$terminal_nodes
  if (is.null(terminal_nodes)) {
    terminal_nodes <- categories$final_genres[[length(
      categories$final_genres
    )]]
  }
  minimum_k <- length(terminal_nodes)
  clipped_k <- pmax(k, minimum_k)
  if (any(k < minimum_k)) {
    warning(
      sprintf(
        "Requested k values below the %d terminal node(s) were clipped to %d.",
        minimum_k,
        minimum_k
      ),
      call. = FALSE
    )
  }
  unique(clipped_k)
}


get_track_category_probabilities <- function(
  P,
  res_list,
  chosen_k,
  tiebreak_order = NULL,
  include_probabilities = TRUE
) {
  #' Get the probabilities of each track belonging to each genre category based on the final mapping from the genre category extraction process.
  #' @param P A normalized vote matrix (dense or sparse) where rows correspond to tracks and columns correspond to genre tags. Each entry represents the normalized vote count for a tag for a given track. Rows sum to 1, representing the distribution of votes across tags for each track. To obtain this matrix, use the \link{get_normalized_vote_matrix} function with the appropriate parameters.
  #' @param res_list A list containing the results of the genre category extraction process, including mappings, weights, sizes, and processing order. To obtain this list, use the \link{get_genre_categories_from_graph} function with the appropriate parameters.
  #' @param chosen_k An integer specifying the number of genre categories to consider in the final mapping. The function will use the final mapping corresponding to the chosen_k value to calculate the probabilities of each track belonging to each genre category.
  #' @return A data frame containing the probabilities of each track belonging to each genre category. The data frame also contains the most likely genre category for each track and the corresponding probability of that category. As a tiebreaker and favoring a more nuanced picture, the function will choose the genre that is mentioned least often in the dataset if there is a tie in probabilities for a track across multiple categories.
  #' @seealso \link{get_normalized_vote_matrix} and \link{get_genre_categories_from_graph} for obtaining the required input parameters.
  n_tags <- nrow(res_list$mappings[[1]])
  if (chosen_k < 1 || chosen_k > n_tags) {
    stop("chosen_k must correspond to an available category state.")
  }
  if (chosen_k == 1) {
    # if chosen_k is 1, return the most relevant genre for all tracks
    most_relevant_genre <- res_list$relevance_order[1]
    prob_track_map <- data.frame(
      cat = rep(most_relevant_genre, nrow(P)),
      cat_prob = rep(1, nrow(P)),
      row.names = rownames(P)
    )
    if (include_probabilities) {
      prob_track_map[[most_relevant_genre]] <- rep(1, nrow(P))
    }
    return(prob_track_map)
  }
  if (is.null(tiebreak_order)) {
    tiebreak_order <- get_tag_name_ranks_for_size_tiebreaking(P)
  }
  state <- n_tags - chosen_k + 1
  final_genres <- res_list$final_genres[[state]]
  probability_matrix <- as.matrix(
    P %*% t(res_list$mappings[[state]][final_genres, , drop = FALSE])
  )
  cat_indices <- calculate_winners(probability_matrix, tiebreak_order)
  prob_track_map <- as.data.frame(probability_matrix)
  prob_track_map$cat <- colnames(probability_matrix)[cat_indices]
  prob_track_map$cat_prob <- probability_matrix[
    cbind(seq_len(nrow(probability_matrix)), cat_indices)
  ]
  if (include_probabilities) {
    prob_track_map
  } else {
    prob_track_map[c("cat", "cat_prob")]
  }
}

get_tag_name_ranks_for_size_tiebreaking <- function(P) {
  rank(Matrix::colSums(P > 0)) |> sort()
}

calculate_winners <- function(track_mapping, tiebreak_order) {
  #' Get the column index of the dominant category per track. Ties are won by the category ranked first in `tiebreak_order`.
  tiebreak_columns <- order(match(
    colnames(track_mapping),
    names(tiebreak_order)
  ))
  reordered <- track_mapping[, tiebreak_columns, drop = FALSE]
  tiebreak_columns[max.col(reordered, ties.method = "first")]
}

get_category_tags <- function(categories) {
  tags <- categories$tags
  if (is.null(tags)) {
    tags <- colnames(categories$mappings[[1]])
  }
  tags
}

prepare_assignment_track_info <- function(data, known_tags) {
  is_known <- data$tag_name %in% known_tags
  unknown_tags <- unique(as.character(data$tag_name[!is_known]))
  if (length(unknown_tags) > 0L) {
    warning(
      sprintf(
        "Ignoring %d unknown tag(s): %s",
        length(unknown_tags),
        if (length(unknown_tags) < 50L) {
          paste(unknown_tags, collapse = ", ")
        } else {
          "list suppressed because there are 50 or more"
        }
      ),
      call. = FALSE
    )
  }
  recognized_data <- data[is_known]
  list(
    data = recognized_data,
    ids = unique(as.character(data$id)),
    recognized_ids = unique(as.character(recognized_data$id)),
    unknown_ids = unique(as.character(data$id[!is_known]))
  )
}

complete_assignment_rows <- function(probabilities, ids) {
  probabilities <- probabilities[
    match(ids, rownames(probabilities)),
    ,
    drop = FALSE
  ]
  probabilities$id <- ids
  rownames(probabilities) <- NULL
  probabilities[c("id", setdiff(names(probabilities), "id"))]
}

get_assignment_status <- function(ids, track_info) {
  status <- ifelse(
    ids %in% track_info$unknown_ids,
    "partial_unknown_tags",
    "assigned"
  )
  status[!ids %in% track_info$recognized_ids] <- "no_recognized_tags"
  status
}

format_assignment_output <- function(probabilities, method) {
  status_column <- intersect("assignment_status", names(probabilities))
  hard_columns <- c("cat", "cat_prob")
  switch(
    method,
    hard = probabilities[c("id", hard_columns, status_column)],
    fuzzy = probabilities[c(
      setdiff(names(probabilities), c(hard_columns, status_column)),
      status_column
    )],
    both = probabilities
  )
}
