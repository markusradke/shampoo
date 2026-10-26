get_normalized_wide_vote_matrix <- function(long_dt) {
  #' Get normalized wide vote matrix from long genre tag data.table representing the distribution of votes for each genre tag across tracks.
  #'
  #' @param long_dt A long data.table containing columns: id, tag_name, tag_count
  #' @return A normalized wide vote matrix where rows correspond to tracks and columns correspond to genre tags. Each entry represents the normalized vote count for a tag for a given track. Rows sum to 1, representing the distribution of votes across tags for each track.
  #' @details The function takes a long data.table containing track-tag pairs and their corresponding vote counts, and converts it into a wide format where each row represents a track and each column represents a genre tag. If you have no detailed tag counts per track, we suggest you set these to one for all rows. The vote counts are then normalized for each track, so that the sum of votes across all tags for a given track equals 1. This normalized wide vote matrix can be used for further analysis, such as building a directed acyclic graph (DAG) of genre tags based on their co-occurrence and generality.
  #' @seealso \link{get_tag_dag_from_normalized_votes} for obtaining the directed acyclic graph of genre tags based on the normalized wide vote matrix.
  #' @export
  wide <- data.table::dcast(
    long_dt,
    track.s.id ~ tag_name,
    value.var = "tag_count",
    fill = 0
  )
  V <- as.matrix(wide[, -1, with = FALSE])
  Vt <- rowSums(V)
  # check for rows with zero sum and stop if any are found
  if (any(Vt == 0)) {
    stop(
      "Some tracks have zero total tag counts. Please check your input data."
    )
  }
  P <- V / Vt # normalize vote counts per track
  rownames(P) <- wide$track.s.id
  return(P)
}


get_tag_dag_from_normalized_votes <- function(P) {
  #' Get directed acyclic graph (DAG) of genre tags based on normalized vote matrix representing a hierarchical mapping of genre tags based on their co-occurrence and generality.
  #' @param P A normalized wide vote matrix where rows correspond to tracks and columns correspond to genre tags. Each entry represents the normalized vote count for a tag for a given track. To obtain this matrix, use the \link{get_normalized_wide_vote_matrix} function with the appropriate parameters.
  #' @return A list containing:
  #' - graph: An igraph object representing the directed acyclic graph of genre tags.
  #' - generality: A named vector of generality scores for each genre tag, where higher values indicate more general tags. Generality is calculated based on the weighted number of indegrees in the DAG, reflecting how many other tags act as subgenres for a given tag.
  #' @details The function takes a normalized wide vote matrix and calculates the joint and conditional probabilities of genre tags based on their co-occurrence across tracks. It then constructs an initital graph where edges represent the conditional probabilities of one tag given another, with weights corresponding to these probabilities. The generality of each tag is calculated based on the weighted number of indegrees in this initial graph, providing a measure of how many other tags act as subgenres for a given tag. Then, a directed acyclig graph (DAG) is contructed by allowing only connections form any given tag to tags with higher generality. The resulting DAG and generality scores can be used for further analysis, such as extracting genre categories and mapping tracks to these categories.
  #'@seealso \link{get_normalized_wide_vote_matrix} for obtaining the required input parameter.
  #' @export
  message("Calculating probabilities and building trees...")
  N <- nrow(P)
  G <- ncol(P)

  # Get marginals, joint and conditional probabilities
  P_g <- colMeans(P)
  eps <- 1e-12
  P_g_smooth <- pmax(P_g, eps) # prevent divide by zero
  joint_raw <- crossprod(P) / N # P(X, Y)
  cond_b_given_a <- sweep(joint_raw, 1, P_g_smooth, FUN = "/") # P(B | A) = P(A, B) / P(B), rows correspond to A
  weights <- cond_b_given_a
  diag(weights) <- 0 # so self-loops
  # get weighted number of indegrees as a measure of globality for a tag
  indegree_weights <- colSums(weights, na.rm = TRUE)

  message(sprintf(
    "Tags with top 20 generality (descending order):\n%s",
    toString(names(
      indegree_weights |> sort(decreasing = TRUE) |> utils::head(20)
    ))
  ))

  # Build DAG
  edges <- which(outer(indegree_weights, indegree_weights, "<"), arr.ind = TRUE) # allowed pairs
  E <- data.frame(
    from = colnames(P)[edges[, 1]],
    to = colnames(P)[edges[, 2]],
    weight = weights[edges]
  )
  E <- E[E$weight > 0, ]
  E$weight <- E$weight / ave(E$weight, E$from, FUN = sum)

  g <- igraph::graph_from_data_frame(E, directed = TRUE)
  # add all nodes to the graph, even if they have no edges
  g <- igraph::add_vertices(
    g,
    nv = G - igraph::vcount(g),
    name = setdiff(colnames(P), igraph::V(g)$name)
  )
  dangling <- igraph::V(g)[igraph::degree(g, mode = "out") == 0]$name
  message(sprintf("Nodes without any supergenres: \n%s", toString(dangling)))
  return(list(graph = g, generality = indegree_weights))
}


get_genre_categories_from_graph <- function(
  dag,
  P,
  lambda = 0.5,
  processing_order = NULL,
  fast = FALSE
) {
  #' Get genre categories from a directed acyclic graph (DAG) of genre tags based on normalized vote matrix representing a hierarchical mapping of genre tags based on their co-occurrence and generality.
  #' @param dag A list containing:
  #' - graph: An igraph object representing the directed acyclic graph of genre tags.
  #' - generality: A named vector of generality scores for each genre tag, where higher values indicate more general tags. Generality is calculated based on the weighted number of indegrees in the DAG, reflecting how many other tags act as subgenres for a given tag. To obtain this list, use the \link{get_tag_dag_from_normalized_votes} function with the appropriate parameters.
  #' @param P A normalized wide vote matrix where rows correspond to tracks and columns correspond to genre tags. Each entry represents the normalized vote count for a tag for a given track. To obtain this matrix, use the \link{get_normalized_wide_vote_matrix} function with the appropriate parameters.
  #' @param lambda A numeric value between 0 and 1 specifying the weight given to the generality of a genre tag when determining the order of processing. A higher value gives more weight to generality, while a lower value gives more weight to size. The default value is 0.5, which gives equal weight to both generality and size.
  #' @param processing_order An optional vector of genre tag names specifying the order in which to process the tags. If NULL, the function will determine the order based on the sizes of the genre categories. Size is calculated based on the number of tracks associated with each genre tag, where only the tags with highest probability for a track are counted counted as dominant genres.
  #' @param fast Will skip calulation of sizes and ginis for each step which greatly speeds up the process. Only possible if processing order is provided, else will fall back to FALSE. Default is FALSE.
  #' @return A list containing:
  #' - mappings: A list of mapping matrices representing the genre category mappings at each step of the folding process. Each mapping matrix has rows corresponding to genre tags and columns corresponding to genre categories, where each entry represents the weight of a genre tag in a given category.
  #' - weights: A list of weight matrices representing the weights of the edges in the DAG at each step of the folding process. Each weight matrix has rows and columns corresponding to genre tags, where each entry represents the weight of the edge from one tag to another.
  #' - sizes: A list of size vectors representing the sizes of the genre categories at each step of the folding process. Each size vector has entries corresponding to genre categories, where each entry represents the number of tracks associated with a given category.
  #' - ginis: A vector of Gini coefficients representing the inequality of the sizes of the genre categories at each step of the folding process. A higher Gini coefficient indicates greater inequality in the sizes of the categories.
  #' - processing_order: A vector of genre tag names representing the order in which the tags were processed during the folding process.
  #' - relevance_order: A vector of genre tag names representing the order of relevance of the tags, with dangling nodes (tags without supergenres) listed first, followed by the processed tags in reverse order of processing.
  #' - final_genres: A list of vectors representing the final genre categories at each step of the folding process. Each vector contains the names of the genre categories that remain after processing a given number of tags.
  #' @details The function processes the genre tags in the specified order (or based on sizes if no order is provided) and iteratively folds the DAG by merging subgenres into their supergenres. The resulting mappings, weights, and sizes are stored at each step of the process, allowing for analysis of the genre category structure at different levels of granularity. Mapping matrices are initialized as identity matrices, and weights are normalized to ensure that all out edges from a node sum to 1. Mapping is done by iteratively (1) updating the mapping matrix based on the weights of the edges in the DAG, (2) updating the weight matrix based on the updated mapping, and (3) calculating the sizes of the genre categories based on the updated mapping. The process continues until all genre tags have been processed, resulting in a final mapping of genre tags to genre categories. If you are interested in a specific set of genre tags, place it last in the processing order, and the function will return the mapping for that set of genre tags. The processing order of other genre tags before that does not matter for the final mapping of the specific set of genre tags, but it may affect the intermediate mappings and sizes of the genre categories.
  #' @seealso \link{get_normalized_wide_vote_matrix} and \link{get_tag_dag_from_normalized_votes} for obtaining the required input parameters.
  #' @export
  g <- dag$graph
  generality <- dag$generality
  dangling <- igraph::V(g)[igraph::degree(g, mode = "out") == 0]$name
  if (!is.null(processing_order)) {
    is_fixed_order <- TRUE
    processing_order <- setdiff(processing_order, dangling) # remove dangling nodes from processing order
    processing_order <- match(processing_order, colnames(P))
  } else {
    fast <- FALSE
    is_fixed_order <- FALSE
  }

  tiebreak_oder <- get_tag_name_ranks_for_size_tiebreaking(P)
  G <- ncol(P) - length(dangling) # number of genres to process
  P <- Matrix::Matrix(P, sparse = TRUE)
  weights <- t(igraph::as_adjacency_matrix(g, attr = "weight", sparse = FALSE))
  weights <- weights[colnames(P), colnames(P)] # reorder weights to match P

  rownames(weights) <- colnames(weights)

  if (!fast) {
    sizes <- calculate_sizes_from_track_mapping(P, tiebreak_oder)
  } else {
    sizes <- NULL
  }

  mapping <- diag(nrow(weights)) # initialize mapping weights as identity
  colnames(mapping) <- colnames(weights)

  processed_genres <- c()
  ginis <- c()
  mappings_states <- list()
  weights_states <- list()
  sizes_states <- list()
  final_genres_states <- list()

  current_weights <- weights
  current_mapping <- mapping
  current_track_mapping <- P
  current_sizes <- sizes
  current_final_genres <- colnames(P)
  if (!fast) {
    current_gini <- DescTools::Gini(sizes)
  } else {
    current_gini <- NULL
  }

  mappings_states[[1]] <- current_mapping
  weights_states[[1]] <- weights
  sizes_states[[1]] <- current_sizes
  ginis[1] <- current_gini
  final_genres_states[[1]] <- current_final_genres

  # process in order of initial sizes
  if (is_fixed_order) {
    next_genre <- processing_order[1]
  } else {
    sizes_wout_dangling <- sizes[setdiff(names(sizes), dangling)]
    sizes_ranks_norm <- rank(sizes_wout_dangling) / length(sizes_wout_dangling)
    generality_wout_dangling <- generality[setdiff(names(generality), dangling)]
    generality_ranks_norm <- rank(generality_wout_dangling) /
      length(generality_wout_dangling)
    score <- lambda * generality_ranks_norm + (1 - lambda) * sizes_ranks_norm
    next_genre <- match(names(which.min(score)), colnames(P))
  }
  i <- 1
  while (length(processed_genres) < G) {
    message(sprintf(
      "Processing genre %d of %d: %s",
      i,
      G,
      colnames(weights)[next_genre]
    ))

    weight_column <- current_weights[, next_genre]
    add_mapping <- weight_column %*% t(current_mapping[next_genre, ])
    current_mapping[next_genre, ] <- 0
    current_mapping <- current_mapping + add_mapping

    add_weights <- weight_column %*% t(current_weights[next_genre, ])
    current_weights[next_genre, ] <- 0
    current_weights <- current_weights + add_weights
    current_track_mapping <- P %*% t(current_mapping)
    colnames(current_track_mapping) <- colnames(P)
    processed_genres <- c(processed_genres, next_genre)
    if (!fast) {
      current_sizes <- calculate_sizes_from_track_mapping(
        current_track_mapping,
        tiebreak_oder
      )
      current_gini <- DescTools::Gini(current_sizes[-processed_genres])
      ginis[i + 1] <- current_gini
    } else {
      current_sizes <- NULL
      current_gini <- NULL
    }
    mappings_states[[i + 1]] <- current_mapping
    weights_states[[i + 1]] <- current_weights
    sizes_states[[i + 1]] <- current_sizes
    current_final_genres <- setdiff(
      colnames(P),
      colnames(P)[processed_genres]
    )
    final_genres_states[[i + 1]] <- current_final_genres
    ginis[i + 1] <- current_gini
    if (!is_fixed_order) {
      next_genre_pool <- setdiff(1:ncol(P), processed_genres)
      dangling_ids <- which(colnames(P) %in% dangling)
      next_genre_pool <- setdiff(next_genre_pool, dangling_ids)
      next_genre_pool_sizes_ranks <- rank(current_sizes[next_genre_pool]) /
        length(next_genre_pool)
      next_genre_pool_generality_ranks <- rank(generality[next_genre_pool]) /
        length(next_genre_pool)
      next_genre_pool_score <- lambda *
        next_genre_pool_generality_ranks +
        (1 - lambda) * next_genre_pool_sizes_ranks
      next_genre_name <- names(which.min(next_genre_pool_score))
      next_genre <- which(colnames(P) == next_genre_name)
    } else {
      next_genre <- processing_order[i + 1]
    }
    i <- i + 1
  }
  processing_order <- c(colnames(P)[processed_genres])
  relevance_order <- c(sort(dangling), rev(processing_order))
  message("Done.")
  return(list(
    mappings = mappings_states,
    weights = weights_states,
    sizes = sizes_states,
    ginis = ginis,
    processing_order = processing_order,
    relevance_order = relevance_order,
    final_genres = final_genres_states
  ))
}

calculate_sizes_from_track_mapping <- function(track_mapping, tiebreak_order) {
  message("Calculating track mapping...")
  winners <- apply(track_mapping, 1, function(x) {
    max_indices <- which(x == max(x))
    if (length(max_indices) > 1) {
      # tie breaker based on tiebreak_order
      tiebreak_indices <- match(
        colnames(track_mapping)[max_indices],
        names(tiebreak_order)
      )
      return(max_indices[which.min(tiebreak_indices)])
    } else {
      return(max_indices)
    }
  })
  winners_names <- colnames(track_mapping)[winners]
  sizes <- table(winners_names)
  missing <- setdiff(colnames(track_mapping), names(sizes))
  missing <- stats::setNames(rep(0, length(missing)), missing)
  sizes <- c(sizes, missing)
  sizes <- sizes[colnames(track_mapping)]
  message("Done.")
  sizes
}

get_track_category_probabilities <- function(
  P,
  res_list,
  chosen_k
) {
  #' Get the probabilities of each track belonging to each genre category based on the final mapping from the genre category extraction process.
  #' @param P A normalized wide vote matrix where rows correspond to tracks and columns correspond to genre tags. Each entry represents the normalized vote count for a tag for a given track. Rows sum to 1, representing the distribution of votes across tags for each track. To obtain this matrix, use the \link{get_normalized_wide_vote_matrix} function with the appropriate parameters.
  #' @param res_list A list containing the results of the genre category extraction process, including mappings, weights, sizes, and processing order. To obtain this list, use the \link{get_genre_categories_from_graph} function with the appropriate parameters.
  #' @param chosen_k An integer specifying the number of genre categories to consider in the final mapping. The function will use the final mapping corresponding to the chosen_k value to calculate the probabilities of each track belonging to each genre category.
  #' @return A data frame containing the probabilities of each track belonging to each genre category. The data frame also contains the most likely genre category for each track and the corresponding probability of that category. As a tiebreaker and favoring a more nuanced picture, the function will choose the genre that is mentioned least often in the dataset if there is a tie in probabilities for a track across multiple categories.
  #' @seealso \link{get_normalized_wide_vote_matrix} and \link{get_genre_categories_from_graph} for obtaining the required input parameters.
  #' @export
  if (chosen_k == 1) {
    # if chosen_k is 1, return the most relevant genre for all tracks
    most_relevant_genre <- res_list$relevance_order[1]
    prob_track_map <- data.frame(
      cat = rep(most_relevant_genre, nrow(P)),
      cat_prob = rep(1, nrow(P))
    )
    prob_track_map[most_relevant_genre] <- 1
    return(prob_track_map)
  }
  P <- Matrix::Matrix(P, sparse = TRUE)
  tiebreak_order <- get_tag_name_ranks_for_size_tiebreaking(P)
  G <- nrow(res_list$mappings[[1]])
  final_mapping <- res_list$mappings[[G - chosen_k + 1]]
  prob_track_map <- P %*% t(final_mapping)
  colnames(prob_track_map) <- colnames(final_mapping)

  final_genres <- res_list$final_genres[[G - chosen_k + 1]]
  prob_track_map <- prob_track_map[, final_genres]

  cat <- apply(prob_track_map, 1, function(x) {
    max_indices <- which(x == max(x))
    if (length(max_indices) > 1) {
      # tie breaker based on tiebreak_order
      tiebreak_indices <- match(
        colnames(prob_track_map)[max_indices],
        names(tiebreak_order)
      )
      return(colnames(prob_track_map)[max_indices[which.min(tiebreak_indices)]])
    } else {
      return(colnames(prob_track_map)[max_indices])
    }
  })

  cat_prob <- apply(prob_track_map, 1, max)
  prob_track_map <- as.data.frame(as.matrix(prob_track_map))
  prob_track_map$cat <- cat
  prob_track_map$cat_prob <- cat_prob
  prob_track_map
}


add_track_map_to_long <- function(long_dt, track_map) {
  #' Add mapped genre assignment probabilities to long genre tag data frame based on the most likely genre category for each track.
  #' @param long_dt A long data.table object containing columns: id, tag_name, tag
  #' count. Each row represents a track-tag pair, where id is the track identifier, tag_name is the genre tag name, and tag_count is the count of votes for that tag for the given track. This should be the same long data frame used to create the normalized wide vote matrix and the directed acyclic graph (DAG) of genre tags in the previous steps.
  #' @param track_map A data frame containing the probabilities of each track belonging to each genre category, as well as the most likely genre category and the corresponding probability for each track. The data frame should have columns: id, cat, cat_prob, and one column for each genre category representing the probability of that category for the given track. To obtain this data frame, use the \link{get_track_category_probabilities} function with the appropriate parameters.
  #' @return A long data frame containing the original track-tag pairs along with the mapped genre assignment probabilities for each track. The resulting data frame will have columns: id, tag_name, tag_count, cat, cat_prob, and one column for each genre category representing the probability of that category for the given track. It will also contain all original columns from the long_dt.
  #' @seealso \link{get_track_category_probabilities} for obtaining the required track mapping data frame.
  #' @export
  wide <- data.table::dcast(
    long_dt,
    track.s.id ~ tag_name,
    value.var = "tag_count",
    fill = 0
  )
  mapping <- cbind(track.s.id = wide$track.s.id, track_map)
  long_dt <- merge(long_dt, mapping, by = "track.s.id", all.x = TRUE)
  long_dt
}

get_tag_name_ranks_for_size_tiebreaking <- function(P) {
  P_binary <- P
  P_binary[P_binary > 0] <- 1 # convert to binary matrix for number of appearances
  tag_name_counts <- colSums(as.matrix(P_binary))
  rank(tag_name_counts) |> sort()
}
