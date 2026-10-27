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
  #' @param fast Will skip calculation of sizes and metrics for each step, which greatly speeds up the process. Only possible if processing order is provided, else will fall back to FALSE. Default is FALSE.
  #' @return A list containing:
  #' - mappings: A list of mapping matrices representing the genre category mappings at each step of the folding process. Each mapping matrix has rows corresponding to genre tags and columns corresponding to genre categories, where each entry represents the weight of a genre tag in a given category.
  #' - weights: A list of weight matrices representing the weights of the edges in the DAG at each step of the folding process. Each weight matrix has rows and columns corresponding to genre tags, where each entry represents the weight of the edge from one tag to another.
  #' - sizes: A list of size vectors representing the sizes of the genre categories at each step of the folding process. Each size vector has entries corresponding to genre categories, where each entry represents the number of tracks associated with a given category.
  #' - metrics: A data frame with columns \code{k}, \code{gini}, and \code{onehot_acc}, containing one row per folding state.
  #' - processing_order: A vector of genre tag names representing the order in which the tags were processed during the folding process.
  #' - relevance_order: A vector of genre tag names representing the order of relevance of the tags, with dangling nodes (tags without supergenres) listed first, followed by the processed tags in reverse order of processing.
  #' - final_genres: A list of vectors representing the final genre categories at each step of the folding process. Each vector contains the names of the genre categories that remain after processing a given number of tags.
  #' @details The function processes the genre tags in the specified order (or based on sizes if no order is provided) and iteratively folds the DAG by merging subgenres into their supergenres. The resulting mappings, weights, and sizes are stored at each step of the process, allowing for analysis of the genre category structure at different levels of granularity. Mapping matrices are initialized as identity matrices, and weights are normalized to ensure that all out edges from a node sum to 1. Mapping is done by iteratively (1) updating the mapping matrix based on the weights of the edges in the DAG, (2) updating the weight matrix based on the updated mapping, and (3) calculating the sizes of the genre categories based on the updated mapping. The process continues until all genre tags have been processed, resulting in a final mapping of genre tags to genre categories. If you are interested in a specific set of genre tags, place it last in the processing order, and the function will return the mapping for that set of genre tags. The processing order of other genre tags before that does not matter for the final mapping of the specific set of genre tags, but it may affect the intermediate mappings and sizes of the genre categories.
  #' @seealso \link{get_normalized_wide_vote_matrix} and \link{get_tag_dag_from_normalized_votes} for obtaining the required input parameters.
  #' @export
  prepared <- prepare_category_extraction(dag, P, processing_order, fast)
  states <- initialize_category_states(prepared, lambda)
  states <- fold_category_states(states, prepared, lambda)
  states$processing_order <- colnames(prepared$P)[states$processed_genres]
  states$relevance_order <- c(
    sort(prepared$dangling),
    rev(states$processing_order)
  )
  states$final_genres <- states$final_genres_states
  states$metrics <- if (prepared$fast) {
    NULL
  } else {
    data.frame(
      k = lengths(states$final_genres_states),
      gini = states$ginis,
      onehot_acc = states$accuracy
    )
  }
  states[c(
    "mappings",
    "weights",
    "sizes",
    "metrics",
    "processing_order",
    "relevance_order",
    "final_genres"
  )]
}

prepare_category_extraction <- function(
  dag,
  P,
  processing_order,
  fast
) {
  g <- dag$graph
  tags <- colnames(P)
  dangling <- igraph::V(g)[igraph::degree(g, mode = "out") == 0]$name
  fixed_order <- !is.null(processing_order)
  if (fixed_order) {
    processing_order <- match(setdiff(processing_order, dangling), tags)
  } else {
    fast <- FALSE
  }
  P <- Matrix::Matrix(P, sparse = TRUE)
  weights <- t(igraph::as_adjacency_matrix(g, attr = "weight", sparse = FALSE))
  weights <- weights[tags, tags]
  rownames(weights) <- tags
  mapping <- diag(length(tags))
  dimnames(mapping) <- list(tags, tags)
  tiebreak_order <- get_tag_name_ranks_for_size_tiebreaking(P)
  baseline_winners <- calculate_winners(P, tiebreak_order)
  list(
    P = P,
    weights = weights,
    mapping = mapping,
    tags = tags,
    dangling = dangling,
    generality = dag$generality,
    processing_order = processing_order,
    fixed_order = fixed_order,
    fast = fast,
    G = length(tags) - length(dangling),
    tiebreak_order = tiebreak_order,
    baseline_winners = baseline_winners
  )
}

initialize_category_states <- function(prepared, lambda) {
  sizes <- if (prepared$fast) {
    NULL
  } else {
    calculate_sizes_from_track_mapping(
      prepared$P,
      prepared$tiebreak_order
    )
  }
  states <- list(
    mappings = vector("list", prepared$G + 1L),
    weights = vector("list", prepared$G + 1L),
    sizes = vector("list", prepared$G + 1L),
    final_genres_states = vector("list", prepared$G + 1L),
    ginis = numeric(prepared$G + 1L),
    accuracy = numeric(prepared$G + 1L),
    processed_genres = integer(prepared$G)
  )
  states$mappings[[1]] <- prepared$mapping
  states$weights[[1]] <- prepared$weights
  states$sizes[[1]] <- sizes
  states$final_genres_states[[1]] <- prepared$tags
  if (!prepared$fast) {
    state_metrics <- calculate_state_metrics(
      prepared$P,
      prepared$tiebreak_order,
      prepared$baseline_winners,
      sizes,
      integer()
    )
    states$ginis[1] <- state_metrics$gini
    states$accuracy[1] <- state_metrics$onehot_acc
  } else {
    states$ginis <- NULL
    states$accuracy <- NULL
    states$sizes <- list()
  }
  states
}

fold_category_states <- function(states, prepared, lambda) {
  next_genre <- select_next_genre(
    states$sizes[[1]],
    prepared,
    integer(),
    lambda
  )
  for (i in seq_len(prepared$G)) {
    message(sprintf(
      "Processing genre %d of %d: %s",
      i,
      prepared$G,
      prepared$tags[next_genre]
    ))
    states$processed_genres[i] <- next_genre
    states$mappings[[i + 1]] <- fold_mapping(
      states$mappings[[i]],
      states$weights[[i]],
      next_genre
    )
    states$weights[[i + 1]] <- fold_weights(
      states$weights[[i]],
      next_genre
    )
    active_genres <- setdiff(
      prepared$tags,
      prepared$tags[states$processed_genres[seq_len(i)]]
    )
    states$final_genres_states[[i + 1]] <- active_genres
    if (!prepared$fast) {
      track_mapping <- prepared$P %*% t(states$mappings[[i + 1]])
      colnames(track_mapping) <- prepared$tags
      states$sizes[[i + 1]] <- calculate_sizes_from_track_mapping(
        track_mapping,
        prepared$tiebreak_order
      )
      state_metrics <- calculate_state_metrics(
        track_mapping,
        prepared$tiebreak_order,
        prepared$baseline_winners,
        states$sizes[[i + 1]],
        states$processed_genres[seq_len(i)]
      )
      states$ginis[i + 1] <- state_metrics$gini
      states$accuracy[i + 1] <- state_metrics$onehot_acc
    }
    if (i < prepared$G) {
      next_genre <- select_next_genre(
        states$sizes[[i + 1]],
        prepared,
        states$processed_genres[seq_len(i)],
        lambda
      )
    }
  }
  states
}

select_next_genre <- function(sizes, prepared, processed_genres, lambda) {
  if (prepared$fixed_order) {
    return(prepared$processing_order[length(processed_genres) + 1L])
  }
  pool <- setdiff(seq_along(prepared$tags), processed_genres)
  pool <- pool[!prepared$tags[pool] %in% prepared$dangling]
  size_ranks <- rank(sizes[pool]) / length(pool)
  generality_ranks <- rank(prepared$generality[pool]) / length(pool)
  score <- lambda * generality_ranks + (1 - lambda) * size_ranks
  match(names(which.min(score)), prepared$tags)
}

fold_mapping <- function(mapping, weights, genre) {
  weight_column <- weights[, genre]
  add_mapping <- weight_column %*% t(mapping[genre, ])
  mapping[genre, ] <- 0
  mapping + add_mapping
}

fold_weights <- function(weights, genre) {
  weight_column <- weights[, genre]
  add_weights <- weight_column %*% t(weights[genre, ])
  weights[genre, ] <- 0
  weights + add_weights
}

calculate_one_hot_accuracy <- function(
  track_mapping,
  tiebreak_order,
  baseline_winners
) {
  predicted <- calculate_winners(
    track_mapping,
    tiebreak_order
  )
  mean(predicted == baseline_winners)
}

calculate_state_metrics <- function(
  track_mapping,
  tiebreak_order,
  baseline_winners,
  sizes,
  processed_genres
) {
  list(
    gini = DescTools::Gini(
      if (length(processed_genres) == 0) {
        sizes
      } else {
        sizes[-processed_genres]
      }
    ),
    onehot_acc = calculate_one_hot_accuracy(
      track_mapping,
      tiebreak_order,
      baseline_winners
    )
  )
}

calculate_sizes_from_track_mapping <- function(track_mapping, tiebreak_order) {
  message("Calculating track mapping...")
  winners <- calculate_winners(track_mapping, tiebreak_order)
  winners_names <- colnames(track_mapping)[winners]
  sizes <- table(winners_names)
  missing <- setdiff(colnames(track_mapping), names(sizes))
  missing <- stats::setNames(rep(0, length(missing)), missing)
  sizes <- c(sizes, missing)
  sizes <- sizes[colnames(track_mapping)]
  message("Done.")
  sizes
}

calculate_winners <- function(track_mapping, tiebreak_order) {
  tiebreak_columns <- order(match(
    colnames(track_mapping),
    names(tiebreak_order)
  ))
  reordered <- track_mapping[, tiebreak_columns, drop = FALSE]
  tiebreak_columns[max.col(as.matrix(reordered), ties.method = "first")]
}
