get_genre_categories_from_graph <- function(
  dag,
  P,
  lambda = 0.5,
  processing_order = NULL,
  fast = FALSE,
  terminal_nodes = NULL
) {
  #' Get genre categories from a directed acyclic graph (DAG) of genre tags based on normalized vote matrix representing a hierarchical mapping of genre tags based on their co-occurrence and generality.
  #' @param dag A list containing:
  #' - graph: An igraph object representing the directed acyclic graph of genre tags.
  #' - generality: A named vector of generality scores for each genre tag, where higher values indicate more general tags. Generality is calculated based on the weighted number of indegrees in the DAG, reflecting how many other tags act as subgenres for a given tag. To obtain this list, use the \link{get_tag_dag_from_normalized_votes} function with the appropriate parameters.
  #' @param P A normalized vote matrix where rows correspond to tracks and columns correspond to genre tags. Each entry represents the normalized vote count for a tag for a given track. To obtain this matrix, use the \link{get_normalized_vote_matrix} function with the appropriate parameters.
  #' @param lambda A numeric value between 0 and 1 specifying the weight given to the generality of a genre tag when determining the order of processing. A higher value gives more weight to generality, while a lower value gives more weight to size. The default value is 0.5, which gives equal weight to both generality and size.
  #' @param processing_order An optional vector of genre tag names specifying the order in which to process the tags. If NULL, the function will determine the order based on the sizes of the genre categories. Size is calculated based on the number of tracks associated with each genre tag, where only the tags with highest probability for a track are counted counted as dominant genres.
  #' @param fast Will skip calculation of sizes and metrics for each step, which greatly speeds up the process. Only possible if processing order is provided, else will fall back to FALSE. Default is FALSE.
  #' @param terminal_nodes Optional genre tags that are never processed. Defaults to the nodes without supergenres in the DAG.
  #' @return A list containing:
  #' - mappings: A list of mapping matrices representing the genre category mappings at each step of the folding process. Each mapping matrix has rows corresponding to genre tags and columns corresponding to genre categories, where each entry represents the weight of a genre tag in a given category.
  #' - weights: A list of weight matrices representing the weights of the edges in the DAG at each step of the folding process. Each weight matrix has rows and columns corresponding to genre tags, where each entry represents the weight of the edge from one tag to another.
  #' - sizes: A list of size vectors representing the sizes of the genre categories at each step of the folding process. Each size vector has entries corresponding to genre categories, where each entry represents the number of tracks associated with a given category.
  #' - metrics: A data frame with columns \code{k}, \code{gini}, and \code{onehot_acc}, containing one row per folding state.
  #' - processing_order: A vector of genre tag names representing the order in which the tags were processed during the folding process.
  #' - relevance_order: A vector of genre tag names representing the order of relevance of the tags, with dangling nodes (tags without supergenres) listed first, followed by the processed tags in reverse order of processing.
  #' - final_genres: A list of vectors representing the final genre categories at each step of the folding process. Each vector contains the names of the genre categories that remain after processing a given number of tags.
  #' @details The function processes the genre tags in the specified order (or based on sizes if no order is provided) and iteratively folds the DAG by merging subgenres into their supergenres. The resulting mappings, weights, and sizes are stored at each step of the process, allowing for analysis of the genre category structure at different levels of granularity. Mapping matrices are initialized as identity matrices, and weights are normalized to ensure that all out edges from a node sum to 1. Folding a genre moves its rows in the mapping and weight matrices to its supergenres, weighted by the edges to them. The track category probabilities are updated in the same way, but only for tracks with a share in the folded genre, and their dominant categories determine the sizes. The process continues until all genre tags have been processed, resulting in a final mapping of genre tags to genre categories. If you are interested in a specific set of genre tags, place it last in the processing order, and the function will return the mapping for that set of genre tags. The processing order of other genre tags before that does not matter for the final mapping of the specific set of genre tags, but it may affect the intermediate mappings and sizes of the genre categories.
  #' @seealso \link{get_normalized_vote_matrix} and \link{get_tag_dag_from_normalized_votes} for obtaining the required input parameters.
  tags <- colnames(P)
  if (is.null(terminal_nodes)) {
    terminal_nodes <- get_terminal_nodes(dag$graph)
  }
  fixed_order <- !is.null(processing_order)
  if (fixed_order) {
    processing_order <- match(setdiff(processing_order, terminal_nodes), tags)
  } else {
    fast <- FALSE
  }
  n_folds <- length(tags) - length(terminal_nodes)
  weights <- t(igraph::as_adjacency_matrix(
    dag$graph,
    attr = "weight",
    sparse = FALSE
  ))[tags, tags]
  mapping <- diag(length(tags))
  dimnames(mapping) <- list(tags, tags)
  if (!fast) {
    track_mapping <- as.matrix(P)
    tiebreak_order <- get_tag_name_ranks_for_size_tiebreaking(P)
    baseline_winners <- calculate_winners(track_mapping, tiebreak_order)
    winners <- baseline_winners
  }

  mappings <- weight_states <- final_genres <- vector("list", n_folds + 1L)
  sizes <- vector("list", if (fast) 0L else n_folds + 1L)
  gini <- onehot_acc <- numeric(n_folds + 1L)
  processed_genres <- integer()
  for (step in seq_len(n_folds + 1L)) {
    mappings[[step]] <- mapping
    weight_states[[step]] <- weights
    active_genres <- setdiff(seq_along(tags), processed_genres)
    final_genres[[step]] <- tags[active_genres]
    if (!fast) {
      sizes[[step]] <- stats::setNames(
        as.numeric(tabulate(winners, length(tags))),
        tags
      )
      gini[step] <- DescTools::Gini(sizes[[step]][active_genres])
      onehot_acc[step] <- mean(winners == baseline_winners)
    }
    if (step > n_folds) {
      break
    }

    genre <- if (fixed_order) {
      processing_order[step]
    } else {
      select_next_genre(
        sizes[[step]],
        dag$generality,
        setdiff(active_genres, match(terminal_nodes, tags)),
        lambda
      )
    }
    message(sprintf(
      "Processing genre %d of %d: %s",
      step,
      n_folds,
      tags[genre]
    ))
    parents <- which(weights[, genre] != 0)
    parent_weights <- weights[parents, genre]
    if (!fast) {
      affected <- which(track_mapping[, genre] > 0)
      track_mapping[affected, parents] <-
        track_mapping[affected, parents, drop = FALSE] +
        outer(track_mapping[affected, genre], parent_weights)
      track_mapping[affected, genre] <- 0
      winners[affected] <- calculate_winners(
        track_mapping[affected, , drop = FALSE],
        tiebreak_order
      )
    }
    mapping <- fold_genre_rows(mapping, genre, parents, parent_weights)
    weights <- fold_genre_rows(weights, genre, parents, parent_weights)
    processed_genres <- c(processed_genres, genre)
  }

  list(
    mappings = mappings,
    weights = weight_states,
    sizes = sizes,
    metrics = if (!fast) {
      data.frame(
        k = lengths(final_genres),
        gini = gini,
        onehot_acc = onehot_acc
      )
    },
    processing_order = tags[processed_genres],
    relevance_order = c(sort(terminal_nodes), rev(tags[processed_genres])),
    final_genres = final_genres
  )
}

select_next_genre <- function(sizes, generality, candidates, lambda) {
  size_ranks <- rank(sizes[candidates]) / length(candidates)
  generality_ranks <- rank(generality[names(sizes)[candidates]]) /
    length(candidates)
  candidates[which.min(lambda * generality_ranks + (1 - lambda) * size_ranks)]
}

fold_genre_rows <- function(matrix, genre, parents, parent_weights) {
  #' Move the row of a folded genre to its parent rows, weighted by the edges from the genre to its parents.
  matrix[parents, ] <- matrix[parents, , drop = FALSE] +
    outer(parent_weights, matrix[genre, ])
  matrix[genre, ] <- 0
  matrix
}
