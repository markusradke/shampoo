get_normalized_vote_matrix <- function(
  long_dt,
  tags = sort_unique_in_c_locale(long_dt$tag_name)
) {
  #' Get normalized sparse vote matrix from long genre tag data.table representing the distribution of votes for each genre tag across tracks.
  #'
  #' @param long_dt A long data.table containing columns: id, tag_name, tag_count
  #' @param tags The genre tags used as columns, in this order. Defaults to all tags in `long_dt`, sorted in C locale. All tags in `long_dt` must be contained.
  #' @return A sparse normalized vote matrix where rows correspond to tracks (sorted by id) and columns correspond to genre tags. Each entry represents the normalized vote count for a tag for a given track. Rows sum to 1, representing the distribution of votes across tags for each track.
  #' @details The function takes a long data.table containing track-tag pairs and their corresponding vote counts, and converts it into a sparse matrix where each row represents a track and each column represents a genre tag. If you have no detailed tag counts per track, we suggest you set these to one for all rows. The vote counts are then normalized for each track, so that the sum of votes across all tags for a given track equals 1. This normalized vote matrix can be used for further analysis, such as building a directed acyclic graph (DAG) of genre tags based on their co-occurrence and generality.
  #' @seealso \link{get_tag_dag_from_normalized_votes} for obtaining the directed acyclic graph of genre tags based on the normalized vote matrix.
  track_ids <- sort_unique_in_c_locale(long_dt$id)
  rows <- match(long_dt$id, track_ids)
  track_totals <- rowsum(long_dt$tag_count, rows)[, 1]
  if (any(track_totals == 0)) {
    stop(
      "Some tracks have zero total tag counts. Please check your input data."
    )
  }
  Matrix::sparseMatrix(
    i = rows,
    j = match(long_dt$tag_name, tags),
    x = long_dt$tag_count / track_totals[rows],
    dims = c(length(track_ids), length(tags)),
    dimnames = list(as.character(track_ids), as.character(tags))
  )
}

sort_unique_in_c_locale <- function(values) {
  #' Sort unique values like data.table does, i.e. strings by their UTF-8 bytes.
  values <- unique(values)
  if (is.character(values)) {
    values <- enc2utf8(values)
  }
  sort(values, method = "radix")
}


get_tag_dag_from_normalized_votes <- function(P) {
  #' Get directed acyclic graph (DAG) of genre tags based on normalized vote matrix representing a hierarchical mapping of genre tags based on their co-occurrence and generality.
  #' @param P A normalized vote matrix (dense or sparse) where rows correspond to tracks and columns correspond to genre tags. Each entry represents the normalized vote count for a tag for a given track. To obtain this matrix, use the \link{get_normalized_vote_matrix} function with the appropriate parameters.
  #' @return A list containing:
  #' - graph: An igraph object representing the directed acyclic graph of genre tags.
  #' - generality: A named vector of generality scores for each genre tag, where higher values indicate more general tags. Generality is calculated based on the weighted number of indegrees in the DAG, reflecting how many other tags act as subgenres for a given tag.
  #' @details The function takes a normalized vote matrix and calculates the joint and conditional probabilities of genre tags based on their co-occurrence across tracks. It then constructs an initital graph where edges represent the conditional probabilities of one tag given another, with weights corresponding to these probabilities. The generality of each tag is calculated based on the weighted number of indegrees in this initial graph, providing a measure of how many other tags act as subgenres for a given tag. Then, a directed acyclig graph (DAG) is contructed by allowing only connections form any given tag to tags with higher generality. The resulting DAG and generality scores can be used for further analysis, such as extracting genre categories and mapping tracks to these categories.
  #'@seealso \link{get_normalized_vote_matrix} for obtaining the required input parameter.
  message("Calculating probabilities and building trees...")
  N <- nrow(P)
  G <- ncol(P)

  # Get marginals, joint and conditional probabilities
  P_g <- Matrix::colMeans(P)
  eps <- 1e-12
  P_g_smooth <- pmax(P_g, eps) # prevent divide by zero
  joint_raw <- as.matrix(Matrix::crossprod(P)) / N # P(X, Y)
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
  message(sprintf(
    "Nodes without any supergenres: \n%s",
    toString(get_terminal_nodes(g))
  ))
  return(list(graph = g, generality = indegree_weights))
}

get_terminal_nodes <- function(graph) {
  igraph::V(graph)[igraph::degree(graph, mode = "out") == 0]$name
}
