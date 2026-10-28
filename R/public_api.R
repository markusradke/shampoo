# use data.table semantics for data.tables inside the package
.datatable.aware <- TRUE

prepare_long_tag_data <- function(data) {
  required <- c("id", "tag_name")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(sprintf(
      "Input is missing required column(s): %s.",
      paste(missing, collapse = ", ")
    ))
  }
  data <- data.table::as.data.table(
    as.list(data)[intersect(c(required, "tag_count"), names(data))]
  )
  if (is.null(data$tag_count)) {
    data$tag_count <- 1
  } else if (
    !is.numeric(data$tag_count) ||
      any(!is.finite(data$tag_count)) ||
      any(data$tag_count < 0)
  ) {
    stop("tag_count must contain finite, non-negative numeric values.")
  }
  if (
    any(is.na(data$id)) ||
      any(is.na(data$tag_name)) ||
      any(!nzchar(as.character(data$tag_name)))
  ) {
    stop("id and tag_name must not contain missing or empty values.")
  }
  if (anyDuplicated(data, by = required) > 0L) {
    stop("Input contains duplicate id/tag_name combinations.")
  }
  data
}

#' Create a genre DAG from long-format track tags.
#'
#' @param data A data frame with `id`, `tag_name`, and optionally `tag_count`.
#' @return A genre DAG object containing the graph, tag matrix, and metadata.
#' @export
create_genre_dag <- function(data) {
  P <- get_normalized_vote_matrix(prepare_long_tag_data(data))
  dag <- get_tag_dag_from_normalized_votes(P)
  dag$tag_matrix <- as.matrix(P)
  dag$track_ids <- rownames(P)
  dag$tags <- colnames(P)
  dag$terminal_nodes <- get_terminal_nodes(dag$graph)
  dag
}

#' Aggregate a genre DAG into category states.
#'
#' @param dag A result from [create_genre_dag()].
#' @param lambda Weight of tag generality when selecting the processing order.
#' @param processing_order Optional explicit tag processing order.
#' @param fast Whether to skip state metrics when an explicit order is supplied.
#' @param target_set Optional character vector of final genre tags. All genres
#' outside this set are processed, and target tags are treated as terminal.
#' @return Category state history, including the reusable normalized tag matrix.
#' @export
aggregate_genre_categories <- function(
  dag,
  lambda = 0.5,
  processing_order = NULL,
  fast = FALSE,
  target_set = NULL
) {
  if (is.null(dag$tag_matrix) || is.null(dag$graph)) {
    stop("dag must be returned by create_genre_dag().")
  }
  terminal_nodes <- dag$terminal_nodes
  if (is.null(terminal_nodes)) {
    terminal_nodes <- get_terminal_nodes(dag$graph)
  }
  if (!is.null(target_set)) {
    if (
      !is.character(target_set) ||
        length(target_set) == 0L ||
        anyNA(target_set) ||
        any(!nzchar(target_set))
    ) {
      stop("target_set must be a non-empty character vector of genre tags.")
    }
    target_set <- unique(target_set)
    missing_genres <- setdiff(target_set, dag$tags)
    if (length(missing_genres) > 0L) {
      stop(sprintf(
        "target_set contains genre tag(s) not present in the DAG: %s.",
        paste(missing_genres, collapse = ", ")
      ))
    }
    missing_terminals <- setdiff(terminal_nodes, target_set)
    if (length(missing_terminals) > 0L) {
      warning(
        sprintf(
          "Adding terminal genre tag(s) to target_set: %s.",
          paste(missing_terminals, collapse = ", ")
        ),
        call. = FALSE
      )
      target_set <- c(target_set, missing_terminals)
    }
    if (!is.null(processing_order)) {
      stop("Specify either target_set or processing_order, not both.")
    }
    processing_order <- setdiff(dag$tags, target_set)
    terminal_nodes <- target_set
  }
  result <- get_genre_categories_from_graph(
    dag,
    dag$tag_matrix,
    lambda = lambda,
    processing_order = processing_order,
    fast = fast,
    terminal_nodes = terminal_nodes
  )
  result$tags <- dag$tags
  result$track_ids <- dag$track_ids
  result$tag_matrix <- dag$tag_matrix
  result$terminal_nodes <- terminal_nodes
  result$target_set <- target_set
  result
}

#' Assign hard and/or fuzzy categories to long-format track tags.
#'
#' @param data A data frame with `id`, `tag_name`, and optionally `tag_count`.
#' @param categories A result from [aggregate_genre_categories()].
#' @param k One or more numbers of categories to use. For categories created
#' with `target_set`, this argument is ignored and the target-set size is used.
#' A warning is printed when `k` is supplied in that case. Otherwise, values
#' below the number of terminal DAG nodes are clipped to that minimum.
#' Multiple values are made unique after clipping and return a named list of
#' assignment data frames.
#' @param method One of `"both"`, `"hard"`, or `"fuzzy"`.
#' @return One row per track containing category probabilities, assignments,
#' and `assignment_status`, which is one of `"assigned"`,
#' `"partial_unknown_tags"`, or `"no_recognized_tags"`. Row names are not used
#' as identifiers. When multiple `k` values are supplied, a named list of such
#' data frames is returned.
#' @export
assign_genre_categories <- function(
  data,
  categories,
  k = NULL,
  method = c("both", "hard", "fuzzy"),
  assignment_status = FALSE
) {
  method <- match.arg(method)
  data <- prepare_long_tag_data(data)
  k <- validate_assignment_inputs(categories, k, k_supplied = !missing(k))
  known_tags <- get_category_tags(categories)
  track_info <- prepare_assignment_track_info(data, known_tags)
  P <- get_normalized_vote_matrix(track_info$data, known_tags)
  tiebreak_order <- get_tag_name_ranks_for_size_tiebreaking(P)

  assignments <- lapply(k, function(chosen_k) {
    probabilities <- get_track_category_probabilities(
      P,
      categories,
      chosen_k,
      tiebreak_order,
      include_probabilities = method != "hard"
    )
    probabilities <- complete_assignment_rows(probabilities, track_info$ids)
    if (assignment_status) {
      probabilities$assignment_status <- get_assignment_status(
        probabilities$id,
        track_info
      )
    }
    format_assignment_output(probabilities, method)
  })
  names(assignments) <- as.character(k)
  if (length(assignments) == 1L) assignments[[1L]] else assignments
}

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
