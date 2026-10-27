prepare_long_tag_data <- function(data) {
  required <- c("id", "tag_name")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(sprintf(
      "Input is missing required column(s): %s.",
      paste(missing, collapse = ", ")
    ))
  }
  if ("tag_count" %in% names(data)) {
    if (!is.numeric(data$tag_count) ||
        any(!is.finite(data$tag_count)) ||
        any(data$tag_count < 0)) {
      stop("tag_count must contain finite, non-negative numeric values.")
    }
  } else {
    data$tag_count <- 1
  }
  if (any(is.na(data$id)) || any(is.na(data$tag_name)) ||
      any(!nzchar(as.character(data$tag_name)))) {
    stop("id and tag_name must not contain missing or empty values.")
  }
  duplicated_rows <- duplicated(data[c("id", "tag_name")])
  if (any(duplicated_rows)) {
    stop("Input contains duplicate id/tag_name combinations.")
  }
  data.table::as.data.table(data)
}

#' Create a genre DAG from long-format track tags.
#'
#' @param data A data frame with `id`, `tag_name`, and optionally `tag_count`.
#' @return A genre DAG object containing the graph, tag matrix, and metadata.
#' @export
create_genre_dag <- function(data) {
  data <- prepare_long_tag_data(data)
  P <- get_normalized_wide_vote_matrix(data)
  dag <- get_tag_dag_from_normalized_votes(P)
  dag$tag_matrix <- P
  dag$track_ids <- rownames(P)
  dag$tags <- colnames(P)
  dag
}

#' Aggregate a genre DAG into category states.
#'
#' @param dag A result from [create_genre_dag()].
#' @param lambda Weight of tag generality when selecting the processing order.
#' @param processing_order Optional explicit tag processing order.
#' @param fast Whether to skip state metrics when an explicit order is supplied.
#' @return Category state history.
#' @export
aggregate_genre_categories <- function(
  dag,
  lambda = 0.5,
  processing_order = NULL,
  fast = FALSE
) {
  if (is.null(dag$tag_matrix) || is.null(dag$graph)) {
    stop("dag must be returned by create_genre_dag().")
  }
  result <- get_genre_categories_from_graph(
    dag,
    dag$tag_matrix,
    lambda = lambda,
    processing_order = processing_order,
    fast = fast
  )
  result$tags <- dag$tags
  result$track_ids <- dag$track_ids
  result
}

#' Assign hard and/or fuzzy categories to long-format track tags.
#'
#' @param data A data frame with `id`, `tag_name`, and optionally `tag_count`.
#' @param categories A result from [aggregate_genre_categories()].
#' @param k Number of categories to use.
#' @param method One of `"both"`, `"hard"`, or `"fuzzy"`.
#' @return One row per track containing category probabilities and assignments.
#' @export
assign_genre_categories <- function(
  data,
  categories,
  k,
  method = c("both", "hard", "fuzzy")
) {
  method <- match.arg(method)
  data <- prepare_long_tag_data(data)
  if (is.null(categories$mappings) || is.null(categories$final_genres)) {
    stop("categories must be returned by aggregate_genre_categories().")
  }
  all_ids <- unique(as.character(data$id))
  if (!is.numeric(k) || length(k) != 1L || k != as.integer(k) || k < 1L ||
      k > nrow(categories$mappings[[1]])) {
    stop("k must be an available positive integer category count.")
  }

  known_tags <- categories$tags
  unknown_tags <- setdiff(unique(as.character(data$tag_name)), known_tags)
  if (length(unknown_tags) > 0L) {
    message_text <- sprintf(
      "Ignoring %d unknown tag(s): %s",
      length(unknown_tags),
      if (length(unknown_tags) < 50L) {
        paste(unknown_tags, collapse = ", ")
      } else {
        "list suppressed because there are 50 or more"
      }
    )
    warning(message_text, call. = FALSE)
    data <- data[data$tag_name %in% known_tags, , drop = FALSE]
  }
  if (nrow(data) == 0L) {
    stop("No recognized tags remain after removing unknown tags.")
  }

  P <- get_normalized_wide_vote_matrix(data)
  P_aligned <- matrix(
    0,
    nrow = nrow(P),
    ncol = length(known_tags),
    dimnames = list(rownames(P), known_tags)
  )
  shared_tags <- intersect(colnames(P), known_tags)
  P_aligned[, match(shared_tags, known_tags)] <-
    P[, match(shared_tags, colnames(P)), drop = FALSE]
  probabilities <- get_track_category_probabilities(
    P_aligned,
    categories,
    chosen_k = k
  )
  probabilities$id <- rownames(P_aligned)
  missing_ids <- setdiff(all_ids, probabilities$id)
  if (length(missing_ids) > 0L) {
    missing_rows <- as.data.frame(
      lapply(probabilities, function(column) {
        if (is.numeric(column)) {
          rep(NA_real_, length(missing_ids))
        } else {
          rep(NA_character_, length(missing_ids))
        }
      }),
      stringsAsFactors = FALSE
    )
    missing_rows$id <- missing_ids
    probabilities <- rbind(probabilities, missing_rows)
  }
  probabilities <- probabilities[match(all_ids, probabilities$id), , drop = FALSE]
  probabilities <- probabilities[, c(
    "id",
    setdiff(names(probabilities), "id")
  )]
  if (method == "hard") {
    return(probabilities[c("id", "cat", "cat_prob")])
  }
  if (method == "fuzzy") {
    return(probabilities[c(
      "id",
      setdiff(names(probabilities), c("id", "cat", "cat_prob"))
    )])
  }
  probabilities
}
