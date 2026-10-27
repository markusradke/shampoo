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
    if (
      !is.numeric(data$tag_count) ||
        any(!is.finite(data$tag_count)) ||
        any(data$tag_count < 0)
    ) {
      stop("tag_count must contain finite, non-negative numeric values.")
    }
  } else {
    data$tag_count <- 1
  }
  if (
    any(is.na(data$id)) ||
      any(is.na(data$tag_name)) ||
      any(!nzchar(as.character(data$tag_name)))
  ) {
    stop("id and tag_name must not contain missing or empty values.")
  }
  if (any(duplicated(data[c("id", "tag_name")]))) {
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
#' @return Category state history, including the reusable normalized tag matrix.
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
  result$tag_matrix <- dag$tag_matrix
  result
}

#' Assign hard and/or fuzzy categories to long-format track tags.
#'
#' @param data A data frame with `id`, `tag_name`, and optionally `tag_count`.
#' @param categories A result from [aggregate_genre_categories()].
#' @param k One or more numbers of categories to use. Multiple values return a
#' named list of assignment data frames.
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
  k,
  method = c("both", "hard", "fuzzy"),
  assignment_status = FALSE
) {
  method <- match.arg(method)
  data <- prepare_long_tag_data(data)
  validate_assignment_inputs(categories, k)
  known_tags <- get_category_tags(categories)
  track_info <- prepare_assignment_track_info(data, known_tags)
  P_aligned <- prepare_assignment_matrix(track_info$data, known_tags)
  tiebreak_order <- get_assignment_tiebreak_order(P_aligned, known_tags)

  assignments <- lapply(k, function(chosen_k) {
    probabilities <- calculate_assignment_probabilities(
      P_aligned,
      categories,
      chosen_k,
      tiebreak_order,
      include_probabilities = method != "hard"
    )
    probabilities <- complete_assignment_rows(
      probabilities,
      track_info$ids
    )
    if (assignment_status) {
      probabilities$assignment_status <- get_assignment_status(
        probabilities$id,
        track_info
      )
    }
    format_assignment_output(probabilities, method, assignment_status)
  })
  names(assignments) <- as.character(k)
  assignments <- lapply(assignments, function(result) {
    rownames(result) <- NULL
    result
  })
  if (length(assignments) == 1L) assignments[[1L]] else assignments
}

validate_assignment_inputs <- function(categories, k) {
  if (is.null(categories$mappings) || is.null(categories$final_genres)) {
    stop("categories must be returned by aggregate_genre_categories().")
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
}

get_category_tags <- function(categories) {
  tags <- categories$tags
  if (is.null(tags)) {
    tags <- colnames(categories$mappings[[1]])
  }
  tags
}

prepare_assignment_track_info <- function(data, known_tags) {
  ids <- unique(as.character(data$id))
  input_tags <- as.character(data$tag_name)
  unknown_tags <- setdiff(unique(input_tags), known_tags)
  unknown_by_id <- tapply(
    input_tags %in% unknown_tags,
    as.character(data$id),
    any
  )

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

  recognized_data <- data[data$tag_name %in% known_tags, , drop = FALSE]
  list(
    data = recognized_data,
    ids = ids,
    recognized_ids = unique(as.character(recognized_data$id)),
    unknown_by_id = unknown_by_id
  )
}

prepare_assignment_matrix <- function(data, known_tags) {
  P <- if (nrow(data) > 0L) {
    Matrix::Matrix(get_normalized_wide_vote_matrix(data), sparse = TRUE)
  } else {
    Matrix::sparseMatrix(
      i = integer(),
      j = integer(),
      dims = c(0L, length(known_tags)),
      dimnames = list(character(), known_tags)
    )
  }
  aligned <- Matrix::sparseMatrix(
    i = integer(),
    j = integer(),
    x = numeric(),
    dims = c(nrow(P), length(known_tags)),
    dimnames = list(rownames(P), known_tags)
  )
  if (nrow(P) > 0L) {
    shared_tags <- intersect(colnames(P), known_tags)
    source_columns <- match(shared_tags, colnames(P))
    target_columns <- match(shared_tags, known_tags)
    entries <- Matrix::summary(P[, source_columns, drop = FALSE])
    aligned <- Matrix::sparseMatrix(
      i = entries$i,
      j = target_columns[entries$j],
      x = entries$x,
      dims = c(nrow(P), length(known_tags)),
      dimnames = list(rownames(P), known_tags)
    )
  }
  aligned
}

get_assignment_tiebreak_order <- function(P, known_tags) {
  if (nrow(P) > 0L) {
    return(get_tag_name_ranks_for_size_tiebreaking(P))
  }
  stats::setNames(seq_along(known_tags), known_tags)
}

calculate_assignment_probabilities <- function(
  P,
  categories,
  chosen_k,
  tiebreak_order,
  include_probabilities = TRUE
) {
  if (nrow(P) > 0L) {
    result <- get_track_category_probabilities(
      P,
      categories,
      chosen_k = chosen_k,
      tiebreak_order = tiebreak_order,
      include_probabilities = include_probabilities
    )
    rownames(result) <- rownames(P)
    return(result)
  }
  final_genres <- categories$final_genres[[
    nrow(categories$mappings[[1]]) - chosen_k + 1L
  ]]
  empty <- as.data.frame(setNames(
    replicate(length(final_genres), numeric(), simplify = FALSE),
    final_genres
  ))
  empty$cat <- character()
  empty$cat_prob <- numeric()
  empty
}

complete_assignment_rows <- function(probabilities, ids) {
  probabilities$id <- rownames(probabilities)
  missing_ids <- setdiff(ids, probabilities$id)
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
  probabilities <- probabilities[match(ids, probabilities$id), , drop = FALSE]
  probabilities[, c("id", setdiff(names(probabilities), "id"))]
}

get_assignment_status <- function(ids, track_info) {
  recognized_lookup <- stats::setNames(
    rep(TRUE, length(track_info$recognized_ids)),
    track_info$recognized_ids
  )
  vapply(
    ids,
    function(track_id) {
      has_unknown <- isTRUE(track_info$unknown_by_id[[track_id]])
      has_recognized <- isTRUE(recognized_lookup[track_id])
      if (!has_recognized) {
        "no_recognized_tags"
      } else if (has_unknown) {
        "partial_unknown_tags"
      } else {
        "assigned"
      }
    },
    character(1)
  )
}

format_assignment_output <- function(probabilities, method, assignment_status) {
  if (method == "hard") {
    if (assignment_status) {
      return(probabilities[c("id", "cat", "cat_prob", "assignment_status")])
    } else {
      return(probabilities[c("id", "cat", "cat_prob")])
    }
  }
  if (method == "fuzzy") {
    if (assignment_status) {
      return(probabilities[c(
        "id",
        setdiff(
          names(probabilities),
          c("cat", "cat_prob", "assignment_status")
        ),
        "assignment_status"
      )])
    } else {
      return(probabilities[c(
        "id",
        setdiff(names(probabilities), c("cat", "cat_prob", "assignment_status"))
      )])
    }
  }
  probabilities
}
