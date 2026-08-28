make_long_genre_data <- function() {
  data.table::data.table(
    track.s.id = c(
      1,
      1,
      2,
      2,
      3,
      3,
      4,
      4,
      5,
      5,
      6,
      6
    ),
    tag_name = c(
      "rock",
      "pop",
      "electronic",
      "pop",
      "classical",
      "pop",
      "rock",
      "pop",
      "electronic",
      "pop",
      "classical",
      "pop"
    ),
    tag_count = c(3, 1, 2, 1, 4, 1, 1, 1, 1, 2, 1, 1)
  )
}

make_normalized_votes <- function() {
  shampoo::get_normalized_wide_vote_matrix(make_long_genre_data())
}

make_hierarchy <- function() {
  graph <- igraph::graph_from_data_frame(
    data.frame(
      from = c("rock", "electronic", "classical"),
      to = c("pop", "pop", "pop"),
      weight = c(0.8, 0.6, 0.4)
    ),
    directed = TRUE,
    vertices = data.frame(name = c("rock", "electronic", "classical", "pop"))
  )
  list(
    graph = graph,
    generality = c(rock = 0.8, electronic = 0.6, classical = 0.4, pop = 0)
  )
}

test_that("normalized wide vote matrix has one row per track, sums to one, and provides correct values", {
  normalized_votes <- make_normalized_votes()

  expect_equal(dim(normalized_votes), c(6L, 4L))
  expect_equal(
    colnames(normalized_votes),
    c("classical", "electronic", "pop", "rock")
  )
  expect_equal(rowSums(normalized_votes), rep(1, 6))
  expect_equal(
    normalized_votes[1, ],
    c(classical = 0, electronic = 0, pop = 0.25, rock = 0.75)
  )
  expect_equal(
    normalized_votes[2, ],
    c(classical = 0, electronic = 2 / 3, pop = 1 / 3, rock = 0)
  )
})

test_that("DAG construction keeps all tags and only creates acyclic positive-weight edges", {
  normalized_votes <- make_normalized_votes()
  dag <- shampoo::get_tag_dag_from_normalized_votes(normalized_votes)
  graph <- dag$graph

  expect_s3_class(graph, "igraph")
  expect_setequal(igraph::V(graph)$name, colnames(normalized_votes))
  expect_true(igraph::is_dag(graph))
  expect_named(dag$generality, colnames(normalized_votes))
  expect_true(all(dag$generality >= 0))

  edge_table <- igraph::as_data_frame(graph, what = "edges")
  expect_true(all(edge_table$weight > 0))
  expect_true(all(
    dag$generality[edge_table$from] < dag$generality[edge_table$to] |
      dag$generality[edge_table$from] == dag$generality[edge_table$to]
  ))
})

test_that("category extraction returns a complete fixed-order state history", {
  normalized_votes <- make_normalized_votes()
  hierarchy <- make_hierarchy()
  extraction <- shampoo::get_genre_categories_from_graph(
    hierarchy,
    normalized_votes,
    processing_order = c("rock", "electronic", "classical", "pop")
  )

  expect_equal(
    extraction$processing_order,
    c("rock", "electronic", "classical")
  )
  expect_equal(
    extraction$relevance_order,
    c("pop", "classical", "electronic", "rock")
  )
  expect_length(extraction$mappings, 4)
  expect_length(extraction$weights, 4)
  expect_length(extraction$sizes, 4)
  expect_length(extraction$ginis, 4)
  expect_length(extraction$final_genres, 4)
  expect_equal(extraction$final_genres[[1]], colnames(normalized_votes))
  expect_equal(extraction$final_genres[[4]], "pop")
  expect_equal(
    extraction$sizes[[1]],
    c(classical = 2, electronic = 1, pop = 1, rock = 2)
  )
  expect_equal(
    extraction$sizes[[4]],
    c(classical = 0, electronic = 0, pop = 6, rock = 0)
  )
  expect_equal(
    colSums(extraction$mappings[[4]]),
    setNames(rep(1, 4), colnames(normalized_votes))
  )
  expect_equal(
    extraction$mappings[[4]][3, ],
    setNames(rep(1, 4), colnames(normalized_votes))
  )
  expect_true(all(is.finite(extraction$ginis[1:3])))
  expect_true(is.nan(extraction$ginis[4]))
})

test_that("automatic category extraction and fast fixed-order extraction are reproducible", {
  normalized_votes <- make_normalized_votes()
  hierarchy <- make_hierarchy()
  fixed_order <- c("rock", "electronic", "classical")

  automatic <- shampoo::get_genre_categories_from_graph(
    hierarchy,
    normalized_votes,
    lambda = 0.25
  )
  fast <- shampoo::get_genre_categories_from_graph(
    hierarchy,
    normalized_votes,
    processing_order = fixed_order,
    fast = TRUE
  )
  fast_again <- shampoo::get_genre_categories_from_graph(
    hierarchy,
    normalized_votes,
    processing_order = fixed_order,
    fast = TRUE
  )

  expect_length(automatic$processing_order, 3)
  expect_length(unique(automatic$processing_order), 2)
  expect_true(all(automatic$processing_order %in% colnames(normalized_votes)))
  expect_equal(fast$processing_order, fixed_order)
  expect_length(fast$sizes, 0)
  expect_null(fast$ginis)
  expect_equal(fast$final_genres, fast_again$final_genres)
  expect_equal(fast$mappings, fast_again$mappings)
})

test_that("category mappings preserve track probability mass", {
  normalized_votes <- make_normalized_votes()
  extraction <- shampoo::get_genre_categories_from_graph(
    make_hierarchy(),
    normalized_votes,
    processing_order = c("rock", "electronic", "classical")
  )

  for (mapping in extraction$mappings) {
    track_categories <- normalized_votes %*% t(mapping)
    expect_equal(rowSums(track_categories), rowSums(normalized_votes))
    expect_true(all(track_categories >= -1e-12))
  }
})

test_that("track category probabilities reports categories and deterministic winners", {
  normalized_votes <- make_normalized_votes()
  extraction <- shampoo::get_genre_categories_from_graph(
    make_hierarchy(),
    normalized_votes,
    processing_order = c("rock", "electronic", "classical")
  )

  two_categories <- shampoo::get_track_category_probabilities(
    normalized_votes,
    extraction,
    chosen_k = 2
  )
  one_category <- shampoo::get_track_category_probabilities(
    normalized_votes,
    extraction,
    chosen_k = 1
  )

  expect_named(two_categories, c("classical", "pop", "cat", "cat_prob"))
  expect_equal(nrow(two_categories), 6)
  expect_equal(
    two_categories$cat,
    c("pop", "pop", "classical", "pop", "pop", "classical")
  )
  expect_equal(two_categories$cat_prob, c(1, 1, 0.8, 1, 1, 0.5))
  expect_equal(rowSums(two_categories[c("classical", "pop")]), rep(1, 6))
  expect_equal(one_category$cat, rep("pop", 6))
  expect_equal(one_category$cat_prob, rep(1, 6))
  expect_equal(one_category$pop, rep(1, 6))
})

test_that("track category probabilities resolves ties using tag prevalence", {
  normalized_votes <- matrix(
    c(0.5, 0.5, 1, 0, 1, 0, 0, 1),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(NULL, c("common", "rare"))
  )
  extraction <- list(
    mappings = list(
      matrix(
        diag(2),
        nrow = 2,
        dimnames = list(c("common", "rare"), c("common", "rare"))
      ),
      matrix(
        diag(2),
        nrow = 2,
        dimnames = list(c("common", "rare"), c("common", "rare"))
      )
    ),
    final_genres = list(c("common", "rare"), c("common", "rare")),
    relevance_order = c("common", "rare")
  )

  result <- shampoo::get_track_category_probabilities(
    normalized_votes,
    extraction,
    2
  )

  expect_equal(result$cat, c("rare", "common", "common", "rare"))
  expect_equal(result$cat_prob, c(0.5, 1, 1, 1))
})

test_that("long genre data receives the track-level mapping", {
  long_data <- make_long_genre_data()
  track_map <- data.frame(
    cat = rep(c("rock", "electronic", "classical"), each = 2),
    cat_prob = rep(c(0.75, 2 / 3, 0.8), each = 2),
    stringsAsFactors = FALSE
  )
  track_map$rock <- track_map$cat == "rock"

  mapped <- shampoo::add_track_map_to_long(long_data, track_map)

  expect_equal(nrow(mapped), nrow(long_data))
  expect_true(all(c("cat", "cat_prob", "rock") %in% names(mapped)))
  expect_equal(
    mapped$cat_prob[match(1:6, mapped$track.s.id)],
    c(0.75, 0.75, 2 / 3, 2 / 3, 0.8, 0.8)
  )
  expect_equal(mapped$tag_count, long_data$tag_count)
})

test_that("size tie-breaking ranks less prevalent tags first", {
  normalized_votes <- make_normalized_votes()

  ranks <- shampoo:::get_tag_name_ranks_for_size_tiebreaking(normalized_votes)

  expect_equal(names(ranks), c("classical", "electronic", "rock", "pop"))
  expect_equal(unname(ranks), c(2, 2, 2, 4))
})

test_that("size calculation counts dominant tags and includes zero-sized tags", {
  track_mapping <- matrix(
    c(0.8, 0.2, 0, 0.5, 0.5, 0, 0.1, 0.9, 0, 0.1, 0.9, 0),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(NULL, c("alpha", "beta", "gamma"))
  )
  tiebreak_order <- c(beta = 1, alpha = 2, gamma = 3)

  sizes <- shampoo:::calculate_sizes_from_track_mapping(
    track_mapping,
    tiebreak_order
  )

  expect_equal(sizes, c(alpha = 1, beta = 3, gamma = 0))
})
