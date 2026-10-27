test_that("public API uses id and defaults tag_count", {
  input <- data.frame(
    id = c("track-a", "track-a", "track-b"),
    tag_name = c("rock", "pop", "rock")
  )

  dag <- shampoo::create_genre_dag(input)

  expect_equal(dag$track_ids, c("track-a", "track-b"))
  expect_equal(colnames(dag$tag_matrix), c("pop", "rock"))
  expect_equal(unname(rowSums(dag$tag_matrix)), c(1, 1))
  expect_equal(getNamespaceExports("shampoo"), c(
    "assign_genre_categories",
    "aggregate_genre_categories",
    "create_genre_dag"
  ))
})

test_that("public API rejects duplicate id/tag rows", {
  input <- data.frame(
    id = c("track-a", "track-a"),
    tag_name = c("rock", "rock"),
    tag_count = c(1, 2)
  )

  expect_error(
    shampoo::create_genre_dag(input),
    "duplicate id/tag_name"
  )
})

test_that("assignment returns one row per recognized track in each mode", {
  input <- data.frame(
    id = c("track-a", "track-a", "track-b", "track-b"),
    tag_name = c("rock", "pop", "rock", "pop"),
    tag_count = c(3, 1, 1, 3)
  )
  categories <- shampoo::aggregate_genre_categories(
    shampoo::create_genre_dag(input)
  )

  both <- shampoo::assign_genre_categories(input, categories, k = 2)
  hard <- shampoo::assign_genre_categories(
    input,
    categories,
    k = 2,
    method = "hard"
  )
  fuzzy <- shampoo::assign_genre_categories(
    input,
    categories,
    k = 2,
    method = "fuzzy"
  )

  expect_equal(nrow(both), 2)
  expect_equal(nrow(hard), 2)
  expect_equal(nrow(fuzzy), 2)
  expect_true(all(c("id", "cat", "cat_prob") %in% names(both)))
  expect_equal(names(hard), c("id", "cat", "cat_prob"))
  expect_false(any(c("cat", "cat_prob") %in% names(fuzzy)))
})

test_that("unknown tags are reported and ignored", {
  input <- data.frame(
    id = c("track-a", "track-a"),
    tag_name = c("rock", "unseen")
  )
  training <- data.frame(
    id = c("track-a", "track-a", "track-b", "track-b"),
    tag_name = c("rock", "pop", "rock", "pop")
  )
  categories <- shampoo::aggregate_genre_categories(
    shampoo::create_genre_dag(training)
  )

  expect_warning(
    result <- shampoo::assign_genre_categories(input, categories, k = 2),
    "Ignoring 1 unknown tag"
  )
  expect_equal(result$id, "track-a")
})
