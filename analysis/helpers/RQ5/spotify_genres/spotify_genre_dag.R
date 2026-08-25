# BUILD A GENREMAP FOR SPOTIFY BASED ON ARTIST DATA
rm(list = ls())
gc()
library(data.table)
library(dplyr)
devtools::load_all()

LAMBDA <- 0.5
N_MIN_ARTISTS <- 10

# LOAD DTA ----
fold1_ids <- readr::read_csv("data/splits/dag_fold1.csv") |>
  distinct(track.s.id) |>
  pull(track.s.id)
fold2_ids <- readr::read_csv("data/splits/dag_fold2.csv") |>
  distinct(track.s.id) |>
  pull(track.s.id)
train_ids <- readr::read_csv("data/splits/train.csv") |>
  distinct(track.s.id) |>
  pull(track.s.id)
holdout_ids <- readr::read_csv("data/splits/holdout.csv") |>
  distinct(track.s.id) |>
  pull(track.s.id)
s_long <- fread("data/long_data/s_long.csv")

# PREPARE VOTING MATRIX FROM SPOTIFY ARTIST GENRES ----
s_tags <- get_common_tags(
  s_long,
  fold1_ids,
  fold2_ids,
  min_artists = N_MIN_ARTISTS
)
s_train <- s_long |>
  filter(track.s.id %in% train_ids) |>
  filter(tag_name %in% s_tags)
# give me the columns of P_s_train where the entries are only ones and zeros, i.e. no fractional values
P_s_train <- get_normalized_wide_vote_matrix(s_train)
binary_columns <- colnames(P_s_train)[apply(P_s_train, 2, function(x) {
  all(x %in% c(0, 1))
})]
# remove those columns from the training data: These genres do not cooccur with any other genres, so they are not useful for building a DAG
s_train <- s_train |>
  filter(!tag_name %in% binary_columns)
P_s_train <- get_normalized_wide_vote_matrix(s_train)


# GET DAG AND CATEOGORY MAPPING STATES ----
dag_s_train <- get_tag_dag_from_normalized_votes(P_s_train)

cat_states_s <- get_genre_categories_from_graph(
  dag_s_train,
  P_s_train,
  lambda = LAMBDA,
)
saveRDS(cat_states_s, "models/dag/cat_states_spotify.rds")

# CHOOSE NUMBER OF GENRE CATEGORIES ----
cat_states_s <- readRDS("models/dag/cat_states_spotify.rds")
plot_gini_genre_categories(cat_states_s, 100)
chosen_k <- 28 # local minimum of gini index < 30


# SAVE MAPPED GENRES FOR TRAIN AND HOLDOUT SETS ----
s_holdout <- s_long |>
  filter(track.s.id %in% holdout_ids) |>
  filter(tag_name %in% s_tags) |>
  filter(!tag_name %in% binary_columns)
P_s_holdout <- get_normalized_wide_vote_matrix(s_holdout)

# update manually
setdiff(colnames(P_s_train), colnames(P_s_holdout))
P_s_holdout <- cbind(
  P_s_holdout,
  # needs to be updated manually because the holdout set does not have any tracks with this genre
  "lo-fi beats" = rep(0, nrow(P_s_holdout))
)
P_s_holdout <- P_s_holdout[, colnames(P_s_train)]


train_mapping <- get_track_category_probabilities(
  P_s_train,
  cat_states_s,
  chosen_k
)
holdout_mapping <- get_track_category_probabilities(
  P_s_holdout,
  cat_states_s,
  chosen_k
)

wide_train <- dcast(
  s_train,
  track.s.id ~ tag_name,
  value.var = "tag_count",
  fill = 0
)
wide_holdout <- dcast(
  s_holdout,
  track.s.id ~ tag_name,
  value.var = "tag_count",
  fill = 0
)

s_mapping_train <- cbind(track.s.id = wide_train$track.s.id, train_mapping) |>
  left_join(s_train, train_mapping, by = "track.s.id") |>
  select(
    -all_of(c(
      "track.s.firstartist.id",
      "tag_name",
      "tag_count",
      "cat",
      "cat_prob"
    ))
  ) |>
  select(track.s.id, everything()) |>
  distinct(track.s.id, .keep_all = T)
mapping_ref_class <- s_mapping_train |>
  select(-track.s.id) |>
  colSums() |>
  which.max() |>
  names()
s_mapping_train <- s_mapping_train[,
  -which(colnames(s_mapping_train) == mapping_ref_class)
]
colnames(s_mapping_train)[-1] <- paste0("sp_", colnames(s_mapping_train)[-1])
colnames(s_mapping_train) <- stringr::str_replace_all(
  colnames(s_mapping_train),
  " ",
  "_"
)

s_mapping_holdout <- cbind(
  track.s.id = wide_holdout$track.s.id,
  holdout_mapping
) |>
  left_join(s_train, holdout_mapping, by = "track.s.id") |>
  select(
    -all_of(c(
      "track.s.firstartist.id",
      "tag_name",
      "tag_count",
      "cat",
      "cat_prob"
    ))
  ) |>
  select(track.s.id, everything()) |>
  distinct(track.s.id, .keep_all = T)
s_mapping_holdout <- s_mapping_holdout[,
  -which(colnames(s_mapping_holdout) == mapping_ref_class)
]
colnames(s_mapping_holdout)[-1] <- paste0(
  "sp_",
  colnames(s_mapping_holdout)[-1]
)
colnames(s_mapping_holdout) <- stringr::str_replace_all(
  colnames(s_mapping_holdout),
  " ",
  "_"
)


readr::write_csv(
  s_mapping_train,
  "data/modeled_features/spotify_genres_mapping_train.csv"
)
readr::write_csv(
  s_mapping_holdout,
  "data/modeled_features/spotify_genres_mapping_holdout.csv"
)


# INSPECT MAPPED GENRES TOP LEVEL IGRAPH ----
G <- ncol(P_s_train)
final_genres <- setdiff(
  colnames(P_s_train),
  head(cat_states_s$processing_order, G - chosen_k)
)

k_weights <- cat_states_s$weights[[G - chosen_k + 1]][
  final_genres,
  final_genres
]
# make graph and add sizes as node attributes
k_graph <- igraph::graph_from_adjacency_matrix(
  t(k_weights),
  mode = "directed",
  weighted = TRUE
)
(k_sizes <- cat_states_s$sizes[[G - chosen_k + 1]][final_genres])

# inspect graph with igraph
k_graph_filtered <- igraph::delete_edges(
  k_graph,
  E(k_graph)[is.nan(E(k_graph)$weight)]
)
k_graph_filtered <- igraph::delete_edges(
  k_graph_filtered,
  E(k_graph_filtered)[E(k_graph_filtered)$weight <= 0.05]
)
igraph::plot.igraph(k_graph_filtered, vertex.label.cex = 1)
