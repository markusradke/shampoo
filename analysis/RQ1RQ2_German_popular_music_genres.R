rm(list = ls())
gc()
library(dplyr)
library(ggplot2)
library(flextable)
devtools::load_all()
source("analysis/helpers/helpers_RQ1RQ2.R")

LAMBDA <- 0.5
CHOSEN_Ks <- c(5, 17, 32)

# LOAD DATA ----
mb <- data.table::fread("data/long_data/mb_long.csv")

# APPLY SHAMPOO ----
P <- get_normalized_wide_vote_matrix(mb)
readr::write_csv(
  as.data.frame(P),
  "data/normalized_voting_matrices/P_POPTRAG.csv"
)

dag <- get_tag_dag_from_normalized_votes(P)
cat_states <- get_genre_categories_from_graph(
  dag,
  P,
  lambda = LAMBDA
)
saveRDS(
  cat_states,
  sprintf("models/dag/cat_states_full_%d-lambda.rds", round(LAMBDA * 100))
)
cat_states <- readRDS(sprintf(
  "models/dag/cat_states_full_%d-lambda.rds",
  round(LAMBDA * 100)
))

# RQ1 OPTIMAL GRANULARITIES ----
plot_gini_genre_categories(cat_states, 50, CHOSEN_Ks)
ggsave(
  "reports/figures/fig03_gini_trajectory.png",
  width = 6,
  height = 4.5,
  dpi = 600
)
plot_gini_genre_categories_full(cat_states)
ggsave(
  "reports/figures/appendix/figA1_gini_trajectory_full.png",
  width = 6,
  height = 4.5,
  dpi = 600
)

plot_one_hot_acc_trajectory(cat_states, xlim_max = 50)
ggsave(
  "reports/figures/fig04_one_hot_acc_trajectory_max50.png",
  width = 6,
  height = 4.5,
  dpi = 600
)

plot_full_one_hot_acc_trajectory(cat_states, P)
ggsave(
  "reports/figures/appendix/figA2_one_hot_acc_trajectory_full.png",
  width = 6,
  height = 4.5,
  dpi = 600
)


# RQ2 NETWORKS OF SUPERGENRES ----
for (k in CHOSEN_Ks) {
  message(sprintf("Processing k = %d (max: %d)", k, max(CHOSEN_Ks)))
  save_chosen_k_supergenre_networks_gephi(cat_states, P, k)
}

for (k in CHOSEN_Ks) {
  message(sprintf("Inspecting flows for k = %d", k))
  trackmap <- get_track_category_probabilities(P, cat_states, k)
  ids <- add_track_map_to_long(mb, trackmap) |> distinct(track.s.id, cat)
  flows_cutoff <- get_flow_summary(mb, ids, perc_threshold = 1)
  readr::write_csv(
    flows_cutoff$flows,
    sprintf("models/dag/flows_k%d.csv", k)
  )
  flextable::flextable(flows_cutoff$flows) |>
    flextable::save_as_docx(
      path = sprintf("reports/figures/flows_k%d.docx", k)
    )

  # calculate differentiation and boundary strength
  flows_full <- get_flow_summary(mb, ids, perc_threshold = 0)
  differentiation <- flows_full$tracks |>
    group_by(cat) |>
    count(cat_naive) |>
    # effective number of parties is inverted Herfindahl index
    summarise(differentiation = 1 / DescTools::Herfindahl(n)) |>
    ungroup()

  boundary_strength <- trackmap |>
    group_by(cat) |>
    summarize(boundary_strength = mean(cat_prob)) |>
    ungroup()

  res <- differentiation |>
    left_join(boundary_strength, by = "cat") |>
    mutate(k = k)
  readr::write_csv(
    res,
    sprintf("models/dag/differentiation_boundary_strength_k%d.csv", k)
  )
}
