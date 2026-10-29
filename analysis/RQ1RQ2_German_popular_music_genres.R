rm(list = ls())
gc()
library(dplyr)
library(ggplot2)
library(flextable)
devtools::load_all()
source("analysis/config.R")
source("analysis/helpers/helpers_RQ1RQ2.R")

# LOAD DATA ----
mb <- readr::read_csv("data/long_data/mb_long.csv", show_col_types = FALSE)

# APPLY SHAMPOO ----
dag <- shampoo::create_genre_dag(mb)
cat_states <- shampoo::aggregate_genre_categories(dag, lambda = LAMBDA)
saveRDS(
  cat_states,
  sprintf("models/dag/cat_states_full_%d-lambda.rds", round(LAMBDA * 100))
)
cat_states <- readRDS(sprintf(
  "models/dag/cat_states_full_%d-lambda.rds",
  round(LAMBDA * 100)
))

# RQ1 OPTIMAL GRANULARITIES ----
plot_gini_genre_categories(cat_states, 50, CHOSEN_KS)
save_figure("fig03_gini_trajectory.png", width = 6, height = 4.5)
plot_gini_genre_categories_full(cat_states)
save_figure("appendix/figA1_gini_trajectory_full.png", width = 6, height = 4.5)

plot_one_hot_acc_trajectory(cat_states, xlim_max = 50)
save_figure("fig04_one_hot_acc_trajectory_max50.png", width = 6, height = 4.5)
plot_full_one_hot_acc_trajectory(cat_states)
save_figure(
  "appendix/figA2_one_hot_acc_trajectory_full.png",
  width = 6,
  height = 4.5
)

# RQ2 NETWORKS OF SUPERGENRES AND FLOWS ----
trackmaps <- shampoo::assign_genre_categories(mb, cat_states, CHOSEN_KS)
majority_votes <- get_majority_votes(mb, cat_states)
for (k in CHOSEN_KS) {
  message(sprintf("Inspecting networks and flows for k = %d", k))
  trackmap <- trackmaps[[as.character(k)]]
  save_supergenre_network_gephi(cat_states, trackmap, k)

  ids <- trackmap |> distinct(id, cat)
  flows_cutoff <- get_flow_summary(majority_votes, ids, perc_threshold = 1)
  readr::write_csv(flows_cutoff$flows, sprintf("models/dag/flows_k%d.csv", k))
  flextable::flextable(flows_cutoff$flows) |>
    flextable::save_as_docx(
      path = sprintf("reports/figures/flows_k%d.docx", k)
    )

  # calculate differentiation and boundary strength
  flows_full <- get_flow_summary(majority_votes, ids, perc_threshold = 0)
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
