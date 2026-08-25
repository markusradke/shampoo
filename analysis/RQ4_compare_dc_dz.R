rm(list = ls())
gc()

library(dplyr)
library(ggplot2)
library(data.table)
devtools::load_all()
source("analysis/helpers/helpers_RQ4.R")

mb <- fread("data/long_data/mb_long.csv")
dc <- fread("data/long_data/dc_long.csv")
dz <- fread("data/long_data/dz_long.csv")
cat_states <- readRDS("models/dag/cat_states_full_50-lambda.rds")

mb_genres <- get_tag_names(mb, "tag_name")
dc_genres <- get_tag_names(dc, "tag_name")
dz_genres <- get_tag_names(dz, "tag_name")

P <- get_normalized_wide_vote_matrix(mb)
dag <- get_tag_dag_from_normalized_votes(P)

# COMPARE TO DC ----
dc_equivalent <- get_dc_equiv_sets(mb_genres, dc_genres)
res_dc <- run_dc_compare(
  dc_equivalent,
  mb,
  dc,
  P,
  dag,
  cat_states,
  load_states_if_exists = TRUE
)
ggsave(
  "reports/figures/fig07_DC_contingency.png",
  plot = res_dc$plot,
  width = 8,
  height = 6,
  dpi = 600
)


# COMPARE TO DZ ----
dz_equivalent <- get_dz_equiv(mb_genres, dz_genres)
res_dz <- run_dz_compare(
  dz_equivalent,
  mb,
  dz,
  P,
  dag,
  cat_states,
  load_states_if_exists = FALSE
)
ggsave(
  "reports/figures/fig09_DZ_contingency.png",
  plot = res_dz$plot,
  width = 12,
  height = 10,
  dpi = 600
)


gini <- calc_ginis(res_dc, res_dz, P, cat_states)
saveRDS(gini, "models/dag/ginis_comparison.rds")
