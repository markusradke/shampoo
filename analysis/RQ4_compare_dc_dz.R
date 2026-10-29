rm(list = ls())
gc()

library(dplyr)
library(ggplot2)
library(data.table)
devtools::load_all()
source("analysis/config.R")
source("analysis/helpers/helpers_RQ4.R")

mb <- fread("data/long_data/mb_long.csv")
dc <- fread("data/long_data/dc_long.csv")
dz <- fread("data/long_data/dz_long.csv") |>
  apply_ext_translation(get_dz_translation_map()) # German names to English
cat_states <- readRDS(
  sprintf("models/dag/cat_states_full_%d-lambda.rds", round(LAMBDA * 100))
)
dag <- shampoo::create_genre_dag(mb)
mb_genres <- get_tag_names(mb, "tag_name")

# COMPARE TO DISCOGS ----
dc_equivalent <- get_dc_equiv_sets(mb_genres, get_tag_names(dc, "tag_name"))
res_dc <- run_external_compare(dc_equivalent, mb, dc, dag, "Discogs")
save_figure(
  "fig08_DC_contingency.png",
  plot = res_dc$plot,
  width = 8,
  height = 6
)

# COMPARE TO DEEZER ----
dz_equivalent <- get_dz_equiv(mb_genres, get_tag_names(dz, "tag_name"))
res_dz <- run_external_compare(dz_equivalent, mb, dz, dag, "Deezer")
save_figure(
  "fig09_DZ_contingency.png",
  plot = res_dz$plot,
  width = 12,
  height = 10
)

# COMPARE BALANCE OF CATEGORIES ----
ginis <- list(
  discogs = calc_ginis(dc, res_dc$hard_classes, mb, cat_states),
  deezer = calc_ginis(dz, res_dz$hard_classes, mb, cat_states)
)
print(ginis)
saveRDS(ginis, "models/dag/ginis_comparison.rds")
