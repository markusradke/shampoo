rm(list = ls())
gc()
library(dplyr)
library(data.table)
library(ggplot2)
library(readr)
devtools::load_all()
source("analysis/config.R")
source("analysis/helpers/helpers_RQ3.R")

N_BINS <- 5 # quantile bins of popularity and release year for stratification
CHECKPOINT_DIR <- "models/dag/robustness_checkpoints"

# LOAD DATA ----
mb <- fread(
  "data/long_data/mb_long.csv",
  select = c(
    "id",
    "tag_name",
    "tag_count",
    "track.s.firstartist.id",
    "album.s.releaseyear",
    "artist.s.popularity"
  )
)
max_k <- length(unique(mb$tag_name))

# CALCULATE ROBUSTNESS TRAJECTORIES FOR DIFFERENT DRAWS IN PARALLEL ----
set.seed(RANDOM_STATE)
seeds <- sample(1:10000, N_REPEATS, replace = FALSE)

start_workers(min(N_WORKERS, N_REPEATS), fork = TRUE)
robustness <- future.apply::future_lapply(
  seq_len(N_REPEATS),
  function(i) {
    checkpoint <- file.path(
      CHECKPOINT_DIR,
      sprintf("robustness_trajectories_%s_%02d.csv", RUN_TAG, i)
    )
    if (file.exists(checkpoint)) {
      return(read_csv(checkpoint, show_col_types = FALSE))
    }
    load_shampoo_in_worker()
    trajectories <- get_robustness_trajectories(
      mb,
      random_state = seeds[i],
      lambda = LAMBDA,
      max_k = max_k,
      n_bins = N_BINS
    )
    write_csv(trajectories, checkpoint)
    trajectories
  },
  future.seed = TRUE, # draws are seeded explicitly with `seeds`
  future.packages = "dplyr"
) |>
  bind_rows()
future::plan(future::sequential)

# CALCULATE ROBUSTNESS STATISTICS AND PLOT ----
robustness_statistics <- get_robustness_statistics(robustness)
plot_robustness(
  robustness_statistics,
  statistics = c("order_precision_top_k"),
  ylabel = "Order precision (top k intersection)",
  max_k = 150
)
save_figure("fig06_robustness_order.png", width = 6, height = 4.5)

plot_robustness(
  robustness_statistics,
  statistics = c("track_acc", "track_prob_overlap"),
  ylabel = "Track assignment accuracy and probability overlap",
  max_k = 150
)
save_figure("fig07_robustness_trackassignment.png", width = 6, height = 4.5)
