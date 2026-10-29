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

start_workers(min(N_WORKERS, N_REPEATS))
robustness <- with_worker_progress({
  progress <- progressr::progressor(N_REPEATS)
  future.apply::future_lapply(
    seq_len(N_REPEATS),
    function(i) {
      checkpoint <- file.path(
        CHECKPOINT_DIR,
        sprintf("robustness_trajectories_%s_maxk%d_%02d.csv", RUN_TAG, max_k, i)
      )
      if (file.exists(checkpoint)) {
        progress(sprintf("repetition %d read from checkpoint", i))
        return(read_csv(checkpoint, show_col_types = FALSE))
      }
      start_time <- Sys.time()
      load_shampoo_in_worker()
      trajectories <- get_robustness_trajectories(
        mb,
        random_state = seeds[i],
        lambda = LAMBDA,
        max_k = max_k,
        n_bins = N_BINS
      )
      write_csv(trajectories, checkpoint)
      progress(sprintf(
        "repetition %d done in %.1f min",
        i,
        difftime(Sys.time(), start_time, units = "mins")
      ))
      trajectories
    },
    future.seed = TRUE, # draws are seeded explicitly with `seeds`
    future.packages = c("dplyr", "readr")
  )
}) |>
  bind_rows()
future::plan(future::sequential)
write_csv(robustness, "models/dag/robustness_trajectories.csv")

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
  statistics = c("order_precision_top_k"),
  ylabel = "Order precision (top k intersection)",
  max_k = max_k
)
save_figure("appendix/robustness_order_full.png", width = 6, height = 4.5)

plot_robustness(
  robustness_statistics,
  statistics = c("track_acc", "track_prob_overlap"),
  ylabel = "Track assignment accuracy",
  max_k = 150
)
save_figure("fig07_robustness_trackassignment.png", width = 6, height = 4.5)

plot_robustness(
  robustness_statistics,
  statistics = c("track_acc", "track_prob_overlap"),
  ylabel = "Track assignment accuracy",
  max_k = max_k
)
save_figure(
  "appendix/robustness_track_assignments_full.png",
  width = 6,
  height = 4.5
)
