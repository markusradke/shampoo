rm(list = ls())
gc()
library(dplyr)
library(data.table)
library(ggplot2)
library(readr)
devtools::load_all()
source("analysis/helpers/helpers_RQ3.R")

LAMBDA <- 0.5
RANDOM_STATE <- 42
N_BINS <- 5 # for the split of the data into folds
N_REPEATS <- 20 # number of repetitions for the robustness analysis
MAX_K <- 265 # set to number of genre tags after denoising
N_WORKERS <- 3 # number of parallel workers for the robustness analysis

# LOAD DATA ----
mb <- fread("data/long_data/mb_long.csv")


# CALCULATE ROBUSTNESS TRAJECTORIES FOR DIFFERENT DRAWS ----
set.seed(RANDOM_STATE)
seeds <- sample(1:10000, N_REPEATS, replace = FALSE)

for (i in seq_len(N_REPEATS)) {
  message(sprintf(
    "CALCULATING ROBUSTNESS TRAJECTORIES FOR DRAW %d/%d",
    i,
    N_REPEATS
  ))
  if (
    file.exists(sprintf(
      "models/dag/robustness_checkpoints/robustness_trajectories_%02d.csv",
      i
    ))
  ) {
    message("File already exists, skipping...")
    next
  }
  random_state <- seeds[i]

  fold_assignments <- get_assignments_to_compare(
    mb = mb,
    lambda = LAMBDA,
    random_state = random_state,
    max_k = MAX_K,
    n_bins = N_BINS
  )

  robustness <- get_robustness_trajectories(fold_assignments)

  out <- sprintf(
    "models/dag/robustness_checkpoints/robustness_trajectories_%02d.csv",
    i
  )
  write_csv(robustness, out)
  rm(fold_assignments, robustness)
  gc()

  NULL
}

# LOAD ROBUSTNESS TRAJECTORIES AND COMBINE ----
robustness_files <- list.files(
  path = "models/dag/robustness_checkpoints/",
  pattern = "robustness_trajectories_\\d{2}\\.csv",
  full.names = TRUE
)
robustness <- lapply(robustness_files, read_csv) |>
  bind_rows()

# CALCULATE ROBUSTNESS STATISTICS AND PLOT ----
robustness_statistics <- get_robustness_statistics(robustness)
plot_robustness(
  robustness_statistics,
  statistics = c("order_precision_top_k"),
  ylabel = "Order precision (top k intersection)",
  max_k = 150
)
ggsave(
  "reports/figures/fig06_robustness_order.png",
  width = 6,
  height = 4.5,
  dpi = 600
)

plot_robustness(
  robustness_statistics,
  statistics = c("track_acc", "track_prob_overlap"),
  ylabel = "Track assignment accuracy and probability overlap",
  max_k = 150
)
ggsave(
  "reports/figures/fig07_robustness_trackassignment.png",
  width = 6,
  height = 4.5,
  dpi = 600
)
