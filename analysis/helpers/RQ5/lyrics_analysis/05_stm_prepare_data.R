rm(list = ls())
gc()
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(forcats)
library(stm)

# Functions ----
get_zero_index_reference_type <- function(model) {
  theta_sums <- colSums(model$theta)
  reference_type <- as.integer(which.max(theta_sums) - 1)
  return(reference_type)
}

prep_mean_props_by_genre <- function(thetas, labels, genres) {
  thetas$cat <- genres
  pivoted_thetas <- thetas |>
    pivot_longer(
      cols = all_of(labels),
      names_to = "variable",
      values_to = "value"
    ) |>
    group_by(cat, variable) |>
    summarize(value = mean(value), .groups = "drop")
}

save_top_frex_examples <- function(model, labels, voc, configuration) {
  top_frex <- labelTopics(model, n = 200)$frex
  top_frex_df <- data.frame(
    type_num = rep(0:(nrow(top_frex) - 1), each = ncol(top_frex)),
    type_label = rep(labels, each = ncol(top_frex)),
    ngram_rank = rep(1:ncol(top_frex), times = nrow(top_frex)),
    ngram = as.vector(t(top_frex))
  ) |>
    left_join(voc, by = "ngram")
  filepath <- sprintf(
    "models/lyrics/%s_top_frex_words.csv",
    configuration
  )
  readr::write_csv(top_frex_df, filepath)
}


plot_heatmap <- function(pivoted_thetas) {
  max_gradient <- max(pivoted_thetas$value)
  ggplot(pivoted_thetas, aes(x = variable, y = cat, fill = value)) +
    geom_tile() +
    scale_fill_gradient(
      limits = c(0, max_gradient),
      low = "white",
      high = "#c40d20"
    ) +
    geom_text(
      aes(label = sprintf("%.0f", value * 100)),
      color = ifelse(
        pivoted_thetas$value > max_gradient * 0.5,
        "white",
        "black"
      ),
      size = 4.5
    ) +
    scale_x_discrete(position = "top") +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 80, hjust = 0, vjust = 0, size = 16),
      axis.text.y = element_text(size = 16),
      legend.position = "none",
      plot.margin = margin(0, 1, 0, 0, unit = "cm")
    )
}

save_interpretation <- function(
  model,
  labels,
  X_train,
  y_train,
  vocabulary,
  configuration
) {
  readr::write_csv(
    data.frame(
      labels,
      topic_num = 0:(length(labels) - 1)
    ),
    sprintf("data/modeled_features/lyrics/%s_labels.csv", configuration)
  )
  save_top_frex_examples(model, labels, vocabulary, configuration)

  thetas <- X_train
  colnames(thetas) <- labels
  pivoted_thetas <- prep_mean_props_by_genre(thetas, labels, y_train)
  plot <- plot_heatmap(pivoted_thetas)
  ggsave(
    sprintf("models/lyrics//%s_stm_heatmap_train_tracks.png", configuration),
    plot = plot,
    width = 12,
    height = 12
  )
}

get_features_for_classifcation <- function(X, ids, ref_type_index_R) {
  labels <- paste("lyrics.expr_type", 1:ncol(X), sep = "_")
  colnames(X) <- labels
  X <- X[-ref_type_index_R]
  X <- cbind(ids, X)
  colnames(X)[1] <- "track.s.id"
  return(X)
}


# Load data ----
stm_model <- readRDS("models/lyrics/stm_model.rds")
X_train <- read_csv("data/modeled_features/lyrics/stm_train.csv")
train_meta <- read_csv("data/modeled_features/lyrics/meta_train.csv")
y_train <- train_meta$cat
ids_train <- train_meta$track.s.id
X_holdout <- read_csv("data/modeled_features/lyrics/stm_test.csv")
ids_holdout <- read_csv(
  "data/modeled_features/lyrics/meta_holdout.csv"
)$track.s.id

vocabulary <- read_csv(
  "data/modeled_features/lyrics/fighting_discourseanalysis_vocabulary.csv"
)

# Determine reference types ----
ref_type_index_zero <- get_zero_index_reference_type(stm_model)
ref_type_index_R <- ref_type_index_zero + 1

# Interpret models ----
type_labels <- paste("lyrics.expr", seq(1, 25), sep = "_")
labelTopics(stm_model, n = 200)$frex[1, ] # total of 25 latent expression types

train <- get_features_for_classifcation(
  X_train,
  ids_train,
  ref_type_index_R
)

holdout <- get_features_for_classifcation(
  X_holdout,
  ids_holdout,
  ref_type_index_R
)
readr::write_csv(
  train,
  "data/modeled_features/lyrics/expressiontypes_train_no_ref_type.csv"
)
readr::write_csv(
  holdout,
  "data/modeled_features/lyrics/expressiontypes_holdout_no_ref_type.csv"
)
