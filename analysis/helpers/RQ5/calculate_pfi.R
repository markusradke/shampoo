library(lightgbm)
library(dplyr)
library(ggplot2)
source("analysis/helpers/RQ5/pfi_feature_groups.R")
source("analysis/helpers/RQ5/evaluate_classifiers.R")

get_holdout_permutation_feature_importance <- function(
  model_path,
  holdout,
  feature_groups,
  n_rep,
  random_state = 42
) {
  #' Get the drop in holdout F1 macro of a LightGBM model when permuting all features of a group jointly, for each feature group (in parallel workers) and repetition.
  base_f1_macro <- evaluate_lightgbm_model(
    readRDS(model_path),
    holdout$X_holdout,
    holdout$y_hot_holdout
  )$F1_macro
  threads_per_group <- max(1L, N_WORKERS %/% length(feature_groups))
  with_worker_progress({
    progress <- progressr::progressor(length(feature_groups))
    future.apply::future_lapply(
      names(feature_groups),
      function(group_name) {
        model <- readRDS(model_path)
        current_features <- feature_groups[[group_name]]
        set.seed(random_state)
        group_pfi <- lapply(seq_len(n_rep), function(i) {
          X_holdout_permuted <- holdout$X_holdout
          row_permutation <- sample.int(nrow(X_holdout_permuted))
          X_holdout_permuted[current_features] <- holdout$X_holdout[
            row_permutation,
            current_features,
            drop = FALSE
          ]
          f1_permuted <- evaluate_lightgbm_model(
            model,
            X_holdout_permuted,
            holdout$y_hot_holdout,
            num_threads = threads_per_group
          )$F1_macro
          data.frame(
            rep = i,
            feature_group = group_name,
            n_features_group = length(current_features),
            base_f1_macro = base_f1_macro,
            f1_macro_permuted = f1_permuted,
            pfi_f1_group = base_f1_macro - f1_permuted # higher is better
          )
        }) |>
          bind_rows()
        progress(sprintf("feature group %s done", group_name))
        group_pfi
      },
      future.seed = TRUE,
      future.packages = c("dplyr", "lightgbm")
    )
  }) |>
    bind_rows()
}

get_pfi_statistics <- function(pfi_long) {
  pfi_long <- pfi_long |>
    select(k, feature_group, n_features_group, pfi_f1_group) |>
    rename(pfi_group = pfi_f1_group)
  pfi_long |>
    group_by(k, feature_group) |>
    summarize(
      mean_pfi = mean(pfi_group),
      se_pfi = sd(pfi_group) / sqrt(n()),
      ci_lower = mean_pfi - qt(0.975, df = n() - 1) * se_pfi,
      ci_upper = mean_pfi + qt(0.975, df = n() - 1) * se_pfi,
      .groups = "drop"
    ) |>
    left_join(
      distinct(pfi_long, feature_group, n_features_group),
      by = "feature_group"
    ) |>
    mutate(
      feature_group = paste0(
        feature_group,
        " (",
        n_features_group,
        " features)"
      )
    )
}


plot_pfi <- function(pfi_statistics) {
  x_axis_title <- "Permutation Feature Importance\n(F1 Macro, with 95% CI)"
  # do not display distribution genres in plot, but print their statistics
  is_distribution_genres <- startsWith(
    pfi_statistics$feature_group,
    "Spotify Artist Distribution Genre Map"
  )
  pfi_statistics_wout_distribution_genres <- pfi_statistics[
    !is_distribution_genres,
  ]
  pfi_statistics[is_distribution_genres, ] |>
    select(k, feature_group, mean_pfi, ci_lower, ci_upper) |>
    print()

  ggplot(
    pfi_statistics_wout_distribution_genres,
    aes(x = mean_pfi, y = reorder(feature_group, mean_pfi), color = factor(k))
  ) +
    geom_point(size = 1) +
    geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), width = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    scale_color_discrete(
      name = "# genre categories"
    ) +
    scale_x_continuous(name = x_axis_title) + #, trans = "log10") +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 14, color = "grey45"),
      legend.text = element_text(size = 14, color = "grey45"),
      legend.position = "top",
      legend.justification = "left",
      axis.text.x = element_text(size = 12, color = "grey45"),
      axis.text.y = element_text(size = 14, color = "black"),
      axis.title.x = element_text(
        size = 14,
        color = "grey45",
        hjust = 0
      ),
      axis.title.y = element_blank()
    )
}
