library(lightgbm)
library(dplyr)
library(ggplot2)
source("analysis/helpers/RQ5/pfi_feature_groups.R")
source("analysis/helpers/RQ5/tuning_and_training.R")
source("analysis/helpers/RQ5/evaluate_classifiers.R")

evaluate_and_save_grouped_pfi_for_all_k_models <- function(
  ks,
  feature_groups,
  n_rep,
  random_state
) {
  lgbm_importance <- get_holdout_permutation_feature_importance(
    KS = ks,
    feature_groups = feature_groups,
    random_state = random_state,
    n_rep = n_rep
  )
  saveRDS(
    lgbm_importance,
    "models/classification/holdout_permutation_feature_importance_lgbm.rds"
  )
  # lgbm_importance <- readRDS(
  #   "models/classification/holdout_permutation_feature_importance_lgbm.rds"
  # )
  lgbm_importance_stats <- get_pfi_statistics(lgbm_importance)
  plot_pfi(lgbm_importance_stats)
  ggsave(
    "reports/figures/fig13_pfi.png",
    width = 10,
    height = 8,
    dpi = 600
  )
}


get_holdout_permutation_feature_importance <- function(
  KS,
  feature_groups,
  random_state = 42,
  n_rep = 5
) {
  res <- data.frame(
    k = integer(),
    rep = integer(),
    feature_group = character(),
    n_features_group = integer(),
    base_f1_macro = numeric(),
    f1_macro_permuted = numeric(),
    pfi_f1_group = numeric()
  )

  for (k in KS) {
    holdout <- readRDS(sprintf(
      "data/classification_ready_data/holdout_k%d.rds",
      k
    ))
    model <- readRDS(sprintf(
      "models/classification/final_lgbm_model_k%d.rds",
      k
    ))

    X_holdout <- holdout$X_holdout
    y_holdout_hot <- holdout$y_hot_holdout

    metrics <- evaluate_lgbm_model(
      model,
      X_holdout,
      y_holdout_hot
    )

    base_f1_macro <- metrics$F1_macro
    base_mn_log_loss <- metrics$mn_log_loss

    # shuffle features and calculate metrics for each permutation
    for (group in seq_along(feature_groups)) {
      message(sprintf(
        "Evaluating feature group %d/%d: %s",
        group,
        length(feature_groups),
        names(feature_groups)[group]
      ))
      set.seed(random_state)
      group_name <- names(feature_groups)[group]
      current_features <- feature_groups[[group]]
      n_features <- length(current_features)
      for (i in seq(n_rep)) {
        message(sprintf("Repetition %d of %d", i, n_rep))
        X_holdout_permuted <- X_holdout
        for (feature in current_features) {
          X_holdout_permuted[[feature]] <- sample(X_holdout_permuted[[feature]])
        }
        metrics_perm <- evaluate_lgbm_model(
          model,
          X_holdout_permuted,
          y_holdout_hot
        )
        f1_perm <- metrics_perm$F1_macro
        pfi_group_rep <- base_f1_macro - f1_perm # because higher is better

        res <- bind_rows(
          res,
          data.frame(
            k = k,
            rep = i,
            feature_group = group_name,
            n_features_group = n_features,
            base_f1_macro = base_f1_macro,
            f1_macro_permuted = f1_perm,
            pfi_f1_group = pfi_group_rep
          )
        )
      }
    }
  }
  res
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
  # do not display distribution genres in plot
  pfi_statistics_wout_distribution_genres <- pfi_statistics |>
    filter(
      feature_group != "Spotify Artist Distribution Genre Map (27 features)"
    )
  # print pfi statistics for distribution genres
  pfi_statistics |>
    filter(
      feature_group == "Spotify Artist Distribution Genre Map (27 features)"
    ) |>
    select(k, mean_pfi, ci_lower, ci_upper) |>
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
