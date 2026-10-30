rm(list = ls())
gc()
library(dplyr)
library(readr)
library(ggplot2)
source("analysis/config.R")
source("analysis/helpers/RQ5/calculate_pfi.R")

# Tables and figures of all ML models. Needs the results of RQ5d of all
# machines in `models/classification/<RUN_TAG>/`; missing results are skipped.
MODEL_NAMES <- c(lgbm = "LightGBM", linear_svm = "SVM", tabicl = "TabICLv2")
CONFUSION_MATRIX_FIGURES <- c("fig10", "fig11", "fig12") # order of CHOSEN_KS

read_holdout_evaluation <- function(model, k, feature_set) {
  path <- get_model_path(paste0("holdout_evaluation_", model), k, feature_set)
  if (!file.exists(path)) {
    warning(sprintf("Skipping missing result %s", path))
    return(NULL)
  }
  readRDS(path)
}

format_cv_metrics <- function(cv_metrics) {
  if (is.null(cv_metrics)) {
    return(data.frame(cv_F1_macro = NA_character_, cv_kappa = NA_character_))
  }
  transmute(
    cv_metrics,
    cv_F1_macro = sprintf("%.3f±%.3f", F1_macro_mean, F1_SE),
    cv_kappa = sprintf("%.3f±%.3f", kappa_mean, kappa_SE)
  )
}

results <- expand.grid(
  model = names(MODEL_NAMES),
  k = CHOSEN_KS,
  feature_set = FEATURE_SETS,
  stringsAsFactors = FALSE
) |>
  rowwise() |>
  mutate(evaluation = list(read_holdout_evaluation(model, k, feature_set))) |>
  ungroup() |>
  filter(!vapply(evaluation, is.null, logical(1)))

# TABLE 4: CV AND HOLDOUT PERFORMANCE OF ALL MODELS AND FEATURE SETS ----
table_4 <- results |>
  rowwise() |>
  reframe(
    feature_set = feature_set,
    k = k,
    model = model,
    format_cv_metrics(get_best_cv_metrics(model, k, feature_set)),
    evaluation$metrics
  ) |>
  mutate(model = unname(MODEL_NAMES[model])) |>
  rename(
    holdout_F1_macro = F1_macro,
    holdout_kappa = kappa,
    tau_F1_certainty = certainty_f1_kendall_tau
  ) |>
  arrange(feature_set, k, model)
print(table_4)
write_csv(
  table_4,
  sprintf("reports/figures/table04_ml_performance_%s.csv", RUN_TAG)
)

# ABLATION: HOLDOUT PERFORMANCE WITHOUT SPOTIFY GENRES ----
ablation <- table_4 |>
  select(feature_set, k, model, holdout_F1_macro, holdout_kappa) |>
  tidyr::pivot_wider(
    names_from = feature_set,
    values_from = c(holdout_F1_macro, holdout_kappa)
  )
if (all(FEATURE_SETS %in% table_4$feature_set)) {
  ablation <- mutate(
    ablation,
    delta_F1_macro = holdout_F1_macro_all - holdout_F1_macro_no_spotify_genres,
    delta_kappa = holdout_kappa_all - holdout_kappa_no_spotify_genres
  )
}
print(ablation)
write_csv(
  ablation,
  sprintf("reports/figures/supp_table_ablation_spotify_genres_%s.csv", RUN_TAG)
)

# CONFUSION MATRICES ----
for (i in seq_len(nrow(results))) {
  result <- results[i, ]
  filename <- if (result$model == "lgbm" && result$feature_set == "all") {
    sprintf(
      "%s_cm_lgbm_k%d.png",
      CONFUSION_MATRIX_FIGURES[match(result$k, CHOSEN_KS)],
      result$k
    )
  } else {
    sprintf("supp_cm_%s_%s_k%d.png", result$model, result$feature_set, result$k)
  }
  save_figure(
    filename,
    plot = plot_confusion_matrix(result$evaluation[[1]]),
    width = ifelse(result$k > 20, 12, 8),
    height = ifelse(result$k > 20, 10, 6)
  )
}

# PERMUTATION FEATURE IMPORTANCE OF LIGHTGBM ----
for (feature_set in FEATURE_SETS) {
  pfi_paths <- vapply(
    CHOSEN_KS,
    function(k) get_model_path("holdout_pfi_lgbm", k, feature_set),
    character(1)
  )
  pfi_paths <- pfi_paths[file.exists(pfi_paths)]
  if (length(pfi_paths) == 0) {
    next
  }
  plot_pfi(get_pfi_statistics(bind_rows(lapply(pfi_paths, readRDS))))
  save_figure(
    if (feature_set == "all") {
      "fig13_pfi.png"
    } else {
      sprintf("supp_pfi_%s.png", feature_set)
    },
    width = 10,
    height = 8
  )
}
