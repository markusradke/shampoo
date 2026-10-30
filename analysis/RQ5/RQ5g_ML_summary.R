rm(list = ls())
gc()
library(dplyr)
library(readr)
source("analysis/config.R")

# TABLE 4: CV AND HOLDOUT PERFORMANCE OF ALL MODELS ----
# needs the outputs of RQ5d (LightGBM), RQ5e (linear SVM), and RQ5f (TabICL)
get_best_cv_metrics <- function(model_name, k) {
  tuning_path <- sprintf(
    "models/classification/tuning/%s_%s_k%d_tuning_metrics.csv",
    model_name,
    RUN_TAG,
    k
  )
  if (!file.exists(tuning_path)) {
    return(data.frame(cv_F1_macro = NA_character_, cv_kappa = NA_character_))
  }
  read_csv(tuning_path, show_col_types = FALSE) |>
    slice_max(F1_macro_mean, n = 1, with_ties = FALSE) |>
    transmute(
      cv_F1_macro = sprintf("%.3f±%.3f", F1_macro_mean, F1_SE),
      cv_kappa = sprintf("%.3f±%.3f", kappa_mean, kappa_SE)
    )
}

model_names <- c(lgbm = "LightGBM", linear_svm = "SVM", tabicl = "TabICLv2")
table_4 <- expand.grid(
  model = names(model_names),
  k = CHOSEN_KS,
  stringsAsFactors = FALSE
) |>
  rowwise() |>
  reframe(
    k = k,
    model = model_names[[model]],
    get_best_cv_metrics(model, k),
    readRDS(get_model_path(paste0("holdout_evaluation_", model), k))$metrics
  ) |>
  rename(
    holdout_F1_macro = F1_macro,
    holdout_kappa = kappa,
    tau_F1_certainty = certainty_f1_kendall_tau
  ) |>
  arrange(k, model)
print(table_4)
write_csv(table_4, sprintf("reports/figures/table04_ml_performance_%s.csv", RUN_TAG))
