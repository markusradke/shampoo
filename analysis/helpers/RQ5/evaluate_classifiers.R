evaluate_final_model_on_holdout <- function(K, folds, holdout) {
  message(sprintf("Evaluating final lgbm model on holdout for K=%d", K))
  final_lgbm_model <- readRDS(sprintf(
    "models/classification/final_lgbm_model_k%d.rds",
    K
  ))
  holdout_data <- holdout$X_holdout
  y_holdout_hot <- holdout$y_hot_holdout
  holdout_evaluation <- evaluate_lgbm_model(
    final_lgbm_model,
    holdout_data,
    y_holdout_hot,
    mode = "final"
  )
  saveRDS(
    holdout_evaluation,
    sprintf("models/classification/holdout_evaluation_lgbm_k%d.rds", K)
  )
  print(holdout_evaluation$metrics)
  plot <- plot_confusion_matrix(holdout_evaluation)
  ggplot2::ggsave(
    sprintf("models/classification/holdout_confusion_matrix_lgbm_k%d.png", K),
    plot = plot,
    width = ifelse(K > 20, 12, 8),
    height = ifelse(K > 20, 10, 6),
    dpi = 600
  )
}

evaluate_lgbm_model <- function(
  model,
  val_data,
  val_labels_hot_df,
  mode = "train"
) {
  message("Evaluating lgbm model on validation data...")
  hot_levels <- levels(val_labels_hot_df$cat)
  y_pred_prob <- predict(model, as.matrix(select(val_data, -track.s.id)))
  colnames(y_pred_prob) <- hot_levels
  y_pred_hot <- apply(y_pred_prob, 1, function(x) {
    colnames(y_pred_prob)[which.max(x)]
  }) |>
    factor(levels = hot_levels)
  val_labels_hot <- val_labels_hot_df$cat
  if (mode == "train") {
    return(compute_metrics(
      y_pred_hot,
      val_labels_hot
    ))
  }
  if (mode == "final") {
    compute_final_evaluation(
      y_pred_hot,
      val_labels_hot_df
    )
  }
}

compute_metrics <- function(y_pred_hot, y_true_hot) {
  kappa <- yardstick::kap_vec(y_pred_hot, y_true_hot)
  F1_macro <- get_f1_macro(y_pred_hot, y_true_hot)
  metrics <- data.frame(
    kappa = kappa,
    F1_macro = F1_macro
  )
  metrics
}

get_f1 <- function(y_pred, y_true) {
  # calculate f1 manually to account for 0 predictions in some classes
  classes <- levels(y_true)
  f1_scores <- sapply(classes, function(class) {
    true_positive <- sum(y_pred == class & y_true == class)
    false_positive <- sum(y_pred == class & y_true != class)
    false_negative <- sum(y_pred != class & y_true == class)
    precision <- ifelse(
      true_positive + false_positive == 0,
      0,
      true_positive / (true_positive + false_positive)
    )
    recall <- ifelse(
      true_positive + false_negative == 0,
      0,
      true_positive / (true_positive + false_negative)
    )
    f1 <- ifelse(
      precision + recall == 0,
      0,
      2 * (precision * recall) / (precision + recall)
    )
    f1
  })
  f1_scores
}

get_f1_macro <- function(y_pred, y_true) {
  f1_scores <- get_f1(y_pred, y_true)
  mean(f1_scores)
}

append_metrics_to_file <- function(metrics, model, trial_number) {
  metrics <- cbind(
    trial_number = trial_number,
    model = model,
    metrics
  )
  filename <- sprintf(
    "models/classification/tuning/%s_k%d_tuning_metrics.csv",
    model,
    K
  )
  # look for existing file, if not found create new file with header
  file_exists <- file.exists(filename)
  if (!file_exists) {
    file.create(filename)
    fileConn <- file(filename, open = "a")
    writeLines(paste(colnames(metrics), collapse = ","), fileConn)
    close(fileConn)
  }
  # append metrics to file as new row
  fileConn <- file(filename, open = "a")
  for (i in 1:nrow(metrics)) {
    writeLines(paste(metrics[i, ], collapse = ","), fileConn)
  }
  close(fileConn)
}


compute_final_evaluation <- function(
  y_pred_hot,
  val_labels_hot_df
) {
  val_labels_hot <- val_labels_hot_df$cat
  kappa <- yardstick::kap_vec(y_pred_hot, val_labels_hot)
  F1_macro <- get_f1_macro(y_pred_hot, val_labels_hot)

  certainties <- get_label_certainty(val_labels_hot_df)
  F1_scores <- get_f1(y_pred_hot, val_labels_hot)
  f1_certainty <- certainties |>
    filter(cat != "total") |>
    left_join(
      data.frame(F1_scores = F1_scores, cat = names(F1_scores)),
      by = "cat"
    )
  kendall_tau <- cor(
    f1_certainty$median_prob,
    f1_certainty$F1_scores,
    method = "kendall"
  )

  metrics <- data.frame(
    kappa = kappa,
    F1_macro = F1_macro,
    median_certainty = certainties$median_prob[certainties$cat == "total"],
    certainty_f1_kendall_tau = kendall_tau
  )

  list(
    metrics = metrics,
    f1_certainty = f1_certainty,
    y_pred_hot = y_pred_hot,
    y_true_hot = val_labels_hot
  )
}

get_label_certainty <- function(y_hot) {
  total <- data.frame(cat = "total", median_prob = y_hot$cat_prob |> median())
  per_class <- y_hot |>
    group_by(cat) |>
    summarize(median_prob = median(cat_prob)) |>
    arrange(desc(median_prob))
  bind_rows(total, per_class)
}


plot_confusion_matrix <- function(holdout_evaluation) {
  cm_plot_data <- prepare_lgbm_cm_plot_data(holdout_evaluation)
  make_cm_plot(
    cm_plot_data$df,
    cm_plot_data$corner_label,
    right_margin_pt = 20
  )
}


prepare_lgbm_cm_plot_data <- function(holdout_evaluation) {
  y_pred_hot <- holdout_evaluation$y_pred_hot
  y_true_hot <- holdout_evaluation$y_true_hot
  # get confusion matrix wih mean predicted probabilities for each cell
  cm <- table(y_true_hot, y_pred_hot) |> prop.table(margin = 1)
  cm <- round(cm * 100, 0)
  cm_df <- as.data.frame(cm) |>
    dplyr::rename(
      truth = y_true_hot,
      pred = y_pred_hot,
      prob = Freq
    )

  cm_df_rich <- cm_df |>
    left_join(
      holdout_evaluation$f1_certainty,
      by = c("truth" = "cat")
    ) |>
    mutate(
      median_prob = round(median_prob * 100),
      f1 = round(F1_scores, 2),
      truth = sprintf("%s (%s%% certain, F1=%.2f)", truth, median_prob, f1)
    ) |>
    select(-median_prob, -F1_scores, -f1) |>
    mutate(
      truth = factor(truth, levels = sort(unique(truth))),
      pred = factor(pred, levels = sort(unique(pred)))
    )
  label <- sprintf(
    "F1<sub>macro</sub> = %.3f<br>Median Certainty = %d%%<br>Cor<sub>Kendall</sub>(F1, Certainty) = %.3f",
    holdout_evaluation$metrics$F1_macro,
    round(holdout_evaluation$metrics$median_certainty * 100),
    holdout_evaluation$metrics$certainty_f1_kendall_tau
  )
  list(df = cm_df_rich, corner_label = label)
}

make_cm_plot <- function(
  probabilities,
  corner_label,
  right_margin_pt = 0
) {
  # input is a data frame with columns: truth, pred, prob
  k <- length(unique(probabilities$truth))
  probabilities$text_color <- ifelse(
    probabilities$prob > 50,
    "white",
    "black"
  )

  ggplot2::ggplot(
    probabilities,
    ggplot2::aes(
      x = .data$pred,
      y = forcats::fct_rev(.data$truth),
      fill = .data$prob
    )
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(
        label = .data$prob,
        color = .data$text_color
      ),
      size = 5
    ) +
    ggplot2::scale_fill_gradient(low = "white", high = "#c40d20") +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(
      title = corner_label,
      x = "PREDICTED CATEGORY",
      y = "TARGET CATEGORY"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(
        size = 13,
        face = "bold",
        color = "black",
        hjust = 0,
        vjust = -0.5,
        lineheight = 1.5,
        margin = ggplot2::margin(b = -57)
      ),
      plot.title.position = "plot",
      plot.margin = ggplot2::margin(
        t = 0,
        r = right_margin_pt,
        b = 0,
        l = 0,
        unit = "pt"
      ),
      legend.position = "none",
      axis.text.x = ggplot2::element_text(
        angle = 70,
        hjust = 0,
        vjust = 0,
        size = 14
      ),
      axis.text.y = ggplot2::element_text(hjust = 1, vjust = 0, size = 14),
      axis.title.x = ggplot2::element_text(hjust = 0),
      axis.title.y = ggplot2::element_text(hjust = 1),
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(
        size = 16,
        face = "bold",
        color = "grey45"
      )
    )
}
