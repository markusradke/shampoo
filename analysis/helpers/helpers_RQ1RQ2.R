library(dplyr)
library(ggplot2)
library(data.table)

plot_gini_genre_categories <- function(cat_states, xlim_max, chosen_ks) {
  ginis <- cat_states$metrics$gini
  g_out_sums <- cat_states$weights[[1]] |> colSums(na.rm = TRUE)
  n_dangling <- length(g_out_sums[g_out_sums == 0])

  xlim_max <- min(length(ginis), xlim_max)
  no_genres <- length(ginis):1 + n_dangling - 1
  ggplot(
    data.frame(no_genres, gini = ginis),
    aes(x = no_genres, y = gini)
  ) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.5) +
    theme_minimal() +
    labs(
      x = "Number of Genre Categories",
      y = "Gini Coefficient"
    ) +
    scale_x_continuous(
      breaks = seq(0, xlim_max, by = 5),
      limits = c(0, xlim_max)
    ) +
    geom_vline(
      xintercept = chosen_ks,
      linetype = "dashed",
      color = "#c40d20"
    ) +
    ylim(0, 1) +
    geom_hline(
      yintercept = c(0, 1),
      linetype = "dashed",
      color = "grey45"
    ) +
    annotate(
      "text",
      x = xlim_max,
      y = 0,
      label = "totally uniform",
      hjust = 1,
      vjust = -0.5,
      color = "grey45",
      size = 4
    ) +
    annotate(
      "text",
      x = xlim_max,
      y = 1,
      label = "totally unequal",
      hjust = 1,
      vjust = -0.5,
      color = "grey45",
      size = 4
    ) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
    )
}

plot_gini_genre_categories_full <- function(cat_states) {
  g_out_sums <- cat_states$weights[[1]] |> colSums(na.rm = TRUE)
  n_dangling <- length(g_out_sums[g_out_sums == 0])

  xlim_max <- nrow(cat_states$metrics)
  # no_genres <- length(ginis):1 + n_dangling - 1
  ggplot(
    cat_states$metrics,
    aes(x = k, y = gini)
  ) +
    geom_line(linewidth = 0.5) +
    theme_minimal() +
    labs(
      x = "Number of Genre Categories",
      y = "Gini Coefficient"
    ) +
    scale_x_continuous(
      breaks = seq(0, xlim_max, by = 50),
      limits = c(0, xlim_max)
    ) +
    ylim(0, 1) +
    geom_hline(
      yintercept = c(0, 1),
      linetype = "dashed",
      color = "grey45"
    ) +
    annotate(
      "text",
      x = xlim_max,
      y = 0,
      label = "totally uniform",
      hjust = 1,
      vjust = -0.5,
      color = "grey45",
      size = 4
    ) +
    annotate(
      "text",
      x = xlim_max,
      y = 1,
      label = "totally unequal",
      hjust = 1,
      vjust = -0.5,
      color = "grey45",
      size = 4
    ) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
    )
}

get_one_hot_probs <- function(long) {
  long <- dplyr::select(long, track.s.id, tag_name, tag_count)
  order_tags <- long |>
    dplyr::group_by(tag_name) |>
    dplyr::summarize(total_count = dplyr::n_distinct(track.s.id)) |>
    dplyr::arrange(-total_count) |>
    dplyr::select(tag_name) |>
    dplyr::mutate(rank = dplyr::row_number())
  hot <- long |>
    dplyr::left_join(order_tags, by = "tag_name") |>
    dplyr::group_by(track.s.id) |>
    dplyr::arrange(-tag_count, -rank) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()
  all_genres <- unique(long$tag_name)
  full_hot_wide <- tidyr::pivot_wider(
    hot,
    names_from = tag_name,
    values_from = tag_count,
    values_fill = list(tag_count = 0)
  )
  missing_cols <- setdiff(all_genres, colnames(full_hot_wide))
  full_hot_wide[missing_cols] <- 0
  # make all non-zero values 1 (excep for column track.s.id)
  full_hot_norm <- full_hot_wide |>
    dplyr::mutate(dplyr::across(-track.s.id, ~ ifelse(. > 0, 1, 0))) |>
    dplyr::arrange(track.s.id)

  full_wide <- long |>
    tidyr::pivot_wider(
      names_from = tag_name,
      values_from = tag_count,
      values_fill = list(tag_count = 0)
    )
  V <- full_wide[-1] |> as.matrix()
  P <- V / rowSums(V)
  P_full <- cbind(select(full_wide, track.s.id), as.data.frame(P)) |>
    dplyr::arrange(track.s.id)

  M_full <- dplyr::select(P_full, -track.s.id) |> as.matrix()
  M_hot <- dplyr::select(full_hot_norm, -track.s.id) |> as.matrix()
  rownames(M_hot) <- full_hot_norm$track.s.id
  M_hot[, colnames(M_full)]
}


plot_one_hot_acc_trajectory <- function(
  cat_states,
  xlim_max
) {
  ggplot(
    cat_states$metrics,
    aes(x = k, y = onehot_acc)
  ) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.5) +
    theme_minimal() +
    labs(
      x = "Number of Genre Categories",
      y = "Accuracy [0,1]"
    ) +
    scale_x_continuous(
      breaks = seq(0, xlim_max, by = 5),
      limits = c(0, xlim_max)
    ) +
    ylim(0, 1) +
    geom_hline(
      yintercept = c(0, 1),
      linetype = "dashed",
      color = "grey45"
    ) +
    annotate(
      "text",
      x = xlim_max,
      y = 1,
      label = "equal labels as one-hot encoding",
      hjust = 1,
      vjust = -0.5,
      color = "grey45",
      size = 4
    ) +
    annotate(
      "text",
      x = xlim_max,
      y = 0,
      label = "no labels equal to one-hot encoding",
      hjust = 1,
      vjust = -0.5,
      color = "grey45",
      size = 4
    ) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
    )
}

plot_full_one_hot_acc_trajectory <- function(
  cat_states,
  P
) {
  xlim_max <- ncol(P)
  ggplot(
    cat_states$metrics,
    aes(x = k, y = onehot_acc)
  ) +
    geom_line(linewidth = 0.5) +
    theme_minimal() +
    labs(
      x = "Number of Genre Categories",
      y = "Accuracy [0,1]"
    ) +
    scale_x_continuous(
      breaks = seq(0, xlim_max, by = 50),
      limits = c(0, xlim_max)
    ) +
    ylim(0, 1) +
    geom_hline(
      yintercept = c(0, 1),
      linetype = "dashed",
      color = "grey45"
    ) +
    annotate(
      "text",
      x = xlim_max,
      y = 1,
      label = "equal labels as one-hot encoding",
      hjust = 1,
      vjust = -0.4,
      color = "grey45",
      size = 4
    ) +
    annotate(
      "text",
      x = xlim_max,
      y = 0,
      label = "no labels equal to one-hot encoding",
      hjust = 1,
      vjust = 1.1,
      color = "grey45",
      size = 4
    ) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
    )
}

save_chosen_k_supergenre_networks_gephi <- function(cat_states, P, k) {
  track_mapping <- get_track_category_probabilities(
    P,
    cat_states,
    k
  )
  final_genres <- track_mapping |> select(-cat, -cat_prob) |> colnames()
  G <- cat_states$weights[[1]] |> nrow()
  k_weights <- cat_states$weights[[G - k + 1]][
    final_genres,
    final_genres
  ]
  sizes <- count(track_mapping, cat)
  # make graph and add sizes as node attributes
  k_graph <- igraph::graph_from_adjacency_matrix(
    t(k_weights),
    mode = "directed",
    weighted = TRUE
  )
  export_graph_for_gephi_import(
    k_graph,
    sizes,
    sprintf("models/dag/dag_supergenres_k%d.gexf", k),
    threshold = 0.05
  )
}

export_graph_for_gephi_import <- function(
  graph,
  sizes,
  filepath,
  threshold = 0.05
) {
  # must not be more than 36 colors
  # Make n pastel colors (HSL: high lightness + moderate saturation)
  n <- nrow(sizes)
  pastel <- scales::hue_pal(h = c(0, 360), c = 55, l = 85)(n)
  rgb_colors <- col2rgb(pastel) |> as.data.frame() |> t()
  rownames(rgb_colors) <- NULL
  colors <- rgb_colors |>
    as.data.frame() |>
    dplyr::mutate(a = 0.8) |>
    dplyr::select(r = red, g = green, b = blue, a)

  nodes_df <- data.frame(
    id = c(1:igraph::vcount(graph)),
    label = igraph::V(graph)$name
  ) |>
    left_join(sizes, by = c("label" = "cat"))
  sizes <- select(nodes_df, n)

  renamed_nodes <- nodes_df |>
    dplyr::mutate(
      label = stringr::str_replace_all(.data$label, "&", "&amp;"),
      label = stringr::str_replace_all(.data$label, "<", "&lt;"),
      label = stringr::str_replace_all(.data$label, ">", "&gt;"),
      label = stringr::str_replace_all(.data$label, "\"", ""),
      label = stringr::str_replace_all(.data$label, "'", "")
    ) |>
    select(-n)
  n_edges <- igraph::ecount(graph)
  if (n_edges == 0) {
    message(sprintf("Graph %s has no edges, skipping export.", name))
    return(NULL)
  }
  edges_df <- as.data.frame(igraph::get.edges(
    graph,
    c(1:igraph::ecount(graph))
  ))
  colnames(edges_df) <- c("source", "target")
  edges_weight <- igraph::E(graph)$weight

  # filter nan edges edges below threshold
  edges_df <- edges_df[!is.na(edges_weight) & edges_weight >= threshold, ]
  edges_weight <- edges_weight[!is.na(edges_weight) & edges_weight >= threshold]

  rgexf::write.gexf(
    nodes = renamed_nodes,
    nodesVizAtt = list(size = sizes, color = colors),
    edges = edges_df,
    edgesWeight = edges_weight,
    defaultedgetype = "directed",
    output = filepath
  )
}

get_flow_summary <- function(long, hard_classes, perc_threshold = 1) {
  #' Get a summary of the flows between hard classes from naive one-hot predictions to latent categories.
  #' @param long The long format data frame containing the one-hot predictions. Data frame containing columns: track.s.id, tag_name, tag_count.
  #' @param hard_classes The data frame containing the hard class assignments. Data frame containing columns: track.s.id, cat.
  #' @param perc_threshold The minimum percentage threshold for including flows in the summary. Default is 1%.
  #' @return A data frame summarizing the flows between hard classes and naive predictions, including prevalence and flow percentages.
  one_hot <- get_one_hot_probs(long)
  ids <- dcast(
    long,
    track.s.id ~ tag_name,
    value.var = "tag_count",
    fill = 0
  ) |>
    pull(track.s.id)
  # naive_votes <- data.frame(
  #   track.s.id = ids,
  #   cat_naive = apply(one_hot, 1, function(x) {
  #     colnames(one_hot)[which.max(x)]
  #   })
  # )
  naive_votes <- data.frame(
    track.s.id = rownames(one_hot),
    cat_naive = colnames(one_hot)[max.col(one_hot, ties.method = "first")]
  )

  hard_classes_naive <- left_join(
    hard_classes,
    naive_votes,
    by = c("track.s.id")
  ) |>
    select(track.s.id, cat, cat_naive)

  flows <- count(hard_classes_naive, cat, cat_naive) |>
    group_by(cat) |>
    mutate(
      prevalence = sum(n),
      prevalence_rel = round(prevalence / nrow(hard_classes) * 100),
      prob = n / sum(n) * 100
    ) |>
    filter(prob >= perc_threshold) |> # for better readability, only show flows with at least 1% of the total flow for each category
    mutate(prob = round(prob)) |>
    arrange(cat, -n) |>
    ungroup()
  k <- length(unique(hard_classes$cat))
  name_cat_col <- sprintf("SHAMPOO-G%d", k)
  flows_summary <- flows |>
    group_by(cat) |>
    summarize(
      `Original MusicBrainz Majority Vote Assignment` = paste0(
        cat_naive,
        " (",
        prob,
        "%)",
        collapse = ", "
      ),
      prevalence = unique(prevalence),
      prevalence_rel = unique(prevalence_rel)
    ) |>
    mutate(
      !!name_cat_col := paste0(
        cat,
        "\n(",
        ifelse(prevalence_rel == 0, "<1", prevalence_rel),
        "%, n=",
        trimws(format(prevalence, big.mark = ",")),
        ")"
      )
    ) |>
    select(all_of(c(
      name_cat_col,
      "Original MusicBrainz Majority Vote Assignment"
    )))
  list(flows = flows_summary, tracks = hard_classes_naive)
}
