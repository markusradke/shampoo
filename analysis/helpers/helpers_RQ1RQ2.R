library(dplyr)
library(ggplot2)
library(data.table)

plot_gini_genre_categories <- function(cat_states, xlim_max, chosen_ks) {
  ginis <- cat_states$metrics$gini
  g_out_sums <- cat_states$weights[[1]] |> colSums(na.rm = TRUE)
  n_dangling <- length(g_out_sums[g_out_sums == 0])
  metrics <- filter(cat_states$metrics, !is.na(gini))
  xlim_max <- min(max(metrics$k), xlim_max)
  metrics <- filter(metrics, k <= xlim_max)
  no_genres <- length(ginis):1 + n_dangling - 1
  ggplot(
    metrics,
    aes(x = k, y = gini)
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
  metrics <- filter(cat_states$metrics, !is.na(gini))
  xlim_max <- max(metrics$k)
  ggplot(
    metrics,
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

get_majority_votes <- function(long, cat_states) {
  #' Get the original majority vote tag of each track, i.e. its hard SHAMPOO category before any aggregation (same tie-breaking as SHAMPOO).
  shampoo::assign_genre_categories(
    long,
    cat_states,
    k = length(cat_states$tags),
    method = "hard"
  ) |>
    select(id, cat_naive = cat)
}

plot_one_hot_acc_trajectory <- function(
  cat_states,
  xlim_max
) {
  metrics <- cat_states$metrics |>
    filter(!is.na(onehot_acc) & k <= xlim_max)
  xlim_max <- min(max(metrics$k), xlim_max)
  ggplot(
    metrics,
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
      label = "SHAMPOO labels fully agree with one-hot encoding",
      hjust = 1,
      vjust = -0.5,
      color = "grey45",
      size = 4
    ) +
    annotate(
      "text",
      x = xlim_max,
      y = 0,
      label = "SHAMPOO labels fully disagree with one-hot encoding",
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

plot_full_one_hot_acc_trajectory <- function(cat_states) {
  metrics <- cat_states$metrics |>
    filter(!is.na(onehot_acc))
  xlim_max <- max(metrics$k)
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
      label = "SHAMPOO labels fully agree with one-hot encoding",
      hjust = 1,
      vjust = -0.4,
      color = "grey45",
      size = 4
    ) +
    annotate(
      "text",
      x = xlim_max,
      y = 0,
      label = "SHAMPOO labels fully disagree with one-hot encoding",
      hjust = 1,
      vjust = -0.4,
      color = "grey45",
      size = 4
    ) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
    )
}

save_supergenre_network_gephi <- function(cat_states, trackmap, k) {
  state <- nrow(cat_states$weights[[1]]) - k + 1
  final_genres <- cat_states$final_genres[[state]]
  k_weights <- cat_states$weights[[state]][final_genres, final_genres]
  sizes <- count(trackmap, cat)
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
    message(sprintf("Graph %s has no edges, skipping export.", filepath))
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

get_flow_summary <- function(majority_votes, hard_classes, perc_threshold = 1) {
  #' Get a summary of the flows between hard classes from original majority vote tags to latent categories.
  #' @param majority_votes The data frame containing the majority vote tag of each track. Data frame containing columns: id, cat_naive.
  #' @param hard_classes The data frame containing the hard class assignments. Data frame containing columns: id, cat.
  #' @param perc_threshold The minimum percentage threshold for including flows in the summary. Default is 1%.
  #' @return A data frame summarizing the flows between hard classes and naive predictions, including prevalence and flow percentages.
  hard_classes_naive <- left_join(
    hard_classes,
    majority_votes,
    by = c("id")
  ) |>
    select(id, cat, cat_naive)

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
