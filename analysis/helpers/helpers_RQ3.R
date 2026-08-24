source("analysis/helpers/shared_helpers.R")

get_assignments_to_compare <- function(
  mb,
  random_state = 42,
  lambda = 0.5,
  max_k = 10,
  n_bins = 5
) {
  #' Get the order and track assignments for two random splits of a long data frame of music tracks with tags, for a random state, lambda parameter, and maximum number of genre categories (k).
  #' @param mb A long data frame of music tracks with columns for track.s.id, firstartist.id, album.s.releaseyear, artist.s.popularity, tag_name, tag_count.
  #' @param random_state A random seed for reproducibility (default is 42).
  #' @param lambda The size / generality tradeoff parameter for the SHAMPOO algorithm (default is 0.5).
  #' @param max_k The maximum number of genre categories to check for reliability (default is 10, always starts at 2).
  #' @param n_bins The number of bins to use for stratification when splitting the data into folds (default is 5).
  #' @return A list containing two elements: fold_1 and fold_2, which are lists of order and track assignments for each random split, and max_k, which is the maximum number of genre categories to check for reliability.

  splits <- stratified_split_1_2_holdout(
    long = mb,
    seed = random_state,
    n_bins = 5
  )
  P_holdout <- get_normalized_wide_vote_matrix(splits$holdout)
  assignments_1 <- get_order_and_track_assignments(
    splits$fold_1,
    P_holdout,
    lambda,
    max_k
  )
  assignments_2 <- get_order_and_track_assignments(
    splits$fold_2,
    P_holdout,
    lambda,
    max_k
  )
  list(
    fold_1 = assignments_1,
    fold_2 = assignments_2,
    max_k = max_k,
    random_state = random_state,
    n_bins = n_bins,
    lambda = lambda
  )
}

get_robustness_trajectories <- function(assignments) {
  #' Get the robustness trajectories for a random split, including the iteration number, random state, number of categories (k), processing order precision, processing order tau for intersecting categories, track assignment accuracy, and track assignment probability overlap (zero-padded for genres that appear in only one set at k).
  #' @param assignments A list containing two elements: fold_1 and fold_2, which contain order and track assignments for each random split, and metadata: max_k, which is the maximum number of genre categories to check for reliability; random_state, which is the random seed used for reproducibility; n_bins, which is the number of bins used for stratification when splitting the data into folds; and lambda, which is the size / generality tradeoff parameter for the SHAMPOO algorithm.
  #' @return A data frame containing the robustness trajectories for each random split, including the iteration number, random state, number of categories (k), processing order precision, processing order tau for intersecting categories, track assignment accuracy, and track assignment probability overlap (zero-padded for genres that appear in only one set at k).
  assignments_1 <- assignments$fold_1
  assignments_2 <- assignments$fold_2
  max_k <- assignments$max_k
  kendall_tau_k <- get_kendall_tau_precision_ks(
    assignments_1$order,
    assignments_2$order,
    max_k
  )
  track_acc_overlap <- get_track_acc_and_overlap(
    assignments_1$assignments,
    assignments_2$assignments,
    max_k
  )
  data.frame(
    random_state = assignments$random_state,
    n_bins = assignments$n_bins,
    lambda = assignments$lambda,
    k = kendall_tau_k$k,
    order_precision_top_k = kendall_tau_k$precision,
    order_tau_intersect_top_k = kendall_tau_k$tau_intersect,
    track_acc = track_acc_overlap$acc,
    track_prob_overlap = track_acc_overlap$overlap
  )
}

stratified_split_1_2_holdout <- function(long, seed, n_bins = 5) {
  #' Split a long data frame of tracks with tags into two folds and holdout set of equal size (number of artists).
  #' At the artist level, stratified by popularity and release year (each binned).
  #' @param long A long data frame of tracks with tags. Must containc columns:
  #' track.s.id, track.s.firstartist.id, artist.s.popularity, album.s.releaseyear.
  #' @param seed A random seed for reproducibility.
  #' @param n_bins The number of bins to use for stratification (default is 5).
  #' @return A list containing three elements: fold_1, fold_2, and holdout, which are data frames of tracks with tags for each split.
  initial_split_ids <- split_artist_level_popularity_release_year(
    long,
    test_size = 0.66,
    seed = seed,
    n_bins = n_bins
  )
  holdout <- long |>
    filter(track.s.id %in% initial_split_ids$test_tracks)
  train <- long |>
    filter(track.s.id %in% initial_split_ids$train_tracks)
  fold_split_ids <- split_artist_level_popularity_release_year(
    train,
    test_size = 0.5,
    seed = seed,
    n_bins = n_bins
  )
  fold_1 <- train |>
    filter(track.s.id %in% fold_split_ids$train_tracks)
  fold_2 <- train |>
    filter(track.s.id %in% fold_split_ids$test_tracks)
  return(list(fold_1 = fold_1, fold_2 = fold_2, holdout = holdout))
}

split_artist_level_popularity_release_year <- function(
  mb,
  test_size = 0.8,
  n_bins = 5,
  seed = 42
) {
  #' Split a long data frame of music tracks into train and test sets at the artist level, stratified by popularity and release year.
  #' @param mb A long data frame of music tracks with columns for artist ID, popularity, and release year.
  #' @param test_size The proportion of the data to include in the test set (default is 0.8).
  #' @param seed A random seed for reproducibility (default is 42).
  #' @return A list containing two elements: train_tracks and test_tracks, which are vectors of track IDs for the training and testing sets, respectively.
  artist_strata <- get_artist_strata(mb, n_bins)
  split <- get_artist_level_strata_split(artist_strata, mb, test_size, seed)
  split_tracks <- extract_split_tracks(mb, split$train, split$test)
  split_tracks
}


get_artist_strata <- function(mb, n_bins) {
  bin_breaks <- seq(0, 1, length.out = n_bins + 1)
  artist_strata <- mb |>
    dplyr::group_by(track.s.firstartist.id) |>
    dplyr::summarize(
      median_release_year = median(album.s.releaseyear, na.rm = TRUE),
      artist_popularity = median(artist.s.popularity, na.rm = TRUE) # should be same for all tracks of an artist
    ) |>
    dplyr::mutate(
      popularity_bin = cut(
        artist_popularity,
        breaks = c(
          quantile(
            artist_popularity,
            probs = bin_breaks,
            na.rm = TRUE,
            right = TRUE, # include the right endpoint in the interval
            include.lowest = TRUE
          )
        ),
        labels = FALSE
      ),
      popularity_bin = ifelse(
        is.na(popularity_bin),
        1, # Assign a separate bin for missing popularity values
        popularity_bin
      ),
      release_year_bin = cut(
        median_release_year,
        quantile(
          median_release_year,
          probs = bin_breaks,
          na.rm = TRUE
        ),
        labels = FALSE,
        right = TRUE,
        include.lowest = TRUE
      ),
      release_year_bin = ifelse(
        is.na(release_year_bin),
        1, # Assign a separate bin for missing release years
        release_year_bin
      ),
      strata = paste(popularity_bin, release_year_bin, sep = "_")
    )
  return(artist_strata)
}

get_order_and_track_assignments <- function(
  fold,
  P_holdout,
  lambda = 0.5,
  max_k = 100
) {
  G <- ncol(P_holdout)
  max_genres <- min(max_k, G)
  P <- get_normalized_wide_vote_matrix(fold)
  dag <- get_tag_dag_from_normalized_votes(P)
  cat_states <- get_genre_categories_from_graph(dag, P, lambda)
  assignments <- list()
  for (k in seq(1, max_genres, by = 1)) {
    message(sprintf(
      "Getting track assignments for k = %d/%d",
      k,
      max_genres
    ))
    trackmap <- get_track_category_probabilities(
      P = P_holdout,
      res_list = cat_states,
      chosen_k = k
    )
    assignments[[k]] <- trackmap
  }
  list(assignments = assignments, order = cat_states$relevance_order)
}


get_kendall_tau_precision_ks <- function(order1, order2, max_k = 100) {
  ks <- 1:min(length(order1), length(order2), max_k)
  data.frame(
    k = ks,
    precision = sapply(ks, function(k) {
      top_k_1 <- order1[1:k]
      top_k_2 <- order2[1:k]
      intersect_length <- length(intersect(top_k_1, top_k_2))
      precision <- intersect_length / k
      return(precision)
    }),
    tau_intersect = sapply(ks, function(k) {
      top_k_1 <- order1[1:k]
      top_k_2 <- order2[1:k]
      intersect_length <- length(intersect(top_k_1, top_k_2))
      if (intersect_length < 2) {
        return(NA) # Not enough items to compute Kendall's tau
      }
      intersect_names <- intersect(top_k_1, top_k_2)
      top_k1_ranks <- match(top_k_1, order1)
      names(top_k1_ranks) <- top_k_1
      top_k1_inter <- top_k1_ranks[intersect_names]
      top_k2_ranks <- match(top_k_2, order2)
      names(top_k2_ranks) <- top_k_2
      top_k2_inter <- top_k2_ranks[intersect_names]
      tau <- cor(
        top_k1_inter,
        top_k2_inter,
        method = "kendall"
      )
      return(tau)
    })
  )
}

get_track_acc_and_overlap <- function(
  assignments_1,
  assignments_2,
  max_k = 100
) {
  ks <- seq(1, min(length(assignments_1), length(assignments_2), max_k))
  res <- data.frame(
    k = integer(length = length(ks)),
    acc = numeric(length = length(ks)),
    overlap = numeric(length = length(ks))
  )

  for (k in ks) {
    trackmap_1 <- assignments_1[[k]]
    trackmap_2 <- assignments_2[[k]]
    acc <- mean(trackmap_1$cat == trackmap_2$cat)
    overlap <- get_overlap(trackmap_1, trackmap_2)
    res[k, ] <- data.frame(k = k, acc = acc, overlap = overlap)
  }
  return(res)
}

get_overlap <- function(trackmap_1, trackmap_2) {
  cat_probs_1 <- dplyr::select(trackmap_1, -cat, -cat_prob) |> as.matrix()
  cat_probs_2 <- dplyr::select(trackmap_2, -cat, -cat_prob) |> as.matrix()
  diff1 <- setdiff(colnames(cat_probs_2), colnames(cat_probs_1))
  diff2 <- setdiff(colnames(cat_probs_1), colnames(cat_probs_2))
  cat_probs_1pad <- cbind(
    cat_probs_1,
    matrix(0, nrow = nrow(cat_probs_1), ncol = length(diff1))
  )
  colnames(cat_probs_1pad) <- c(colnames(cat_probs_1), diff1)
  cat_probs_2pad <- cbind(
    cat_probs_2,
    matrix(0, nrow = nrow(cat_probs_2), ncol = length(diff2))
  )
  colnames(cat_probs_2pad) <- c(colnames(cat_probs_2), diff2)
  cat_probs_1pad <- cat_probs_1pad[, sort(colnames(cat_probs_1pad))]
  cat_probs_2pad <- cat_probs_2pad[, sort(colnames(cat_probs_2pad))]
  minoverlap <- pmin(cat_probs_1pad, cat_probs_2pad)
  if (all(minoverlap == 1)) {
    return(1) # both matrices identical (case just one equal category)
  }
  overlap <- mean(rowSums(minoverlap))
  overlap
}

get_robustness_statistics <- function(robustness) {
  #' Get robustness statistics from the robustness trajectories.
  #' @param robustness A data frame containing the robustness trajectories for each random split, including the iteration number, random state, number of categories (k), processing order precision, processing order tau for intersecting categories, track assignment accuracy, and track assignment probability overlap (zero-padded for genres that appear in only one set at k).
  #' @return A data frame containing the robustness statistics for each number of categories (k), including the mean, standard error, and 95% confidence interval for each statistic (based on Student t distribution, two-sided).
  long_robustness <- robustness |>
    tidyr::pivot_longer(
      cols = c(
        "track_prob_overlap",
        "track_acc",
        "order_tau_intersect_top_k",
        "order_precision_top_k"
      ),
      names_to = "statistic",
      values_to = "value"
    )
  statistics <- long_robustness |>
    group_by(k, statistic) |>
    summarize(
      mean = mean(value, na.rm = TRUE),
      se = sd(value, na.rm = TRUE) * sqrt(1 / n() + 1), # + 1 = n_train/n_test, Nadeau-Bengio correction for overlapping train/test sets with train/test split ratio 50%/50%
      ci_lower = mean - qt(0.975, df = n() - 1) * se,
      ci_upper = mean + qt(0.975, df = n() - 1) * se,
      .groups = "drop"
    )
  statistics
}

plot_robustness <- function(
  robustness_statistics,
  statistics,
  ylabel,
  max_k = 100
) {
  chosen_statistics <- filter(robustness_statistics, statistic %in% statistics)
  clip_ci <- chosen_statistics |>
    mutate(
      ci_lower = pmax(ci_lower, 0),
      ci_upper = pmin(ci_upper, 1)
    )
  max_k <- min(max_k, max(robustness_statistics$k))
  if (max_k < 100) {
    x_breaks <- seq(0, max_k, by = 10)
  } else {
    x_breaks <- c(seq(0, 100, by = 10), seq(100, max_k, by = 25))
  }

  ggplot(clip_ci, aes(x = k, y = mean, color = statistic)) +
    geom_line(linewidth = .75) +
    geom_ribbon(
      aes(ymin = ci_lower, ymax = ci_upper, fill = statistic),
      alpha = 0.2,
      color = NA
    ) +
    theme_minimal() +
    labs(
      x = "Number of genre categories",
      y = ylabel
    ) +
    scale_y_continuous(
      limits = c(0, 1),
    ) +
    scale_x_continuous(breaks = x_breaks, limits = c(0, max_k)) +
    scale_color_manual(
      values = c(
        "track_acc" = "#c40d20",
        "track_prob_overlap" = "black",
        "order_precision_top_k" = "#c40d20",
        "order_tau_intersect_top_k" = "black"
      ),
      labels = c(
        "track_acc" = "Hard Category Assignment Accuracy",
        "track_prob_overlap" = "Soft Category Probability Overlap (0-padded)",
        "order_precision_top_k" = "Order Precision Top k",
        "order_tau_intersect_top_k" = "Order Kendall's tau Top k (Intersection)"
      )
    ) +
    scale_fill_manual(
      values = c(
        "track_acc" = "#c40d20",
        "track_prob_overlap" = "grey45",
        "order_precision_top_k" = "#c40d20",
        "order_tau_intersect_top_k" = "grey45"
      ),
    ) +
    guides(
      color = guide_legend(title = "", nrow = 2),
      fill = "none"
    ) +
    geom_hline(yintercept = 0.7, linetype = "dashed", color = "grey45") +
    annotate(
      "text",
      x = max_k,
      y = 0.7,
      label = "0.7",
      color = "grey45",
      hjust = 1,
      vjust = 1.5
    ) +
    theme(
      legend.position = "top",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
}
