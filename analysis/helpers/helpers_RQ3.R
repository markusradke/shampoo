source("analysis/helpers/shared_helpers.R")

get_robustness_trajectories <- function(
  mb,
  random_state,
  lambda = 0.5,
  max_k = length(unique(mb$tag_name)),
  n_bins = 5
) {
  #' Compare the SHAMPOO models of two artist-disjoint training folds of one random split on a shared holdout set.
  #' @param mb A long data frame of tracks with columns id, track.s.firstartist.id, album.s.releaseyear, artist.s.popularity, tag_name, tag_count.
  #' @param random_state The random seed of the split.
  #' @param lambda The size / generality tradeoff parameter of SHAMPOO.
  #' @param max_k The maximum number of genre categories to compare.
  #' @param n_bins The number of quantile bins of popularity and release year used for stratification.
  #' @return A data frame with one row per k containing the processing order precision and Kendall's tau of the intersection, the hard assignment accuracy and the soft probability overlap on the holdout tracks recognized by both models, and the number of these tracks.
  splits <- stratified_split_1_2_holdout(mb, random_state, n_bins)
  models <- lapply(splits[c("fold_1", "fold_2")], function(fold) {
    model <- suppressMessages(shampoo::aggregate_genre_categories(
      shampoo::create_genre_dag(fold),
      lambda
    ))
    model$weights <- NULL # unused here, frees memory
    model
  })
  ks <- seq_len(min(max_k, lengths(lapply(models, `[[`, "tags"))))
  track_agreement <- get_track_agreement(ks, models, splits$holdout)
  data.frame(
    random_state = random_state,
    n_bins = n_bins,
    lambda = lambda,
    get_order_agreement(
      models$fold_1$relevance_order,
      models$fold_2$relevance_order,
      ks
    ),
    track_agreement
  )
}

stratified_split_1_2_holdout <- function(long, seed, n_bins = 5) {
  #' Split a long data frame of tracks with tags into two folds and holdout set of equal size (number of artists).
  #' At the artist level, stratified by popularity and release year (each binned).
  #' @param long A long data frame of tracks with tags. Must containc columns:
  #' id, track.s.firstartist.id, artist.s.popularity, album.s.releaseyear.
  #' @param seed A random seed for reproducibility.
  #' @param n_bins The number of bins to use for stratification (default is 5).
  #' @return A list containing three elements: fold_1, fold_2, and holdout, which are data frames of tracks with tags for each split.
  initial_split_ids <- split_artist_level_popularity_release_year(
    long,
    train_size = 0.66,
    seed = seed,
    n_bins = n_bins
  )
  holdout <- long |>
    filter(id %in% initial_split_ids$test_tracks)
  train <- long |>
    filter(id %in% initial_split_ids$train_tracks)
  fold_split_ids <- split_artist_level_popularity_release_year(
    train,
    train_size = 0.5,
    seed = seed,
    n_bins = n_bins
  )
  fold_1 <- train |>
    filter(id %in% fold_split_ids$train_tracks)
  fold_2 <- train |>
    filter(id %in% fold_split_ids$test_tracks)
  return(list(fold_1 = fold_1, fold_2 = fold_2, holdout = holdout))
}

split_artist_level_popularity_release_year <- function(
  mb,
  train_size = 0.8,
  n_bins = 5,
  seed = 42
) {
  #' Split a long data frame of music tracks into train and test sets at the artist level, stratified by popularity and release year.
  #' @param mb A long data frame of music tracks with columns for artist ID, popularity, and release year.
  #' @param train_size The proportion of artists in the training set (default is 0.8).
  #' @param seed A random seed for reproducibility (default is 42).
  #' @return A list containing two elements: train_tracks and test_tracks, which are vectors of track IDs for the training and testing sets, respectively.
  artist_strata <- get_artist_strata(mb, n_bins)
  split <- get_artist_level_strata_split(artist_strata, mb, train_size, seed)
  extract_split_tracks(mb, split$train, split$test)
}


get_artist_strata <- function(mb, n_bins) {
  #' Get the stratum of each artist from the quantile bins of artist popularity and median release year (computed across artists). Artists with missing values are merged into the lowest bin.
  get_quantile_bin <- function(values) {
    bins <- cut(
      values,
      breaks = quantile(
        values,
        probs = seq(0, 1, length.out = n_bins + 1),
        na.rm = TRUE
      ),
      labels = FALSE,
      include.lowest = TRUE
    )
    ifelse(is.na(bins), 1, bins) # merge missing values into the lowest bin
  }
  mb |>
    dplyr::group_by(track.s.firstartist.id) |>
    dplyr::summarize(
      median_release_year = median(album.s.releaseyear, na.rm = TRUE),
      artist_popularity = median(artist.s.popularity, na.rm = TRUE) # same for all tracks of an artist
    ) |>
    dplyr::mutate(
      popularity_bin = get_quantile_bin(artist_popularity),
      release_year_bin = get_quantile_bin(median_release_year),
      strata = paste(popularity_bin, release_year_bin, sep = "_")
    )
}

get_order_agreement <- function(order_1, order_2, ks) {
  #' Get the precision of the top-k categories of two relevance orders and Kendall's tau of the ranks of their intersection.
  data.frame(
    k = ks,
    order_precision_top_k = sapply(ks, function(k) {
      length(intersect(order_1[1:k], order_2[1:k])) / k
    }),
    order_tau_intersect_top_k = sapply(ks, function(k) {
      shared <- intersect(order_1[1:k], order_2[1:k])
      if (length(shared) < 2) {
        return(NA) # Not enough items to compute Kendall's tau
      }
      cor(match(shared, order_1), match(shared, order_2), method = "kendall")
    })
  )
}

get_track_agreement <- function(ks, models, holdout) {
  #' Get hard assignment accuracy and soft probability overlap of two SHAMPOO models on the holdout tracks recognized by both models.
  #' Uses the matrices behind `shampoo::assign_genre_categories()` one k at a time (assignments below the number of terminal nodes are those of the clipped k). Categories in only one model add pmin(p, 0) = 0 to the overlap, so it equals the zero-padded overlap.
  votes <- lapply(models, function(model) {
    suppressWarnings(shampoo:::prepare_assignment_votes(holdout, model))
  })
  recognized <- intersect(rownames(votes$fold_1$P), rownames(votes$fold_2$P))
  votes <- lapply(votes, function(model_votes) {
    model_votes$P <- model_votes$P[recognized, , drop = FALSE] # tiebreak order stays that of all recognized tracks
    model_votes
  })
  do.call(
    rbind,
    lapply(ks, function(k) {
      probabilities <- Map(
        function(model, model_votes) {
          shampoo:::get_category_probability_matrix(
            model_votes$P,
            model,
            max(k, length(model$terminal_nodes))
          )
        },
        models,
        votes
      )
      winners <- Map(
        function(track_probabilities, model_votes) {
          colnames(track_probabilities)[shampoo:::calculate_winners(
            track_probabilities,
            model_votes$tiebreak_order
          )]
        },
        probabilities,
        votes
      )
      shared <- intersect(
        colnames(probabilities$fold_1),
        colnames(probabilities$fold_2)
      )
      data.frame(
        track_acc = mean(winners$fold_1 == winners$fold_2),
        track_prob_overlap = mean(rowSums(pmin(
          probabilities$fold_1[, shared, drop = FALSE],
          probabilities$fold_2[, shared, drop = FALSE]
        ))),
        n_holdout_tracks = length(recognized)
      )
    })
  )
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
    if (max_k < 200) {
      x_breaks <- c(seq(0, 100, by = 10), seq(100, max_k, by = 20))
    } else {
      x_breaks <- c(seq(0, max_k, by = 50))
    }
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
