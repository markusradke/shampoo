library(dplyr)
source("analysis/helpers/shared_helpers.R")


get_joined_classification_features <- function(
  poptrag,
  poptrag_features,
  spotify,
  expressions,
  labels,
  missing_features_drop = 0.4
) {
  message(
    "Joining features from poptrag, spotify, and expressions with labels..."
  )
  joined <- left_join(
    select(labels, track.s.id),
    select(poptrag, all_of(poptrag_features)),
    by = "track.s.id"
  ) |>
    left_join(spotify, by = "track.s.id") |>
    left_join(expressions, by = "track.s.id")
  message("Dropping rows with >40% missing features...")
  relfreq_missings <- rowSums(is.na(joined)) / (ncol(joined) - 1)
  drop_rows <- relfreq_missings > missing_features_drop
  dropped <- joined[!drop_rows, ]
  message(sprintf(
    "Retained %s samples (%d%%) after dropping rows with >40%% missing features",
    format(nrow(dropped), big.mark = ","),
    round(nrow(dropped) / nrow(labels) * 100)
  ))
  dropped
}

get_splits_with_dummy_coded_features <- function(
  joined_train,
  y_train,
  artist_train,
  joined_holdout,
  y_holdout,
  categorical_features,
  n_folds = 4,
  seed = 42
) {
  # create artist folds for cross-validation from training labels
  splits_ids <- get_artist_level_cat_split(
    cat = y_train,
    artists = artist_train,
    n_folds = n_folds,
    seed = seed
  )
  coded_splits_holdout <- dummy_code_categorical_features(
    joined_train = joined_train,
    split_ids = splits_ids,
    joined_holdout = joined_holdout,
    categorical_features
  )

  correct_colnames(coded_splits_holdout)
}

get_artist_level_cat_split <- function(
  cat,
  artists,
  n_folds = 4,
  seed = 42
) {
  #' Split training tracks into artist-level folds, stratified by the artists' modal category (ties go to the rarest category).
  artist_cats <- left_join(artists, cat, by = "track.s.id")
  cat_tie_order <- names(sort(table(artist_cats$cat)))
  artist_genres <- artist_cats |>
    group_by(track.s.firstartist.id) |>
    summarize(
      mode_genre = get_mode_with_tie_breaker(cat, cat_tie_order),
      .groups = "drop"
    )
  set.seed(seed)
  artist_folds <- get_artist_folds(artist_genres, n_folds = n_folds)
  report_fold_class_counts(artist_folds, artist_genres, artist_cats)
  lapply(artist_folds, function(fold) {
    filter(artists, track.s.firstartist.id %in% fold$track.s.firstartist.id) |>
      select(track.s.id)
  })
}

get_artist_folds <- function(artist_genres, n_folds = 4) {
  artist_folds <- rsample::vfold_cv(
    artist_genres,
    v = n_folds,
    strata = mode_genre,
    pool = 0 # stratify by every category, also the rare ones
  )
  lapply(artist_folds$splits, function(split) {
    select(rsample::assessment(split), track.s.firstartist.id)
  })
}

report_fold_class_counts <- function(artist_folds, artist_genres, artist_cats) {
  #' Print and save the number of artists (by modal category) and tracks per category in each validation fold, and their minimum per fold.
  class_counts <- lapply(seq_along(artist_folds), function(i) {
    fold_artists <- artist_folds[[i]]$track.s.firstartist.id
    full_join(
      artist_genres |>
        filter(track.s.firstartist.id %in% fold_artists) |>
        count(cat = mode_genre, name = "n_artists"),
      artist_cats |>
        filter(track.s.firstartist.id %in% fold_artists) |>
        count(cat, name = "n_tracks"),
      by = "cat"
    ) |>
      mutate(fold = i)
  }) |>
    bind_rows() |>
    tidyr::replace_na(list(n_artists = 0L, n_tracks = 0L))
  readr::write_csv(
    class_counts,
    "data/classification_ready_data/validation_fold_class_counts.csv"
  )
  message("Minimum number of artists and tracks per category in each validation fold:")
  class_counts |>
    group_by(fold) |>
    summarize(
      min_artists = min(n_artists),
      category_min_artists = cat[which.min(n_artists)],
      min_tracks = min(n_tracks),
      category_min_tracks = cat[which.min(n_tracks)]
    ) |>
    print()
}

dummy_code_categorical_features <- function(
  joined_train,
  split_ids,
  joined_holdout,
  categorical_features
) {
  initial_dummies <- get_initial_dummy_coding(
    joined_train,
    joined_holdout,
    categorical_features
  )
  train_initial <- initial_dummies$joined_train
  holdout_initial <- initial_dummies$joined_holdout
  splits_with_dummies <- lapply(split_ids, function(fold) {
    train_initial |>
      filter(track.s.id %in% fold$track.s.id)
  })
  list(
    splits = splits_with_dummies,
    holdout = holdout_initial
  )
}

get_initial_dummy_coding <- function(
  joined_train,
  joined_holdout,
  categorical_features
) {
  for (feature in categorical_features) {
    message(sprintf("Dummy coding feature: %s", feature))
    base_level <- joined_train |>
      filter(!is.na(.data[[feature]])) |>
      count(.data[[feature]]) |>
      arrange(desc(n)) |>
      slice(1) |>
      pull(.data[[feature]])
    message(sprintf("Base level for %s: %s", feature, base_level))
    # dummy code train and holdout (create a flag variable for each level except base level)
    levels_feature <- joined_train |>
      filter(!is.na(.data[[feature]])) |>
      distinct(.data[[feature]]) |>
      pull(.data[[feature]])
    for (level in levels_feature) {
      if (level != base_level) {
        new_col_name <- paste0(feature, "_", level)
        joined_train[[new_col_name]] <- ifelse(
          joined_train[[feature]] == level,
          1,
          0
        )
        joined_holdout[[new_col_name]] <- ifelse(
          joined_holdout[[feature]] == level,
          1,
          0
        )
      }
    }
  }
  list(
    joined_train = select(joined_train, -all_of(categorical_features)),
    joined_holdout = select(joined_holdout, -all_of(categorical_features))
  )
}


correct_colnames <- function(coded_splits_holdout) {
  # remove whitespace from column names in splits and holdout
  coded_splits_holdout$splits <- lapply(
    coded_splits_holdout$splits,
    function(df) {
      colnames(df) <- gsub("\\s+", "_", colnames(df))
      df
    }
  )
  colnames(coded_splits_holdout$holdout) <- gsub(
    "\\s+",
    "_",
    colnames(coded_splits_holdout$holdout)
  )
  coded_splits_holdout
}


lump_small_observations_dummy_cols <- function(
  splits_with_dummies,
  holdout_initial,
  threshold = 100,
  categorical_features
) {
  all_cols <- colnames(splits_with_dummies[[1]])
  cat_cols <- get_categorical_dummy_cols(
    all_cols,
    categorical_features
  )

  stats <- compute_positives_by_col(
    splits_with_dummies,
    cat_cols
  )
  cols_to_drop <- select_cols_below_threshold(stats, threshold)

  splits_lumped <- apply_lumping_to_splits(
    splits_with_dummies,
    cols_to_drop,
    categorical_features,
    threshold
  )
  holdout_lumped <- lump_cols_to_other(
    holdout_initial,
    cols_to_drop,
    categorical_features
  )
  list(
    splits = splits_lumped,
    holdout = holdout_lumped
  )
}

get_categorical_dummy_cols <- function(
  all_cols,
  categorical_features
) {
  categorical_columns <- all_cols[stringr::str_detect(
    all_cols,
    paste0("(", paste(categorical_features, collapse = "|"), ")_")
  )]
  # old
  # cat_cols_all <- unique(unlist(lapply(categorical_features, function(pref) {
  #   all_cols[grepl(pref, all_cols)]
  # })))
  # intersect(cat_cols_all, colnames(holdout_initial))
  categorical_columns
}

compute_positives_by_col <- function(
  splits_with_dummies,
  cat_cols
) {
  pos_splits <- lapply(splits_with_dummies, function(df) {
    colSums(df[, cat_cols, drop = FALSE], na.rm = TRUE)
  })
  pos_splits
}

select_cols_below_threshold <- function(pos_splits, threshold) {
  cols <- names(pos_splits[[1]])
  min_pos <- sapply(cols, function(c) {
    min(sapply(pos_splits, `[[`, c))
  })
  cols[min_pos < threshold]
}

apply_lumping_to_splits <- function(
  splits_with_dummies,
  cols_to_drop,
  categorical_features,
  threshold
) {
  for (col in cols_to_drop) {
    other_col <- get_other_col(col, categorical_features)
    message(sprintf(
      "Dropping column %s from all splits and holdout due to low positives (<%d)",
      col,
      threshold
    ))
    splits_with_dummies <- lapply(
      splits_with_dummies,
      lump_col_into_other,
      col = col,
      other_col = other_col
    )
  }
  splits_with_dummies
}

lump_cols_to_other <- function(df, cols_to_drop, categorical_features) {
  for (col in cols_to_drop) {
    other_col <- get_other_col(col, categorical_features)
    df <- lump_col_into_other(df, col = col, other_col = other_col)
  }
  df
}

get_other_col <- function(col, categorical_features) {
  pref <- categorical_features[stringr::str_detect(
    categorical_features,
    stringr::str_extract(col, "^.*?(?=_)")
  )]
  pref <- pref[[1]]
  paste0(pref, "_other")
}

lump_col_into_other <- function(x, col, other_col) {
  if (!other_col %in% names(x)) {
    x[[other_col]] <- 0
  }
  x[[other_col]] <- x[[other_col]] + x[[col]]
  x[[col]] <- NULL
  x
}

get_folds_from_splits <- function(splits) {
  folds <- list(
    X_train = list(),
    X_val = list()
  )
  for (i in seq_along(splits)) {
    folds$X_train[[i]] <- bind_rows(splits[-i])
    folds$X_val[[i]] <- splits[[i]]
  }
  folds
}

add_aligned_labels_to_folds <- function(folds, y_train) {
  folds_labels <- list(
    y_hot_train = list(),
    y_hot_val = list()
  )
  for (i in seq_along(folds$X_train)) {
    folds_labels$y_hot_train[[i]] <- folds$X_train[[i]] |>
      select(track.s.id) |>
      left_join(y_train, by = "track.s.id") |>
      select(track.s.id, cat)
    folds_labels$y_hot_val[[i]] <- folds$X_val[[i]] |>
      select(track.s.id) |>
      left_join(y_train, by = "track.s.id") |>
      select(track.s.id, cat)
  }
  c(folds, folds_labels)
}

get_aligned_holdout_labels <- function(holdout, y_holdout) {
  list(
    X_holdout = holdout,
    y_hot_holdout = holdout |>
      select(track.s.id) |>
      left_join(y_holdout, by = "track.s.id") |>
      select(track.s.id, cat, cat_prob)
  )
}
