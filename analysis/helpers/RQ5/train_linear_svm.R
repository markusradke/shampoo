source("analysis/helpers/RQ5/train_shared.R")

tune_linear_svm_model <- function(k, feature_set, folds, n_costs) {
  #' Evaluate a logarithmic grid of costs between 1e-4 and 1e4 with 4-fold CV. Each fold is preprocessed once, and its costs run in parallel workers in a shuffled order, which spreads the slow fits of high costs over all workers.
  #' @return The CV metrics per cost, also written to the tuning folder.
  costs <- exp(seq(log(1e-4), log(1e4), length.out = n_costs))
  set.seed(RANDOM_STATE)
  fold_metrics <- with_worker_progress({
    progress <- progressr::progressor(length(folds$X_train) * n_costs)
    lapply(seq_along(folds$X_train), function(i) {
      message(sprintf(
        "Linear SVM %s K=%d: fold %d of %d",
        feature_set,
        k,
        i,
        length(folds$X_train)
      ))
      fold <- preprocess_linear_svm_fold(
        folds$X_train[[i]],
        folds$y_hot_train[[i]],
        folds$X_val[[i]],
        folds$y_hot_val[[i]]
      )
      future.apply::future_lapply(
        sample(costs),
        evaluate_linear_svm_cost, # defined at top level, so only its arguments are sent to the workers
        fold = fold,
        progress = progress,
        future.seed = TRUE,
        future.packages = "LiblineaR"
      ) |>
        bind_rows()
    })
  })
  cv_metrics <- bind_rows(fold_metrics) |>
    group_by(cost) |>
    summarize_cv_metrics()
  readr::write_csv(
    cv_metrics,
    get_tuning_metrics_path("linear_svm", k, feature_set)
  )
  cv_metrics
}

preprocess_linear_svm_fold <- function(
  train_data,
  train_labels,
  val_data,
  val_labels
) {
  #' Preprocess the training and validation data of a CV fold with the imputation and scaling fitted on its training data.
  preprocessing <- preprocess_linear_svm_features(train_data)
  y_train <- factor(train_labels$cat)
  list(
    x_train = preprocessing$x,
    y_train = y_train,
    x_val = apply_linear_svm_preprocessing(val_data, preprocessing),
    y_val = factor(val_labels$cat, levels = levels(y_train))
  )
}

evaluate_linear_svm_cost <- function(cost, fold, progress) {
  #' Train a linear SVM with `cost` on the preprocessed training data of a CV fold and get its metrics on the validation data. Frees the memory of the fit right away, as many workers run in parallel.
  fit <- fit_linear_svm(fold$x_train, fold$y_train, cost)
  y_pred <- predict_linear_svm_matrix(fit, fold$x_val, levels(fold$y_train))
  metrics <- compute_metrics(y_pred, fold$y_val)
  rm(fit)
  gc()
  progress(sprintf("cost %.4g: F1 macro %.3f", cost, metrics$F1_macro))
  data.frame(cost = cost, metrics)
}

train_linear_svm_model <- function(train_data, train_labels, cost) {
  #' Train a class-weighted L2-regularized L2-loss linear SVM (primal solver) on median-imputed, standardized features.
  preprocessing <- preprocess_linear_svm_features(train_data)
  y_train <- factor(train_labels$cat)
  list(
    fit = fit_linear_svm(preprocessing$x, y_train, cost),
    feature_names = preprocessing$feature_names,
    imputation = preprocessing$imputation,
    scaling = preprocessing$scaling,
    class_levels = levels(y_train)
  )
}

fit_linear_svm <- function(x, y, cost) {
  class_weights <- as.numeric(get_balanced_class_weights(y))
  names(class_weights) <- as.character(seq_along(class_weights))
  LiblineaR::LiblineaR(
    data = x,
    target = as.integer(y),
    type = 2,
    cost = cost,
    wi = class_weights,
    bias = 1,
    verbose = FALSE
  )
}

preprocess_linear_svm_features <- function(train_data) {
  #' Fit the median imputation and standardization on the training features.
  #' @return The preprocessed feature matrix `x`, the feature names, and the fitted imputation and scaling.
  feature_names <- setdiff(names(train_data), "track.s.id")
  if (!all(vapply(train_data[feature_names], is.numeric, logical(1)))) {
    stop("Linear SVM features must be numeric after preprocessing.")
  }
  x <- as.matrix(train_data[feature_names])
  imputation <- fit_median_imputer(x)
  x <- apply_median_imputer(x, imputation)
  scaling <- fit_svm_scaler(x)
  list(
    x = apply_svm_scaler(x, scaling),
    feature_names = feature_names,
    imputation = imputation,
    scaling = scaling
  )
}

apply_linear_svm_preprocessing <- function(new_data, preprocessing) {
  #' Impute and standardize new data with a fitted preprocessing (or a model containing it).
  missing_features <- setdiff(preprocessing$feature_names, names(new_data))
  if (length(missing_features) > 0) {
    stop(sprintf(
      "Missing linear SVM features: %s",
      paste(missing_features, collapse = ", ")
    ))
  }
  x <- as.matrix(new_data[preprocessing$feature_names])
  x <- apply_median_imputer(x, preprocessing$imputation)
  apply_svm_scaler(x, preprocessing$scaling)
}

fit_svm_scaler <- function(x_train) {
  centers <- colMeans(x_train)
  scales <- apply(x_train, 2, sd)
  scales[is.na(scales) | scales == 0] <- 1
  list(center = centers, scale = scales)
}

apply_svm_scaler <- function(x, scaling) {
  sweep(sweep(x, 2, scaling$center, "-"), 2, scaling$scale, "/")
}

predict_linear_svm <- function(model, new_data) {
  predict_linear_svm_matrix(
    model$fit,
    apply_linear_svm_preprocessing(new_data, model),
    model$class_levels
  )
}

predict_linear_svm_matrix <- function(fit, x, class_levels) {
  decision_values <- predict(fit, x, decisionValues = TRUE)$decisionValues
  # LiblineaR orders the columns by first appearance of the classes in training
  class_columns <- as.character(seq_along(class_levels))
  choose_first_max_class(
    decision_values[, class_columns, drop = FALSE],
    class_levels
  )
}
