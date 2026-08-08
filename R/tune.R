# Stratified k-fold CV grid search: expands `search_space` (a named list of
# vectors) into every hyperparameter combination via expand.grid. For each
# combination, splits `train_raw` (unscaled) into `k` stratified folds via
# make_cv_folds(); for each fold, fits a fresh preprocessor on that fold's
# training rows only (no leakage), applies it to both the fold-train and
# fold-held-out rows, fits the model with `fit_fn(fold_train, ...hyperparams)`,
# and scores it on the fold-held-out rows with AUC using
# `predict_fn(model, fold_val)` (must return predicted probability of the
# positive class). Returns a data frame with one column per hyperparameter,
# a `mean_auc` column (averaged across the k folds), and a `best` flag (TRUE
# for the row(s) with the highest mean_auc).
cv_grid_search <- function(train_raw, num_vars, cat_vars, fit_fn, predict_fn,
                            search_space, k = 5, seed = NULL,
                            target = "Status", positive = "Fail") {
  grid <- expand.grid(search_space, stringsAsFactors = FALSE)
  fold <- make_cv_folds(train_raw, k = k, strata = target, seed = seed)

  mean_auc <- numeric(nrow(grid))
  for (i in seq_len(nrow(grid))) {
    params <- as.list(grid[i, , drop = FALSE])

    fold_auc <- numeric(k)
    for (f in seq_len(k)) {
      fold_train_raw <- train_raw[fold != f, , drop = FALSE]
      fold_val_raw   <- train_raw[fold == f, , drop = FALSE]

      prep_params <- fit_preprocessor(fold_train_raw, num_vars)
      fold_train  <- apply_preprocessor(prep_params, fold_train_raw)
      fold_val    <- apply_preprocessor(prep_params, fold_val_raw)

      model <- do.call(fit_fn, c(list(fold_train), params))
      probs <- predict_fn(model, fold_val)
      actual <- as.integer(fold_val_raw[[target]] == positive)
      fold_auc[f] <- as.numeric(pROC::auc(actual, probs, quiet = TRUE))
    }

    mean_auc[i] <- mean(fold_auc)
  }

  grid$mean_auc <- mean_auc
  grid$best <- grid$mean_auc == max(grid$mean_auc)
  grid
}

# CV sibling of cv_grid_search() for KNN, which has no persisted model
# object and so can't share the generic fit_fn/predict_fn interface. Same
# stratified k-fold / per-fold-refit-preprocessing procedure, but scores
# each `k` in `k_grid` via `knn_probs()`. Returns a data frame with columns
# `k`, `mean_auc`, and a `best` flag (TRUE for the row(s) with the highest
# mean_auc).
cv_select_knn_k <- function(train_raw, num_vars, cat_vars, k_grid, folds = 5,
                             seed = NULL, target = "Status", positive = "Fail") {
  fold <- make_cv_folds(train_raw, k = folds, strata = target, seed = seed)

  mean_auc <- numeric(length(k_grid))
  for (i in seq_along(k_grid)) {
    kk <- k_grid[i]

    fold_auc <- numeric(folds)
    for (f in seq_len(folds)) {
      fold_train_raw <- train_raw[fold != f, , drop = FALSE]
      fold_val_raw   <- train_raw[fold == f, , drop = FALSE]

      prep_params <- fit_preprocessor(fold_train_raw, num_vars)
      fold_train  <- apply_preprocessor(prep_params, fold_train_raw)
      fold_val    <- apply_preprocessor(prep_params, fold_val_raw)

      probs <- knn_probs(fold_train, fold_val, num_vars, cat_vars, kk)
      actual <- as.integer(fold_val_raw[[target]] == positive)
      fold_auc[f] <- as.numeric(pROC::auc(actual, probs, quiet = TRUE))
    }

    mean_auc[i] <- mean(fold_auc)
  }

  result <- data.frame(k = k_grid, mean_auc = mean_auc)
  result$best <- result$mean_auc == max(result$mean_auc)
  result
}

# Sweep `thresholds` and return the one that maximizes `metric` ("f1" or
# "youdens_j") when classifying `probs` against binary `actual` (0/1).
optimal_threshold <- function(probs, actual, metric = c("f1", "youdens_j"),
                               thresholds = seq(0, 1, 0.01)) {
  metric <- match.arg(metric)

  score_at <- function(t) {
    pred <- as.integer(probs >= t)
    tp <- sum(pred == 1 & actual == 1)
    fp <- sum(pred == 1 & actual == 0)
    tn <- sum(pred == 0 & actual == 0)
    fn <- sum(pred == 0 & actual == 1)

    if (metric == "f1") {
      precision <- if (tp + fp == 0) 0 else tp / (tp + fp)
      recall    <- if (tp + fn == 0) 0 else tp / (tp + fn)
      if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)
    } else {
      sensitivity <- if (tp + fn == 0) 0 else tp / (tp + fn)
      specificity <- if (tn + fp == 0) 0 else tn / (tn + fp)
      sensitivity + specificity - 1
    }
  }

  scores <- sapply(thresholds, score_at)
  thresholds[which.max(scores)]
}
