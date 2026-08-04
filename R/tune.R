# Rigid grid search: expands `search_space` (a named list of vectors) into
# every hyperparameter combination via expand.grid, fits each on train_df
# with `fit_fn(train_df, ...hyperparams)`, scores each on val_df with AUC
# using `predict_fn(model, val_df)` (must return predicted probability of
# the positive class), and returns a data frame with one column per
# hyperparameter, an `auc` column, and a `best` flag (TRUE for the row(s)
# with the highest AUC).
grid_search <- function(train_df, val_df, fit_fn, predict_fn, search_space,
                         target = "Status", positive = "Fail") {
  grid <- expand.grid(search_space, stringsAsFactors = FALSE)
  actual <- as.integer(val_df[[target]] == positive)

  auc <- numeric(nrow(grid))
  for (i in seq_len(nrow(grid))) {
    params <- as.list(grid[i, , drop = FALSE])
    model  <- do.call(fit_fn, c(list(train_df), params))
    probs  <- predict_fn(model, val_df)
    auc[i] <- as.numeric(pROC::auc(actual, probs, quiet = TRUE))
  }

  grid$auc  <- auc
  grid$best <- grid$auc == max(grid$auc)
  grid
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
