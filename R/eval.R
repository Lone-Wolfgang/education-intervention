# Given any fitted model and a validation data frame, return a numeric
# vector of predicted probabilities for the positive class (Fail = 1).
# Dispatches on model class: glm/lda/qda/rpart/randomForest predict
# directly from val_df via their stored formula; cv.glmnet and
# xgb.Booster need num_vars/cat_vars to rebuild the design matrix.
predict_probs <- function(model, val_df, num_vars = NULL, cat_vars = NULL) {
  cls <- class(model)

  if (inherits(model, "glm")) {
    return(predict(model, val_df, type = "response"))
  }
  if (inherits(model, "lda") || inherits(model, "qda")) {
    return(predict(model, val_df)$posterior[, "1"])
  }
  if (inherits(model, "rpart")) {
    return(predict(model, val_df, type = "prob")[, "1"])
  }
  if (inherits(model, "randomForest")) {
    return(predict(model, val_df, type = "prob")[, "1"])
  }
  if (inherits(model, "cv.glmnet")) {
    stopifnot(!is.null(num_vars), !is.null(cat_vars))
    x <- stats::model.matrix(~ . - 1, data = val_df[, c(num_vars, cat_vars)])
    return(as.numeric(predict(model, newx = x, s = "lambda.min", type = "response")))
  }
  if (inherits(model, "xgb.Booster")) {
    stopifnot(!is.null(num_vars), !is.null(cat_vars))
    x <- stats::model.matrix(~ . - 1, data = val_df[, c(num_vars, cat_vars)])
    return(predict(model, x))
  }

  stop("predict_probs: unsupported model class: ", paste(cls, collapse = "/"))
}

# Given predicted probs, true binary labels (0/1), and a decision
# threshold, build a confusion matrix and a summary table of
# classification stats. Returns list(confusion_matrix, summary).
evaluate_predictions <- function(probs, actual, threshold = 0.5) {
  pred <- as.integer(probs >= threshold)
  cm <- table(predicted = pred, actual = actual)

  tp <- sum(pred == 1 & actual == 1)
  fp <- sum(pred == 1 & actual == 0)
  tn <- sum(pred == 0 & actual == 0)
  fn <- sum(pred == 0 & actual == 1)

  accuracy    <- (tp + tn) / (tp + fp + tn + fn)
  precision   <- if (tp + fp == 0) NA else tp / (tp + fp)
  recall      <- if (tp + fn == 0) NA else tp / (tp + fn)
  specificity <- if (tn + fp == 0) NA else tn / (tn + fp)
  f1          <- if (is.na(precision) || is.na(recall) || precision + recall == 0) NA else
    2 * precision * recall / (precision + recall)
  prevalence       <- (tp + fn) / (tp + fp + tn + fn)
  balanced_accuracy <- if (is.na(recall) || is.na(specificity)) NA else (recall + specificity) / 2
  youdens_j        <- if (is.na(recall) || is.na(specificity)) NA else recall + specificity - 1

  summary <- data.frame(
    threshold         = threshold,
    accuracy          = accuracy,
    precision         = precision,
    recall            = recall,
    specificity       = specificity,
    f1                = f1,
    prevalence        = prevalence,
    balanced_accuracy = balanced_accuracy,
    youdens_j         = youdens_j
  )

  list(confusion_matrix = cm, summary = summary)
}

# Compare multiple models: `probs_list` is a named list (model name ->
# predicted probability vector) sharing the same `actual` (0/1) labels.
# `thresholds` is an optional named list/vector of per-model decision
# thresholds (default 0.5 for every model). Returns one row per model
# with AUC, accuracy, precision, sensitivity, specificity, F1, balanced
# accuracy, Youden's J, and prevalence at that model's threshold.
compare_models <- function(probs_list, actual, thresholds = NULL) {
  purrr::imap_dfr(probs_list, function(probs, name) {
    threshold <- if (is.null(thresholds)) 0.5 else thresholds[[name]]
    auc <- as.numeric(pROC::auc(actual, probs, quiet = TRUE))
    ev  <- evaluate_predictions(probs, actual, threshold = threshold)

    tibble::tibble(
      model             = name,
      threshold         = threshold,
      auc               = auc,
      accuracy          = ev$summary$accuracy,
      precision         = ev$summary$precision,
      sensitivity       = ev$summary$recall,
      specificity       = ev$summary$specificity,
      f1                = ev$summary$f1,
      balanced_accuracy = ev$summary$balanced_accuracy,
      youdens_j         = ev$summary$youdens_j,
      prevalence        = ev$summary$prevalence
    )
  })
}
