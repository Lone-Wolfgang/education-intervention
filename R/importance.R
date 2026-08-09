# Given a fitted model with a native/built-in feature-importance metric,
# return a standardized tibble(feature, importance) sorted descending by
# importance. Dispatches on model class:
#   - rpart:        model$variable.importance (already only lists vars
#                    that were actually used for a split)
#   - randomForest: randomForest::importance(model)[, "MeanDecreaseGini"]
#   - xgb.Booster:  xgboost::xgb.importance(model = model)$Gain
# Other model classes (qda, knn, glm, lda, cv.glmnet) have no single
# native importance metric and are not supported here.
get_feature_importance <- function(model) {
  if (inherits(model, "rpart")) {
    vi <- model$variable.importance
    if (is.null(vi)) vi <- numeric(0)
    return(tibble::tibble(feature = names(vi), importance = as.numeric(vi)) %>%
             dplyr::arrange(dplyr::desc(importance)))
  }

  if (inherits(model, "randomForest")) {
    vi <- randomForest::importance(model)
    col <- if ("MeanDecreaseGini" %in% colnames(vi)) "MeanDecreaseGini" else colnames(vi)[1]
    return(tibble::tibble(feature = rownames(vi), importance = vi[, col]) %>%
             dplyr::arrange(dplyr::desc(importance)))
  }

  if (inherits(model, "xgb.Booster")) {
    vi <- xgboost::xgb.importance(model = model)
    return(tibble::tibble(feature = vi$Feature, importance = vi$Gain) %>%
             dplyr::arrange(dplyr::desc(importance)))
  }

  stop("get_feature_importance: unsupported model class: ",
       paste(class(model), collapse = "/"))
}
