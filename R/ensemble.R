# Simplest possible ensemble: mean-pool a named list of predicted
# probability vectors (e.g. val_probs / test_probs, one numeric vector
# per model, all the same length/row order) into a single combined
# probability vector — a plain row-wise average across models, no
# weighting, no meta-model. The result can be dropped into val_probs /
# test_probs like any other model's probs and threshold-tuned /
# evaluated identically (see optimal_threshold(), compare_models()).
ensemble_probs <- function(probs_list) {
  rowMeans(do.call(cbind, probs_list))
}

# Basic pruning: rank each model in `probs_list` by validation AUC and
# return the names of the top `top_n` models. Meant to be computed once
# on validation predictions/labels, then used to subset both val_probs
# and test_probs before pooling (ensemble_probs()) — dropping weak
# models before pooling keeps a handful of low-AUC models from dragging
# down the pooled probability, without any weighting scheme.
prune_by_auc <- function(probs_list, actual, top_n = 5) {
  aucs <- purrr::map_dbl(probs_list, ~ as.numeric(pROC::auc(actual, .x, quiet = TRUE)))
  names(sort(aucs, decreasing = TRUE))[seq_len(min(top_n, length(aucs)))]
}
