source("R/theme.R")
source("R/colors.R")

# Row-normalized confusion matrix heatmap: rows are the true (actual)
# class, normalized so each row sums to 1 (cell = P(predicted | actual)),
# visualizing per-class recall. `cm` is the table returned by
# evaluate_predictions()$confusion_matrix (table(predicted = ..., actual = ...)).
plot_confusion_matrix <- function(cm, title = "Confusion Matrix") {
  class_labels <- c(`0` = "Pass", `1` = "Fail")

  as.data.frame(cm) %>%
    dplyr::group_by(actual) %>%
    dplyr::mutate(pct = Freq / sum(Freq)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(predicted = class_labels[as.character(predicted)],
                  actual    = class_labels[as.character(actual)],
                  label     = sprintf("%d\n(%s)", Freq, scales::percent(pct, accuracy = 0.1))) %>%
    ggplot2::ggplot(ggplot2::aes(x = predicted, y = actual, fill = pct)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = label),
                        size = 6, fontface = "bold", lineheight = 0.9) +
    scale_fill_seq(limits = c(0, 1), name = "row %") +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = title, x = "Predicted", y = "Actual") +
    report_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

# ROC curve(s) for one or more models. `probs_list` is a named list
# (model name -> predicted probability vector); `actual` is the shared
# binary (0/1) truth vector. Legend labels include each model's AUC.
plot_roc <- function(probs_list, actual) {
  curves <- purrr::imap_dfr(probs_list, function(probs, name) {
    roc_obj <- pROC::roc(actual, probs, quiet = TRUE)
    auc_val <- as.numeric(pROC::auc(roc_obj))
    tibble::tibble(
      model       = sprintf("%s (AUC = %.3f)", name, auc_val),
      specificity = roc_obj$specificities,
      sensitivity = roc_obj$sensitivities
    )
  })

  n_models <- length(probs_list)
  palette  <- if (n_models <= length(pal_categorical)) {
    pal_categorical[seq_len(n_models)]
  } else {
    grDevices::colorRampPalette(pal_categorical)(n_models)
  }

  curves %>%
    ggplot2::ggplot(ggplot2::aes(x = 1 - specificity, y = sensitivity, color = model)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate",
                  color = NULL) +
    report_theme()
}

# For a single model, sweep the decision threshold from 0 to 1 and plot
# the selected `metrics` (default sensitivity/specificity) against
# threshold, marking the threshold that maximizes `target` ("f1" or
# "youdens_j", default "youdens_j") with a vertical line and annotation.
# `target`'s metric is always included in the plotted lines (added to
# `metrics` if not already present), so the optimized metric is visible.
plot_threshold_tuning <- function(probs, actual,
                                   metrics = c("sensitivity", "specificity"),
                                   target = c("youdens_j", "f1"),
                                   thresholds_seq = seq(0, 1, 0.01)) {
  metrics <- match.arg(metrics,
                        c("precision", "sensitivity", "specificity", "f1", "youdens_j"),
                        several.ok = TRUE)
  target <- match.arg(target)
  plot_metrics <- union(metrics, target)

  metric_curve <- purrr::map_dfr(thresholds_seq, function(t) {
    ev <- evaluate_predictions(probs, actual, threshold = t)
    tibble::tibble(threshold = t,
                    precision = ev$summary$precision,
                    sensitivity = ev$summary$recall,
                    specificity = ev$summary$specificity,
                    f1 = ev$summary$f1,
                    youdens_j = ev$summary$youdens_j)
  }) %>%
    tidyr::pivot_longer(dplyr::all_of(plot_metrics), names_to = "metric", values_to = "value")

  best_threshold <- optimal_threshold(probs, actual, metric = target,
                                       thresholds = thresholds_seq)

  n_metrics <- length(plot_metrics)
  palette <- pal_categorical[seq_len(n_metrics)]

  target_label <- if (target == "youdens_j") "Youden's J" else "F1"

  y_min <- min(0, metric_curve$value, na.rm = TRUE)
  y_max <- max(1, metric_curve$value, na.rm = TRUE) + 0.05

  metric_curve %>%
    ggplot2::ggplot(ggplot2::aes(x = threshold, y = value, color = metric)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_vline(xintercept = best_threshold, linetype = "dashed", color = "grey30") +
    ggplot2::annotate("text", x = best_threshold, y = y_max - 0.03,
                       label = sprintf("%.2f", best_threshold),
                       hjust = -0.1, vjust = 0, size = 4, fontface = "bold") +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_y_continuous(limits = c(y_min, y_max), expand = c(0, 0)) +
    ggplot2::labs(title = "Decision-Threshold Tuning",
                  subtitle = paste0("Optimal threshold maximizes ", target_label),
                  x = "Threshold", y = "Value", color = NULL) +
    report_theme()
}

# Horizontal bar chart of feature importances. `importance_df` is a
# tibble(feature, importance) — e.g. the output of
# get_feature_importance() in R/importance.R, or a dummy equivalent.
# Features are sorted descending by importance; `top_n` optionally
# truncates to the top N most important features.
plot_feature_importance <- function(importance_df, top_n = NULL,
                                     title = "Feature Importance") {
  plot_df <- importance_df %>% dplyr::arrange(dplyr::desc(importance))
  if (!is.null(top_n)) plot_df <- utils::head(plot_df, top_n)

  plot_df %>%
    ggplot2::ggplot(ggplot2::aes(x = stats::reorder(feature, importance), y = importance)) +
    ggplot2::geom_col(fill = pal_categorical[1]) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = title, x = NULL, y = "Importance") +
    report_theme()
}

# Parallel-coordinates plot of a CV grid-search result — one axis per
# hyperparameter plus `mean_auc`, one line per hyperparameter combination
# — paired (via patchwork, stacked 3:1 height) with a one-row summary
# table of the winning configuration and its AUC underneath. `cv_result`
# is the data.frame produced by cv_grid_search() / cv_select_knn_k()
# (R/tune.R): one column per hyperparameter, a `mean_auc` column, and a
# `best` logical flag. Parallel coordinates scale to any number of
# hyperparameters (unlike a 2D heatmap), which is the point: some models
# (rf, xgboost) already have 2, and more may be added later. Axes are
# min-max scaled per column (`scale = "uniminmax"`) so hyperparameters on
# very different ranges are still comparable side-by-side; the winning
# combination (`best == TRUE`) is drawn in the "high" accent color, all
# other combinations in muted grey.
plot_cv_grid <- function(cv_result, title = "Hyperparameter Tuning") {
  hp_cols <- setdiff(names(cv_result), "best")
  hp_only_cols <- setdiff(hp_cols, "mean_auc")
  best_row <- cv_result[cv_result$best, , drop = FALSE][1, ]

  cv_result <- cv_result %>%
    dplyr::mutate(best_label = factor(ifelse(best, "Best", "Other"), levels = c("Other", "Best")))

  p <- GGally::ggparcoord(cv_result,
                           columns = which(names(cv_result) %in% hp_cols),
                           groupColumn = which(names(cv_result) == "best_label"),
                           scale = "uniminmax", alphaLines = 0.7, showPoints = TRUE) +
    ggplot2::scale_color_manual(values = c(Other = "grey70", Best = pal_sequential[["high"]])) +
    ggplot2::labs(x = NULL, y = "Scaled Value", color = NULL) +
    report_theme()

  tbl_gt <- best_row[, hp_only_cols, drop = FALSE] %>%
    dplyr::mutate(`Mean AUC` = sprintf("%.3f", best_row$mean_auc)) %>%
    gt::gt() %>%
    gt::tab_header(title = "Winning Configuration") %>%
    gt::tab_options(table.font.size          = gt::px(16),
                     column_labels.font.weight = "bold",
                     table_body.hlines.style        = "hidden",
                     table_body.vlines.style        = "hidden",
                     column_labels.border.top.style = "hidden",
                     column_labels.border.bottom.style = "hidden",
                     table.border.top.style    = "hidden",
                     table.border.bottom.style = "hidden",
                     heading.border.bottom.style = "hidden")

  (p / patchwork::wrap_table(tbl_gt, panel = "full")) +
    patchwork::plot_layout(heights = c(3, 1)) +
    patchwork::plot_annotation(
      title = title,
      theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"))
    )
}
