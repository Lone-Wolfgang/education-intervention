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
plot_threshold_tuning <- function(probs, actual,
                                   metrics = c("sensitivity", "specificity"),
                                   target = c("youdens_j", "f1"),
                                   thresholds_seq = seq(0, 1, 0.01)) {
  metrics <- match.arg(metrics, c("precision", "sensitivity", "specificity"),
                        several.ok = TRUE)
  target <- match.arg(target)

  metric_curve <- purrr::map_dfr(thresholds_seq, function(t) {
    ev <- evaluate_predictions(probs, actual, threshold = t)
    tibble::tibble(threshold = t,
                    precision = ev$summary$precision,
                    sensitivity = ev$summary$recall,
                    specificity = ev$summary$specificity)
  }) %>%
    tidyr::pivot_longer(dplyr::all_of(metrics), names_to = "metric", values_to = "value")

  best_threshold <- optimal_threshold(probs, actual, metric = target,
                                       thresholds = thresholds_seq)

  n_metrics <- length(metrics)
  palette <- pal_categorical[seq_len(n_metrics)]

  target_label <- if (target == "youdens_j") "Youden's J" else "F1"

  metric_curve %>%
    ggplot2::ggplot(ggplot2::aes(x = threshold, y = value, color = metric)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_vline(xintercept = best_threshold, linetype = "dashed", color = "grey30") +
    ggplot2::annotate("text", x = best_threshold, y = 1.02,
                       label = sprintf("%.2f", best_threshold),
                       hjust = -0.1, vjust = 0, size = 4, fontface = "bold") +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
    ggplot2::labs(title = "Decision-Threshold Tuning",
                  subtitle = paste0("Optimal threshold maximizes ", target_label),
                  x = "Threshold", y = "Value", color = NULL) +
    report_theme()
}
