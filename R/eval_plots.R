source("R/theme.R")
source("R/colors.R")

# Row-normalized confusion matrix heatmap: rows are the true (actual)
# class, normalized so each row sums to 1 (cell = P(predicted | actual)),
# visualizing per-class recall. `cm` is the table returned by
# evaluate_predictions()$confusion_matrix (table(predicted = ..., actual = ...)).
plot_confusion_matrix <- function(cm, title = "Confusion Matrix") {
  as.data.frame(cm) %>%
    dplyr::group_by(actual) %>%
    dplyr::mutate(pct = Freq / sum(Freq)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = predicted, y = actual, fill = pct)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(pct, accuracy = 0.1)),
                        size = 6, fontface = "bold") +
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
