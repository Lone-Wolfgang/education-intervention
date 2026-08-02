source("R/theme.R")
source("R/colors.R")

# Bar chart of Pass/Fail counts with percentage labels.
plot_target_balance <- function(df) {
  df %>%
    dplyr::count(Status) %>%
    dplyr::mutate(pct = n / sum(n)) %>%
    ggplot2::ggplot(ggplot2::aes(x = Status, y = n, fill = Status)) +
    ggplot2::geom_col(width = 0.55) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(pct, accuracy = 0.1)),
                       vjust = -0.5, size = 8, fontface = "bold") +
    scale_fill_binary() +
    ggplot2::labs(title = "Target Variable: Pass / Fail", x = NULL, y = "Students") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.2))) +
    report_theme() +
    ggplot2::theme(legend.position = "none")
}

# Faceted histograms showing the distribution of each numeric predictor.
plot_num_distributions <- function(df, num_vars) {
  df %>%
    dplyr::select(dplyr::all_of(num_vars)) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "value") %>%
    ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = 25, fill = pal_sequential[["high"]],
                             color = "white", linewidth = 0.3) +
    ggplot2::facet_wrap(~ variable, scales = "free") +
    ggplot2::labs(title = "Univariate Distributions (Numeric)", x = NULL, y = "Count") +
    report_theme()
}

# Faceted boxplots comparing each numeric predictor across Pass/Fail.
plot_num_by_status <- function(df, num_vars) {
  df %>%
    dplyr::select(Status, dplyr::all_of(num_vars)) %>%
    tidyr::pivot_longer(-Status, names_to = "variable", values_to = "value") %>%
    ggplot2::ggplot(ggplot2::aes(x = Status, y = value, fill = Status)) +
    ggplot2::geom_boxplot(alpha = 0.85, outlier.shape = 21, outlier.size = 2) +
    scale_fill_binary() +
    ggplot2::facet_wrap(~ variable, scales = "free_y") +
    ggplot2::labs(title = "Numeric Predictors vs. Outcome", x = NULL, y = NULL) +
    report_theme() +
    ggplot2::theme(legend.position = "none")
}

# Grid of bar charts: fail rate per level of each categorical predictor,
# sharing one diverging color scale so panels are comparable.
plot_fail_rates_cat <- function(df, cat_vars) {
  known_orders <- list(
    c("Low", "Medium", "High"),
    c("Near", "Moderate", "Far"),
    c("High School", "College", "Postgraduate"),
    c("Negative", "Neutral", "Positive")
  )
  # Restore the natural level order if the values match a known ordinal set.
  apply_order <- function(x) {
    for (ord in known_orders) {
      if (all(x %in% ord)) return(factor(x, levels = ord))
    }
    factor(x)
  }

  # Pooled fail rates across all variables define the shared color limits.
  all_rates <- purrr::map_dfr(cat_vars, function(v) {
    df %>%
      dplyr::group_by(lvl = as.character(.data[[v]])) %>%
      dplyr::summarise(fail_rate = mean(Status == "Fail"), .groups = "drop")
  })
  # Center the diverging scale on the overall fail rate so color encodes
  # deviation from the baseline risk, with symmetric limits so equal
  # deviations above/below the baseline get equal color intensity.
  rate_mid <- mean(df$Status == "Fail")
  max_dev  <- max(abs(all_rates$fail_rate - rate_mid))
  rate_min <- rate_mid - max_dev
  rate_max <- rate_mid + max_dev

  plots <- purrr::map(cat_vars, function(v) {
    df %>%
      dplyr::group_by(lvl = as.character(.data[[v]])) %>%
      dplyr::summarise(fail_rate = mean(Status == "Fail"), .groups = "drop") %>%
      dplyr::mutate(lvl = apply_order(lvl)) %>%
      ggplot2::ggplot(ggplot2::aes(x = lvl, y = fail_rate, fill = fail_rate)) +
      ggplot2::geom_col() +
      ggplot2::geom_text(ggplot2::aes(label = scales::percent(fail_rate, accuracy = 0.1)),
                         vjust = -0.5, size = 5, fontface = "bold") +
      ggplot2::scale_fill_gradient2(low  = pal_divergent[["high"]],
                                     mid  = pal_divergent[["mid"]],
                                     high = pal_divergent[["low"]],
                                     midpoint = rate_mid,
                                     limits   = c(rate_min, rate_max),
                                     name     = "fail %") +
      ggplot2::scale_y_continuous(labels = scales::percent,
                                  expand = ggplot2::expansion(mult = c(0, 0.35))) +
      ggplot2::labs(title = v, x = NULL, y = NULL) +
      report_theme() +
      ggplot2::theme(legend.position = "none",
                     plot.title    = ggplot2::element_text(size = 10),
                     axis.text.x   = ggplot2::element_text(angle = 35, hjust = 1))
  })
  patchwork::wrap_plots(plots, ncol = 3)
}

# Heatmap of the Pearson correlation matrix for the numeric predictors.
plot_correlation <- function(df, num_vars) {
  cor_mat <- cor(df[, num_vars], use = "complete.obs")
  as.data.frame(cor_mat) %>%
    tibble::rownames_to_column("var1") %>%
    tidyr::pivot_longer(-var1, names_to = "var2", values_to = "r") %>%
    ggplot2::ggplot(ggplot2::aes(x = var2, y = var1, fill = r)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = round(r, 2)), size = 6, fontface = "bold") +
    scale_fill_div(midpoint = 0, limits = c(-1, 1), name = "r") +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = "Pearson Correlation Matrix", x = NULL, y = NULL) +
    report_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   panel.grid  = ggplot2::element_blank())
}

# Coefficient plot of log-odds with 95% CIs from a fitted glm (intercept dropped).
plot_logodds <- function(fit) {
  broom::tidy(fit, conf.int = TRUE) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(term = forcats::fct_reorder(term, estimate)) %>%
    ggplot2::ggplot(ggplot2::aes(x = estimate, xmin = conf.low, xmax = conf.high,
                                  y = term, color = estimate)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1,
                         color = "grey50") +
    ggplot2::geom_pointrange(linewidth = 0.9, fatten = 4) +
    scale_color_div(midpoint = 0, name = "log-odds") +
    ggplot2::labs(title = "Log-Odds of Failing", subtitle = "Positive = higher failure risk",
                  x = "Log-Odds (95% CI)", y = NULL) +
    report_theme()
}
