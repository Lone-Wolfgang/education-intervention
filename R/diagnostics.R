source("R/theme.R")
source("R/colors.R")

# ── Logistic regression assumptions ───────────────────────────────────────────

# Empirical-logit plot: split each numeric predictor into `bins` quantile
# groups and plot the observed log-odds of failing in each bin against the bin
# mean, with a least-squares reference line. Logistic regression assumes the
# logit is linear in each numeric predictor, so points that track the line
# support the assumption and systematic curvature argues for a transform or a
# polynomial term. Bins use the Haldane-Anscombe 0.5 correction so bins with
# zero failures still plot.
plot_empirical_logit <- function(df, num_vars, bins = 10) {
  purrr::map_dfr(num_vars, function(v) {
    df %>%
      dplyr::mutate(bin = dplyr::ntile(.data[[v]], bins)) %>%
      dplyr::group_by(bin) %>%
      dplyr::summarise(x = mean(.data[[v]]),
                       n = dplyr::n(),
                       fails = sum(Status == "Fail"),
                       .groups = "drop") %>%
      dplyr::mutate(emp_logit = log((fails + 0.5) / (n - fails + 0.5)),
                    variable  = v)
  }) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = emp_logit)) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
                         color = "grey50", linewidth = 0.8) +
    ggplot2::geom_point(color = pal_sequential[["high"]], size = 2.5) +
    ggplot2::facet_wrap(~ variable, scales = "free") +
    ggplot2::labs(title = "Empirical Logit vs. Numeric Predictors",
                  subtitle = "Straight line = logit is linear in the predictor",
                  x = "Bin mean", y = "log odds(Fail)") +
    report_theme()
}

# Binned residual plot: sort the fitted probabilities, split them into `bins`
# equal-count groups, and plot each bin's mean response residual against its
# mean fitted probability inside a +/- 2 SE band. Raw logistic residuals are
# uninformative one point at a time; binned means should scatter around zero
# and stay inside the band.
plot_binned_residuals <- function(fit, bins = 40) {
  tibble::tibble(fitted = stats::fitted(fit),
                 resid  = stats::residuals(fit, type = "response")) %>%
    dplyr::mutate(bin = dplyr::ntile(fitted, bins)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(fitted    = mean(fitted),
                     mean_res  = mean(resid),
                     se        = 2 * stats::sd(resid) / sqrt(dplyr::n()),
                     .groups   = "drop") %>%
    ggplot2::ggplot(ggplot2::aes(x = fitted, y = mean_res)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = -se, ymax = se), width = 0.01,
                           color = pal_sequential[["high"]], alpha = 0.5) +
    ggplot2::geom_point(color = pal_sequential[["high"]], size = 2.5) +
    ggplot2::labs(title = "Binned Residuals vs. Fitted Probability",
                  subtitle = "Bars = +/- 2 SE for the bin; points should sit inside them",
                  x = "Mean predicted P(Fail)", y = "Mean residual") +
    report_theme()
}

# Generalized VIF for a fitted glm (car::vif). For multi-df terms the
# comparable quantity is gvif_adj^2 = GVIF^(1/(2*Df))^2, which is on the same
# scale as an ordinary VIF, so the usual VIF > 5 rule of thumb applies to
# `vif_equiv`.
vif_summary <- function(fit) {
  v <- car::vif(fit)
  out <- if (is.matrix(v)) {
    tibble::tibble(term = rownames(v), gvif = v[, "GVIF"], df = v[, "Df"],
                   gvif_adj = v[, 3])
  } else {
    tibble::tibble(term = names(v), gvif = as.numeric(v), df = 1,
                   gvif_adj = sqrt(as.numeric(v)))
  }
  out %>%
    dplyr::mutate(vif_equiv = gvif_adj^2) %>%
    dplyr::arrange(dplyr::desc(vif_equiv))
}

# Render the VIF summary as a gt table, flagging terms above `threshold`
# on the ordinary-VIF scale.
table_vif <- function(v, threshold = 5) {
  v %>%
    gt::gt() %>%
    gt::fmt_number(c(gvif, gvif_adj, vif_equiv), decimals = 2) %>%
    gt::cols_label(term = "Term", gvif = "GVIF", df = "df",
                   gvif_adj = "GVIF^(1/2df)", vif_equiv = "VIF equivalent") %>%
    gt::tab_header(title = "Multicollinearity (generalized VIF)",
                   subtitle = sprintf("VIF equivalent > %g flags collinearity", threshold)) %>%
    gt::data_color(columns = vif_equiv,
                   fn = scales::col_bin(c("white", "firebrick"),
                                        bins = c(0, threshold, Inf)))
}

# Per-observation influence diagnostics from broom::augment(): the `top_n`
# rows with the largest Cook's distance, alongside leverage and standardized
# residual. 4/n is the conventional screening cut-off for Cook's D.
influence_summary <- function(fit, top_n = 10) {
  broom::augment(fit) %>%
    dplyr::mutate(obs = dplyr::row_number()) %>%
    dplyr::select(obs, cooks_d = .cooksd, leverage = .hat,
                  std_resid = .std.resid, fitted = .fitted) %>%
    dplyr::arrange(dplyr::desc(cooks_d)) %>%
    utils::head(top_n)
}

# Render the influence summary as a gt table; `n_obs` is used to label the
# 4/n cut-off in the subtitle.
table_influence <- function(infl, n_obs) {
  infl %>%
    gt::gt() %>%
    gt::fmt_number(c(cooks_d, leverage, std_resid, fitted), decimals = 3) %>%
    gt::cols_label(obs = "Row", cooks_d = "Cook's D", leverage = "Leverage",
                   std_resid = "Std. residual", fitted = "Fitted (logit)") %>%
    gt::tab_header(title = "Most influential observations",
                   subtitle = sprintf("Screening cut-off 4/n = %.4f", 4 / n_obs))
}

# Index plot of Cook's distance with the 4/n reference line; a handful of
# points above the line is normal, a few points dominating the rest is not.
plot_influence <- function(fit) {
  aug <- broom::augment(fit) %>% dplyr::mutate(obs = dplyr::row_number())
  cutoff <- 4 / nrow(aug)

  ggplot2::ggplot(aug, ggplot2::aes(x = obs, y = .cooksd)) +
    ggplot2::geom_hline(yintercept = cutoff, linetype = "dashed", color = pal_binary[[2]]) +
    ggplot2::geom_point(color = pal_sequential[["high"]], size = 1.2, alpha = 0.7) +
    ggplot2::labs(title = "Cook's Distance by Observation",
                  subtitle = sprintf("Dashed line = 4/n = %.4f", cutoff),
                  x = "Observation", y = "Cook's D") +
    report_theme()
}

# Separation check. Logistic MLEs only exist when the classes overlap; under
# near-complete separation the fitted probabilities pile up at 0/1 and the
# coefficients and their SEs both explode. Reports the share of fitted
# probabilities within `eps` of 0 or 1 plus the largest |coefficient| and SE.
separation_summary <- function(fit, eps = 0.001) {
  p  <- stats::fitted(fit)
  cf <- summary(fit)$coefficients
  tibble::tibble(
    pct_extreme_fitted = mean(p < eps | p > 1 - eps),
    max_abs_coef       = max(abs(cf[, "Estimate"])),
    max_std_error      = max(cf[, "Std. Error"])
  )
}

# Render the separation check as a gt table.
table_separation <- function(sep, eps = 0.001) {
  sep %>%
    gt::gt() %>%
    gt::fmt_percent(pct_extreme_fitted, decimals = 1) %>%
    gt::fmt_number(c(max_abs_coef, max_std_error), decimals = 2) %>%
    gt::cols_label(pct_extreme_fitted = sprintf("Fitted p within %g of 0/1", eps),
                   max_abs_coef = "Largest |coefficient|",
                   max_std_error = "Largest SE") %>%
    gt::tab_header(title = "Separation check")
}

# ── LDA / QDA assumptions ─────────────────────────────────────────────────────

# Within-class densities of each numeric predictor. LDA and QDA both assume
# the predictors are multivariate normal within each class, so each panel
# should show two roughly bell-shaped curves.
plot_within_class_density <- function(df, num_vars) {
  df %>%
    dplyr::select(dplyr::all_of(num_vars), Status) %>%
    tidyr::pivot_longer(-Status, names_to = "variable", values_to = "value") %>%
    ggplot2::ggplot(ggplot2::aes(x = value, fill = Status)) +
    ggplot2::geom_density(alpha = 0.5) +
    scale_fill_binary() +
    ggplot2::facet_wrap(~ variable, scales = "free") +
    ggplot2::labs(title = "Within-Class Densities (Numeric Predictors)",
                  subtitle = "LDA/QDA assume approximate within-class normality",
                  x = NULL, y = "Density") +
    report_theme()
}

# Normal QQ plots of each numeric predictor within each class — the sharper
# version of the density check, since deviations in the tails are easier to
# read off a QQ line than off a density curve.
plot_within_class_qq <- function(df, num_vars) {
  df %>%
    dplyr::select(dplyr::all_of(num_vars), Status) %>%
    tidyr::pivot_longer(-Status, names_to = "variable", values_to = "value") %>%
    ggplot2::ggplot(ggplot2::aes(sample = value, color = Status)) +
    ggplot2::stat_qq(size = 0.8, alpha = 0.6) +
    ggplot2::stat_qq_line(color = "grey30", linetype = "dashed") +
    scale_color_binary() +
    ggplot2::facet_wrap(~ Status + variable, scales = "free",
                        ncol = length(num_vars)) +
    ggplot2::labs(title = "Within-Class Normal QQ Plots",
                  x = "Theoretical quantile", y = "Sample quantile") +
    report_theme() +
    ggplot2::theme(legend.position = "none")
}

# Shapiro-Wilk normality test of each numeric predictor within each class.
# shapiro.test() caps out at 5000 observations, so larger classes are
# subsampled (seeded for reproducibility).
normality_summary <- function(df, num_vars, max_n = 5000, seed = 42) {
  withr_sample <- function(x) {
    if (length(x) <= max_n) return(x)
    set.seed(seed)
    sample(x, max_n)
  }
  purrr::map_dfr(num_vars, function(v) {
    purrr::map_dfr(levels(df$Status), function(lvl) {
      x <- withr_sample(df[[v]][df$Status == lvl])
      tt <- stats::shapiro.test(x)
      tibble::tibble(variable = v, class = lvl, n = length(x),
                     w = unname(tt$statistic), p_value = tt$p.value)
    })
  })
}

# Render the Shapiro-Wilk results as a gt table grouped by predictor.
table_normality <- function(norm_res, alpha = 0.05) {
  norm_res %>%
    gt::gt(groupname_col = "variable") %>%
    gt::fmt_number(w, decimals = 3) %>%
    gt::fmt_scientific(p_value, decimals = 2) %>%
    gt::cols_label(class = "Class", n = "n", w = "W", p_value = "p") %>%
    gt::tab_header(title = "Within-class normality (Shapiro-Wilk)",
                   subtitle = sprintf("p < %g rejects normality", alpha)) %>%
    gt::data_color(columns = p_value,
                   fn = scales::col_bin(c("firebrick", "white"), bins = c(0, alpha, 1)))
}

# Levene's test of equal variance across classes for each numeric predictor,
# reported alongside the within-class SDs and their ratio. LDA assumes a
# common covariance matrix; QDA drops that assumption.
levene_summary <- function(df, num_vars) {
  purrr::map_dfr(num_vars, function(v) {
    tt <- car::leveneTest(df[[v]] ~ df$Status)
    sd_fail <- stats::sd(df[[v]][df$Status == "Fail"])
    sd_pass <- stats::sd(df[[v]][df$Status == "Pass"])
    tibble::tibble(variable = v, sd_fail = sd_fail, sd_pass = sd_pass,
                   sd_ratio = sd_fail / sd_pass,
                   f_value = tt$`F value`[1], p_value = tt$`Pr(>F)`[1])
  }) %>% dplyr::arrange(p_value)
}

# Render Levene's test results as a gt table.
table_levene <- function(lev_res, alpha = 0.05) {
  lev_res %>%
    gt::gt() %>%
    gt::fmt_number(c(sd_fail, sd_pass, sd_ratio, f_value), decimals = 2) %>%
    gt::fmt_scientific(p_value, decimals = 2) %>%
    gt::cols_label(variable = "Variable", sd_fail = "SD (Fail)",
                   sd_pass = "SD (Pass)", sd_ratio = "SD ratio",
                   f_value = "F", p_value = "p") %>%
    gt::tab_header(title = "Equal-variance check (Levene's test)",
                   subtitle = sprintf("p < %g rejects equal variance", alpha)) %>%
    gt::data_color(columns = p_value,
                   fn = scales::col_bin(c("firebrick", "white"), bins = c(0, alpha, 1)))
}

# Box's M test of equal covariance matrices across classes — the multivariate
# version of the Levene check, and the assumption that separates LDA (common
# covariance) from QDA (per-class covariance). Uses the chi-square
# approximation to M with Box's small-sample correction. Implemented directly
# rather than via heplots so the report keeps its current dependency set.
boxm_test <- function(df, num_vars) {
  x      <- as.matrix(df[, num_vars, drop = FALSE])
  groups <- df$Status
  p      <- ncol(x)
  g      <- nlevels(groups)

  n_i <- as.integer(table(groups))
  cov_i <- lapply(levels(groups), function(lvl) stats::cov(x[groups == lvl, , drop = FALSE]))
  pooled <- Reduce(`+`, Map(function(S, n) (n - 1) * S, cov_i, n_i)) / (sum(n_i) - g)

  m <- sum(mapply(function(S, n) (n - 1) * (determinant(pooled, logarithm = TRUE)$modulus -
                                             determinant(S, logarithm = TRUE)$modulus),
                  cov_i, n_i))
  c1 <- (sum(1 / (n_i - 1)) - 1 / sum(n_i - 1)) *
    (2 * p^2 + 3 * p - 1) / (6 * (p + 1) * (g - 1))
  statistic <- as.numeric(m) * (1 - c1)
  dof <- (g - 1) * p * (p + 1) / 2

  tibble::tibble(chi_sq = statistic, df = dof,
                 p_value = stats::pchisq(statistic, dof, lower.tail = FALSE))
}

# Render Box's M as a gt table.
table_boxm <- function(boxm, alpha = 0.05) {
  boxm %>%
    gt::gt() %>%
    gt::fmt_number(chi_sq, decimals = 2) %>%
    gt::fmt_scientific(p_value, decimals = 2) %>%
    gt::cols_label(chi_sq = "Chi-square", df = "df", p_value = "p") %>%
    gt::tab_header(title = "Equal-covariance check (Box's M)",
                   subtitle = sprintf("p < %g rejects a common covariance matrix", alpha))
}

# Side-by-side correlation heatmaps of the numeric predictors within each
# class: shows *where* the covariance structures differ, which the single
# Box's M p-value cannot.
plot_class_correlations <- function(df, num_vars) {
  purrr::map_dfr(levels(df$Status), function(lvl) {
    cor_mat <- stats::cor(df[df$Status == lvl, num_vars, drop = FALSE])
    as.data.frame(cor_mat) %>%
      tibble::rownames_to_column("var1") %>%
      tidyr::pivot_longer(-var1, names_to = "var2", values_to = "r") %>%
      dplyr::mutate(Status = lvl)
  }) %>%
    dplyr::mutate(Status = factor(Status, levels = levels(df$Status))) %>%
    ggplot2::ggplot(ggplot2::aes(x = var2, y = var1, fill = r)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = round(r, 2)), size = 3) +
    scale_fill_div(midpoint = 0, limits = c(-1, 1), name = "r") +
    ggplot2::facet_wrap(~ Status) +
    ggplot2::labs(title = "Within-Class Correlation Structure",
                  subtitle = "LDA assumes these two panels match",
                  x = NULL, y = NULL) +
    report_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   panel.grid  = ggplot2::element_blank())
}
