# Directory every gt table PNG export lands in (created on first use).
TABLE_PNG_DIR <- "tables"

# Render a gt table to a PNG in `dir` (default `TABLE_PNG_DIR`) via
# gt::gtsave()/webshot2, and return `gt_obj` unchanged so this can sit at the
# end of a pipe (`table_foo(...) %>% save_table_png("foo")`) without
# disturbing the chunk's normal auto-printed output.
save_table_png <- function(gt_obj, name, dir = TABLE_PNG_DIR) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  gt::gtsave(gt_obj, filename = file.path(dir, paste0(name, ".png")))
  gt_obj
}

# gt table of Pass/Fail counts and shares. `cutoff` is optional and only
# used to label the title with the threshold that produced this balance.
table_class_balance <- function(df, cutoff = NULL) {
  title <- if (is.null(cutoff)) "Class Balance" else sprintf("Class Balance (cutoff = %s)", cutoff)
  df %>%
    dplyr::count(Status) %>%
    dplyr::mutate(pct = n / sum(n)) %>%
    gt::gt() %>%
    gt::fmt_percent(pct, decimals = 1) %>%
    gt::tab_header(title = title)
}

# Welch t-test of each numeric predictor between Fail and Pass groups;
# returns group means, their difference, and the p-value, sorted by p.
ttest_summary <- function(df, num_vars) {
  purrr::map_dfr(num_vars, function(v) {
    f  <- df %>% dplyr::filter(Status == "Fail") %>% dplyr::pull(v)
    p  <- df %>% dplyr::filter(Status == "Pass") %>% dplyr::pull(v)
    tt <- t.test(f, p)
    tibble::tibble(variable  = v,
                   mean_fail = mean(f),
                   mean_pass = mean(p),
                   diff      = mean(f) - mean(p),
                   p_value   = tt$p.value)
  }) %>% dplyr::arrange(p_value)
}

# Render the t-test summary as a formatted gt table.
table_ttest <- function(smd) {
  smd %>%
    gt::gt() %>%
    gt::fmt_number(c(mean_fail, mean_pass, diff), decimals = 2) %>%
    gt::fmt_scientific(p_value, decimals = 2) %>%
    gt::tab_header(title = "Numeric predictors vs. Failing (Welch t-test)")
}

# Long table of n and fail rate for every level of every categorical predictor.
fail_rate_summary <- function(df, cat_vars) {
  purrr::map_dfr(cat_vars, function(v) {
    df %>%
      dplyr::group_by(level = as.character(.data[[v]])) %>%
      dplyr::summarise(n = dplyr::n(),
                       fail_rate = mean(Status == "Fail"),
                       .groups = "drop") %>%
      dplyr::mutate(variable = v)
  })
}

# Render fail rates as a gt table grouped by variable.
table_fail_rates <- function(fail_rates) {
  fail_rates %>%
    dplyr::select(variable, level, n, fail_rate) %>%
    gt::gt(groupname_col = "variable") %>%
    gt::fmt_percent(fail_rate, decimals = 1) %>%
    gt::tab_header(title = "Fail rate by category level")
}

# For each level of each variable in `cat_vars`, compute the standardized
# gap in `num_var` between Pass and Fail (mean difference / pooled sd) —
# used to screen categorical variables for candidate interaction terms
# with `num_var`.
interaction_gap_summary <- function(df, num_var, cat_vars) {
  gap_check <- function(catvar) {
    df %>%
      dplyr::group_by(level = as.character(.data[[catvar]])) %>%
      dplyr::summarise(
        mean_fail = mean(.data[[num_var]][Status == "Fail"]),
        mean_pass = mean(.data[[num_var]][Status == "Pass"]),
        gap       = mean_pass - mean_fail,
        sd_pooled = sd(.data[[num_var]]),
        std_gap   = gap / sd_pooled,
        n         = dplyr::n(),
        .groups   = "drop"
      ) %>%
      dplyr::mutate(variable = catvar, .before = 1)
  }
  purrr::map_dfr(cat_vars, gap_check)
}

# Chi-square test of independence between each categorical predictor and
# Status; returns statistic, df, and p-value sorted by p.
chisq_summary <- function(df, cat_vars) {
  purrr::map_dfr(cat_vars, function(v) {
    ct <- suppressWarnings(chisq.test(table(df[[v]], df$Status)))
    tibble::tibble(variable  = v,
                   statistic = ct$statistic,
                   df        = ct$parameter,
                   p_value   = ct$p.value)
  }) %>% dplyr::arrange(p_value)
}

# Render the chi-square results as a formatted gt table.
table_chisq <- function(chi_res) {
  chi_res %>%
    gt::gt() %>%
    gt::fmt_number(statistic, decimals = 2) %>%
    gt::fmt_scientific(p_value, decimals = 2) %>%
    gt::tab_header(title = "Categorical predictors vs. Failing (Chi-square)")
}

# Render the interaction screen (output of screen_interactions()) as a gt
# table, showing only the `top_n` most promising candidates and flagging any
# that survive the BH correction.
table_interaction_lrt <- function(screen, top_n = 15, alpha = 0.05) {
  n_signif <- sum(screen$p_adj < alpha, na.rm = TRUE)
  subtitle <- sprintf("%d of %d candidate terms significant at BH-adjusted p < %.2f",
                      n_signif, nrow(screen), alpha)

  screen %>%
    utils::head(top_n) %>%
    gt::gt() %>%
    gt::fmt_number(lrt, decimals = 2) %>%
    gt::fmt_number(c(p_value, p_adj), decimals = 4) %>%
    gt::cols_label(term = "Interaction", added_df = "df", lrt = "LRT",
                   p_value = "p", p_adj = "p (BH)") %>%
    gt::tab_header(title    = "Two-way interaction screen (likelihood-ratio tests)",
                   subtitle = subtitle) %>%
    gt::data_color(columns = p_adj,
                   fn = scales::col_bin(c("firebrick", "white"), bins = c(0, alpha, 1)))
}

# gt table of model coefficients with odds ratios and CIs,
# highlighting p-values below 0.05.
table_coef <- function(fit) {
  broom::tidy(fit, conf.int = TRUE) %>%
    dplyr::mutate(odds_ratio = exp(estimate),
                  or_low     = exp(conf.low),
                  or_high    = exp(conf.high)) %>%
    dplyr::select(term, estimate, std.error, statistic, p.value,
                  odds_ratio, or_low, or_high) %>%
    gt::gt() %>%
    gt::fmt_number(c(estimate, std.error, statistic,
                     odds_ratio, or_low, or_high), decimals = 3) %>%
    gt::fmt_scientific(p.value, decimals = 2) %>%
    gt::cols_label(estimate = "log-odds", std.error = "SE", statistic = "z",
                   p.value  = "p", odds_ratio = "OR",
                   or_low   = "OR 2.5%", or_high = "OR 97.5%") %>%
    gt::tab_header(title    = "Logistic Regression Coefficients",
                   subtitle = "Outcome = Fail") %>%
    gt::data_color(columns = p.value,
                   fn = scales::col_bin(c("firebrick", "white"), bins = c(0, 0.05, 1)))
}

# gt table of overall model fit: deviances, AIC, and McFadden pseudo-R^2.
table_fit_quality <- function(fit) {
  tibble::tibble(null_deviance  = fit$null.deviance,
                 resid_deviance = fit$deviance,
                 AIC            = fit$aic,
                 pseudo_R2      = 1 - fit$deviance / fit$null.deviance) %>%
    gt::gt() %>%
    gt::fmt_number(dplyr::everything(), decimals = 2) %>%
    gt::tab_header(title = "Model fit (McFadden pseudo-R\u00b2)")
}

# gt table of the tuning outcome (output of cv_best_summary()): the winning
# hyperparameters per model, the size of the grid they were chosen from, and
# the winner's mean CV AUC with its fold-to-fold SD. The row with the highest
# mean CV AUC is bolded.
table_cv_best <- function(cv_summary) {
  best_row <- which.max(cv_summary$mean_auc)

  cv_summary %>%
    gt::gt() %>%
    gt::fmt_number(c(mean_auc, sd_auc), decimals = 4) %>%
    gt::cols_label(model = "Model", n_configs = "Configs searched",
                   hyperparameters = "Selected hyperparameters",
                   mean_auc = "Mean CV AUC", sd_auc = "SD across folds") %>%
    gt::tab_header(title    = "Hyperparameter Tuning Summary",
                   subtitle = "Stratified 5-fold CV on the training split") %>%
    gt::tab_style(style     = gt::cell_text(weight = "bold"),
                  locations = gt::cells_body(rows = best_row))
}

# gt table comparing models (output of compare_models()): Model,
# Threshold, AUC, Accuracy, Precision, Sensitivity, Specificity, F1,
# Balanced Accuracy, Youden's J, Prevalence — the row with the highest
# F1 is bolded.
table_model_comparison <- function(comparison) {
  best_row <- which(comparison$f1 == max(comparison$f1, na.rm = TRUE))

  comparison %>%
    gt::gt() %>%
    gt::fmt_number(c(threshold, auc, accuracy, precision, sensitivity,
                     specificity, f1, balanced_accuracy, youdens_j, prevalence),
                   decimals = 3) %>%
    gt::cols_label(model = "Model", threshold = "Threshold", auc = "AUC",
                   accuracy = "Accuracy", precision = "Precision",
                   sensitivity = "Sensitivity", specificity = "Specificity",
                   f1 = "F1", balanced_accuracy = "Balanced Accuracy",
                   youdens_j = "Youden's J", prevalence = "Prevalence") %>%
    gt::tab_header(title = "Model Comparison") %>%
    gt::tab_style(style     = gt::cell_text(weight = "bold"),
                   locations = gt::cells_body(rows = best_row))
}

# gt table comparing models at each one's own best decision rule (output of
# best_rule_per_model()): Model, Decision Rule, Threshold, TP, FP, FN,
# Flagged, Investment, Loss, Net Benefit — the row with the highest net
# benefit is bolded.
table_model_rule_comparison <- function(comparison) {
  best_row <- which.max(comparison$net_benefit)

  comparison %>%
    dplyr::select(model, rule, threshold, tp, fp, fn, n_flagged,
                  investment, loss, net_benefit) %>%
    gt::gt() %>%
    gt::fmt_number(threshold, decimals = 3) %>%
    gt::fmt_currency(c(investment, loss, net_benefit), decimals = 0) %>%
    gt::cols_label(model = "Model", rule = "Decision Rule",
                   threshold = "Threshold", tp = "TP", fp = "FP", fn = "FN",
                   n_flagged = "Flagged", investment = "Investment",
                   loss = "Loss", net_benefit = "Net Benefit") %>%
    gt::tab_header(title = "Models Compared at Their Optimal Decision Rule",
                   subtitle = "Each model's best rule on validation, ranked by net benefit") %>%
    gt::tab_style(style = gt::cell_text(weight = "bold"),
                  locations = gt::cells_body(rows = best_row))
}
