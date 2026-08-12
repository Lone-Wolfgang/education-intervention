source("R/theme.R")
source("R/colors.R")

# Default cost assumptions, in dollars per student. `intervention_cost` is
# spent on every student the model flags (every TP and every FP);
# `no_action_cost` is what a failure costs when nobody intervenes (every FN,
# and every failure in the do-nothing baseline). A TP therefore nets
# no_action_cost - intervention_cost.
INTERVENTION_COST <- 1000
NO_ACTION_COST    <- 5000

# Compact dollar labels ($120K rather than $120,000) for the color-bar legends,
# which are too narrow for full-length amounts.
dollar_short <- function(x) {
  scales::dollar(x / 1000, accuracy = 1, suffix = "K")
}

# The four confusion-matrix cells with the cash consequence of each, as a tidy
# table: `cost` is what the cell costs the school and `net` is the value it
# creates or destroys — a true positive earns the averted failure minus the
# intervention, a false positive burns an intervention, and a false negative
# eats the full failure cost. Only TN is neutral.
#
# Note that `net` is scored against a world where the failure does not happen,
# not against the do-nothing baseline: relative to doing nothing an FN is a
# wash (the failure was going to cost no_action_cost either way), but charging
# it zero makes a miss look free, which it is not. The scorecard's net benefit
# (net_benefit()) is the one measured against the do-nothing baseline, so these
# cells deliberately do not sum to it.
payoff_table <- function(intervention_cost = INTERVENTION_COST,
                         no_action_cost = NO_ACTION_COST) {
  tibble::tibble(
    cell      = c("TP", "FP", "FN", "TN"),
    actual    = factor(c("Fail", "Pass", "Fail", "Pass"), levels = c("Fail", "Pass")),
    predicted = factor(c("Fail", "Fail", "Pass", "Pass"), levels = c("Pass", "Fail")),
    outcome   = c("Flagged and would have failed: we intervene and the failure is averted",
                  "Flagged but would have passed: the intervention is wasted",
                  "Missed a failing student: no intervention, the failure happens",
                  "Correctly left alone: no spend, no failure"),
    cost      = c(intervention_cost, intervention_cost, no_action_cost, 0),
    net       = c(no_action_cost - intervention_cost, -intervention_cost,
                  -no_action_cost, 0)
  )
}

# Render the payoff table as a gt table.
table_payoff <- function(payoff, intervention_cost = INTERVENTION_COST,
                         no_action_cost = NO_ACTION_COST) {
  payoff %>%
    dplyr::select(cell, actual, predicted, outcome, cost, net) %>%
    gt::gt() %>%
    gt::fmt_currency(c(cost, net), decimals = 0) %>%
    gt::cols_label(cell = "Cell", actual = "Actual", predicted = "Predicted",
                   outcome = "What it means", cost = "Cost",
                   net = "Net value") %>%
    gt::tab_header(
      title = "Cost / Benefit of Each Decision",
      subtitle = sprintf("Intervention %s per flagged student; an unaddressed failure costs %s",
                         scales::dollar(intervention_cost), scales::dollar(no_action_cost))) %>%
    gt::tab_source_note(
      "Net value scores each cell against a student who neither fails nor needs an intervention, so a missed failure is a real loss rather than a wash. Net benefit further down is measured against the do-nothing baseline instead, so these four cells do not sum to it.") %>%
    gt::data_color(columns = net,
                   fn = scales::col_bin(c("firebrick", "white", "#92D050"),
                                        bins = c(-Inf, -0.001, 0.001, Inf)))
}

# Schematic 2x2 confusion matrix colored by the net value of each cell
# (green = value created, red = money lost, grey = neutral). This is the
# decision rule in one picture: only the TP cell pays, so the threshold should
# buy true positives up to the point where the false positives they drag along
# cost more than they are worth.
plot_payoff_matrix <- function(intervention_cost = INTERVENTION_COST,
                               no_action_cost = NO_ACTION_COST) {
  payoff_table(intervention_cost, no_action_cost) %>%
    dplyr::mutate(label = sprintf("%s\n%s\n(cost %s)", cell,
                                  ifelse(net == 0, "no gain", scales::dollar(net)),
                                  scales::dollar(cost))) %>%
    ggplot2::ggplot(ggplot2::aes(x = predicted, y = actual, fill = net)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 5, fontface = "bold",
                       lineheight = 1.0) +
    ggplot2::scale_fill_gradient2(low = pal_divergent[["low"]],
                                  mid = pal_divergent[["mid"]],
                                  high = "#92D050", midpoint = 0,
                                  labels = dollar_short, n.breaks = 4,
                                  name = "Net value") +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = "Payoff Matrix",
                  subtitle = sprintf("Every flag costs %s; every miss costs %s",
                                     scales::dollar(intervention_cost),
                                     scales::dollar(no_action_cost)),
                  x = "Predicted", y = "Actual") +
    report_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

# Score a set of predicted probabilities at one decision threshold in dollars.
# Returns a single row with the confusion counts and the four headline
# quantities:
#   cost_no_action - what every actual failure would cost with no model at all
#   investment     - intervention_cost for every student flagged (TP + FP)
#   loss           - no_action_cost for every failure still missed (FN)
#   net_benefit    - cost_no_action - (investment + loss)
net_benefit <- function(probs, actual, threshold,
                        intervention_cost = INTERVENTION_COST,
                        no_action_cost = NO_ACTION_COST) {
  pred <- as.integer(probs >= threshold)
  tp <- sum(pred == 1 & actual == 1)
  fp <- sum(pred == 1 & actual == 0)
  tn <- sum(pred == 0 & actual == 0)
  fn <- sum(pred == 0 & actual == 1)

  cost_no_action <- sum(actual == 1) * no_action_cost
  investment     <- (tp + fp) * intervention_cost
  loss           <- fn * no_action_cost

  tibble::tibble(
    threshold = threshold,
    n = length(actual), tp = tp, fp = fp, fn = fn, tn = tn,
    n_flagged = tp + fp,
    cost_no_action = cost_no_action,
    investment = investment,
    loss = loss,
    total_cost = investment + loss,
    net_benefit = cost_no_action - (investment + loss),
    net_benefit_per_student = (cost_no_action - (investment + loss)) / length(actual)
  )
}

# The threshold that minimizes expected cost under these assumptions. A student
# is worth flagging when p * no_action_cost (the expected cost of doing nothing)
# exceeds intervention_cost, i.e. when p > intervention_cost / no_action_cost.
# Note this is the cost ratio, not a tuned quantity: it comes from the
# assumptions alone, before seeing any data.
cost_optimal_threshold <- function(intervention_cost = INTERVENTION_COST,
                                   no_action_cost = NO_ACTION_COST) {
  intervention_cost / no_action_cost
}

# Candidate decision rules, each producing one threshold from `probs`/`actual`:
#   argmax     - the textbook 0.5 cut, i.e. predict the more likely class
#   f1         - threshold maximizing F1 (balances precision and recall)
#   youdens_j  - threshold maximizing sensitivity + specificity - 1
#   cost_ratio - intervention_cost / no_action_cost, from the assumptions alone
# Returns a tibble with one row per rule and its net benefit at that threshold,
# sorted by net benefit.
threshold_rules <- function(probs, actual,
                            intervention_cost = INTERVENTION_COST,
                            no_action_cost = NO_ACTION_COST) {
  rules <- list(
    argmax     = 0.5,
    f1         = optimal_threshold(probs, actual, metric = "f1"),
    youdens_j  = optimal_threshold(probs, actual, metric = "youdens_j"),
    cost_ratio = cost_optimal_threshold(intervention_cost, no_action_cost)
  )

  purrr::imap_dfr(rules, function(t, rule) {
    nb <- net_benefit(probs, actual, t, intervention_cost, no_action_cost)
    dplyr::mutate(nb, rule = rule, .before = 1)
  }) %>%
    dplyr::arrange(dplyr::desc(net_benefit))
}

# Render the decision-rule comparison as a gt table, bolding the winner.
table_threshold_rules <- function(rules) {
  best_row <- which.max(rules$net_benefit)

  rules %>%
    dplyr::select(rule, threshold, tp, fp, fn, n_flagged, investment, loss, net_benefit) %>%
    gt::gt() %>%
    gt::fmt_number(threshold, decimals = 3) %>%
    gt::fmt_currency(c(investment, loss, net_benefit), decimals = 0) %>%
    gt::cols_label(rule = "Decision rule", threshold = "Threshold", tp = "TP",
                   fp = "FP", fn = "FN", n_flagged = "Flagged",
                   investment = "Investment", loss = "Loss",
                   net_benefit = "Net benefit") %>%
    gt::tab_header(title = "Decision Rules Compared",
                   subtitle = "Thresholds chosen on validation, ranked by net benefit") %>%
    gt::tab_style(style = gt::cell_text(weight = "bold"),
                  locations = gt::cells_body(rows = best_row))
}

# Net benefit across the whole threshold range, with each candidate rule marked
# on the curve. Shows how much the choice of rule actually matters, and how
# flat or peaked the optimum is.
plot_net_benefit_curve <- function(probs, actual, rules,
                                   intervention_cost = INTERVENTION_COST,
                                   no_action_cost = NO_ACTION_COST,
                                   thresholds_seq = seq(0, 1, 0.01)) {
  curve <- purrr::map_dfr(thresholds_seq, function(t) {
    net_benefit(probs, actual, t, intervention_cost, no_action_cost)
  })

  ggplot2::ggplot(curve, ggplot2::aes(x = threshold, y = net_benefit)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_line(color = pal_sequential[["high"]], linewidth = 1) +
    ggplot2::geom_point(data = rules, ggplot2::aes(color = rule), size = 4) +
    scale_color_cat(name = "Decision rule") +
    ggplot2::scale_y_continuous(labels = scales::dollar) +
    ggplot2::labs(title = "Net Benefit vs. Decision Threshold",
                  subtitle = "Validation set; points mark each candidate rule",
                  x = "Threshold", y = "Net benefit") +
    report_theme()
}

# Confusion matrix in dollars: each cell shows how many students landed there,
# what they cost, and what that spend is worth against the do-nothing baseline.
# The TP and FP cells add up to the program's investment and the FN cell is the
# loss — the same three numbers the scorecard reports — while the color follows
# net value, so the money well spent (TP) reads green and the money wasted (FP)
# reads red. `nb` is one row from net_benefit().
plot_cost_confusion_matrix <- function(nb, intervention_cost = INTERVENTION_COST,
                                       no_action_cost = NO_ACTION_COST,
                                       title = "Confusion Matrix (Dollars)") {
  payoff_table(intervention_cost, no_action_cost) %>%
    dplyr::mutate(
      n       = c(nb$tp, nb$fp, nb$fn, nb$tn),
      spend   = n * cost,
      value   = n * net,
      label   = sprintf("%s\n%d students\ncost %s\nnet %s", cell, n,
                        scales::dollar(spend),
                        ifelse(value == 0, "$0",
                               scales::dollar(value, style_negative = "hyphen")))
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = predicted, y = actual, fill = value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 4.5, fontface = "bold",
                       lineheight = 1.0) +
    ggplot2::scale_fill_gradient2(low = pal_divergent[["low"]],
                                  mid = pal_divergent[["mid"]],
                                  high = "#92D050", midpoint = 0,
                                  labels = dollar_short, n.breaks = 4,
                                  name = "Net value") +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = title,
                  subtitle = sprintf("Threshold = %.3f; total cost %s vs. %s doing nothing",
                                     nb$threshold, scales::dollar(nb$total_cost),
                                     scales::dollar(nb$cost_no_action)),
                  x = "Predicted", y = "Actual") +
    report_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

# The final scorecard: what doing nothing would cost, what the model spends,
# what it still loses, and the difference between the two.
table_business_case <- function(nb, label = "Test set") {
  tibble::tibble(
    line = c("Cost of no action",
             "Investment (interventions)",
             "Loss (missed failures)",
             "Total cost with the model",
             "Net benefit"),
    detail = c(sprintf("%d actual failures x %s", nb$tp + nb$fn,
                       scales::dollar(nb$cost_no_action / max(nb$tp + nb$fn, 1))),
               sprintf("%d students flagged x %s", nb$n_flagged,
                       scales::dollar(nb$investment / max(nb$n_flagged, 1))),
               sprintf("%d failures missed x %s", nb$fn,
                       scales::dollar(nb$loss / max(nb$fn, 1))),
               "Investment + Loss",
               "Cost of no action - Total cost"),
    amount = c(nb$cost_no_action, nb$investment, nb$loss, nb$total_cost, nb$net_benefit)
  ) %>%
    gt::gt() %>%
    gt::fmt_currency(amount, decimals = 0) %>%
    gt::cols_label(line = "", detail = "How it is computed", amount = "Amount") %>%
    gt::tab_header(title = "Business Case",
                   subtitle = sprintf("%s, n = %d students", label, nb$n)) %>%
    gt::tab_style(style = gt::cell_text(weight = "bold"),
                  locations = gt::cells_body(rows = c(4, 5)))
}

# Waterfall view of the same scorecard: start from the cost of doing nothing,
# subtract what the program spends and what it still loses, and land on the net
# benefit.
plot_business_case <- function(nb, title = "Where the Money Goes") {
  steps <- tibble::tibble(
    step   = factor(c("Cost of no action", "Investment", "Loss", "Net benefit"),
                    levels = c("Cost of no action", "Investment", "Loss", "Net benefit")),
    amount = c(nb$cost_no_action, -nb$investment, -nb$loss, nb$net_benefit)
  ) %>%
    dplyr::mutate(
      end   = c(cumsum(amount[1:3]), nb$net_benefit),
      start = c(0, end[1:2], 0),
      kind  = c("Baseline", "Spend", "Spend", "Result")
    )

  ggplot2::ggplot(steps, ggplot2::aes(x = as.integer(step), fill = kind)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = as.integer(step) - 0.4,
                                    xmax = as.integer(step) + 0.4,
                                    ymin = start, ymax = end)) +
    ggplot2::geom_text(ggplot2::aes(y = pmax(start, end),
                                    label = scales::dollar(amount)),
                       vjust = -0.4, size = 4.5, fontface = "bold") +
    ggplot2::scale_x_continuous(breaks = seq_along(levels(steps$step)),
                                labels = levels(steps$step)) +
    ggplot2::scale_fill_manual(values = c(Baseline = pal_sequential[["high"]],
                                          Spend    = pal_divergent[["low"]],
                                          Result   = "#92D050"), name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::dollar,
                                expand = ggplot2::expansion(mult = c(0.02, 0.12))) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    report_theme()
}
