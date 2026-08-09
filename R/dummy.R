# Dummy-result generator used by Modeling_dev.Rmd to emulate the shape of
# real model output (predicted probabilities + 0/1 labels) so plotting and
# table code (R/eval.R, R/eval_plots.R, R/tables.R) can be iterated on
# without re-running expensive model fitting/tuning.

# Simulate predicted probabilities for one "model": draws a latent score
# from N(mu1, sd) for actual = 1 (Fail) and N(mu0, sd) for actual = 0
# (Pass), then squashes through plogis() to land in (0, 1). Larger
# `separation` (mu1 - mu0) yields a better-discriminating (higher-AUC)
# model; `noise` (sd) controls how ragged the curve looks.
simulate_probs <- function(actual, separation = 1.5, noise = 1, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- length(actual)
  mu <- ifelse(actual == 1, separation / 2, -separation / 2)
  score <- rnorm(n, mean = mu, sd = noise)
  plogis(score)
}

# Build a full dummy results bundle mirroring the objects produced by the
# real training pipeline in Modeling.Rmd (val_probs/actual_val,
# test_probs/actual_test, thresholds, best_hp) for a given set of model
# names. `quality` is an optional named list/vector of per-model
# separation values (higher = better model); models not listed fall back
# to `default_separation` with a small amount of jitter so curves aren't
# identical. `prevalence` controls the simulated Fail rate.
generate_dummy_results <- function(model_names,
                                    n_val = 200, n_test = 200,
                                    prevalence = 0.3,
                                    quality = NULL,
                                    default_separation = 1.5,
                                    seed = 42) {
  set.seed(seed)

  actual_val  <- rbinom(n_val, 1, prevalence)
  actual_test <- rbinom(n_test, 1, prevalence)

  sep_for <- function(name) {
    if (!is.null(quality) && name %in% names(quality)) {
      quality[[name]]
    } else {
      default_separation + stats::rnorm(1, 0, 0.3)
    }
  }

  val_probs  <- list()
  test_probs <- list()
  best_hp    <- list()

  for (name in model_names) {
    sep <- sep_for(name)
    val_probs[[name]]  <- simulate_probs(actual_val,  separation = sep)
    test_probs[[name]] <- simulate_probs(actual_test, separation = sep)
    best_hp[[name]] <- list(dummy_param = round(sep, 2))
  }

  thresholds <- purrr::map_dbl(val_probs, ~ optimal_threshold(.x, actual_val, metric = "youdens_j"))

  list(
    actual_val  = actual_val,
    actual_test = actual_test,
    val_probs   = val_probs,
    test_probs  = test_probs,
    thresholds  = thresholds,
    best_hp     = best_hp
  )
}

# Simulate a feature-importance tibble(feature, importance) for one
# model: draws a random positive score per feature from Exp(1) (heavier
# right tail than uniform, closer to how real importance scores look —
# a few dominant features, many small ones), sorted descending.
simulate_importance <- function(feature_names, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  scores <- stats::rexp(length(feature_names), rate = 1)
  tibble::tibble(feature = feature_names, importance = scores) %>%
    dplyr::arrange(dplyr::desc(importance))
}

# Build dummy feature-importance results for the models that have a
# native importance metric (tree, rf, xgboost — see R/importance.R).
# `feature_names` defaults to a representative mock predictor list
# (mirrors num_vars/cat_vars in Modeling.Rmd) since no real fitted
# models are available in the dev sandbox.
generate_dummy_importance <- function(model_names = c("tree", "rf", "xgboost"),
                                       feature_names = c(
                                         "Hours_Studied", "Attendance", "Sleep_Hours",
                                         "Previous_Scores", "Tutoring_Sessions",
                                         "Physical_Activity", "Parental_Involvement",
                                         "Motivation_Level", "Access_to_Resources",
                                         "Extracurricular_Activities"
                                       ),
                                       seed = 42) {
  set.seed(seed)
  purrr::map(model_names, function(name) {
    simulate_importance(feature_names, seed = seed + which(model_names == name))
  }) %>% purrr::set_names(model_names)
}

# Simulate a CV grid-search result for one model: expands `search_space`
# (a named list of hyperparameter vectors, same shape as
# model_specs[[name]]$search_space in Modeling.Rmd) via expand.grid(),
# then assigns a simulated `mean_auc` per combination — a base AUC plus
# a small random-direction linear trend per numeric hyperparameter (so
# some regions of the grid look clearly better) plus noise, clipped to
# (0, 1). Flags the row(s) with the highest mean_auc as `best`, matching
# the shape returned by cv_grid_search()/cv_select_knn_k() in R/tune.R.
simulate_cv_grid <- function(search_space, base_auc = 0.75, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  grid <- expand.grid(search_space, stringsAsFactors = FALSE)

  score <- rep(base_auc, nrow(grid))
  for (col in names(search_space)) {
    vals <- grid[[col]]
    if (is.numeric(vals)) {
      rng <- range(vals)
      norm <- if (diff(rng) == 0) rep(0.5, length(vals)) else (vals - rng[1]) / diff(rng)
      weight <- stats::runif(1, -0.12, 0.15)
      score <- score + weight * norm
    }
  }
  score <- score + stats::rnorm(nrow(grid), 0, 0.02)
  score <- pmin(pmax(score, 0), 1)

  grid$mean_auc <- score
  grid$best <- grid$mean_auc == max(grid$mean_auc)
  grid
}

# Build dummy CV grids for multiple models at once. `search_spaces` is a
# named list of search_space lists (one per model, mirroring the
# `search_space` entries in Modeling.Rmd's model_specs — omit models
# with no tuning, e.g. logistic/lda/qda). Returns a named list of CV
# grid data frames (see simulate_cv_grid()).
generate_dummy_cv_grids <- function(search_spaces, seed = 42) {
  model_names <- names(search_spaces)
  purrr::map(model_names, function(name) {
    simulate_cv_grid(search_spaces[[name]], seed = seed + which(model_names == name))
  }) %>% purrr::set_names(model_names)
}
