# Split df into n proportional subsets, stratified by `strata` so each
# subset preserves the class balance of that column. `props` may be named
# (names become the split names) or unnamed (auto-named split_1, split_2, ...).
# Returns a named list of data frames.
split_data <- function(df, props, strata = "Status", seed = NULL) {
  stopifnot(abs(sum(props) - 1) < 1e-6)

  if (is.null(names(props)) || any(names(props) == "")) {
    names(props) <- paste0("split_", seq_along(props))
  }

  if (!is.null(seed)) set.seed(seed)

  n <- nrow(df)
  split_label <- character(n)

  strata_vals <- df[[strata]]
  for (lvl in unique(strata_vals)) {
    idx <- which(strata_vals == lvl)
    idx <- sample(idx)
    n_lvl <- length(idx)

    # Cumulative counts per split, rounding down and giving any remainder
    # to the last split so every row is assigned exactly once.
    counts <- floor(n_lvl * props)
    counts[length(counts)] <- n_lvl - sum(counts[-length(counts)])

    bounds <- cumsum(c(0, counts))
    for (i in seq_along(props)) {
      lo <- bounds[i] + 1
      hi <- bounds[i + 1]
      if (lo <= hi) {
        split_label[idx[lo:hi]] <- names(props)[i]
      }
    }
  }

  lapply(names(props), function(nm) df[split_label == nm, , drop = FALSE]) %>%
    setNames(names(props))
}

# Assign each row of df to one of k folds, stratified by `strata` so every
# fold preserves the class balance of that column (same stratified-shuffle
# pattern as split_data()). Returns an integer vector (length nrow(df)) of
# fold ids 1..k.
make_cv_folds <- function(df, k = 5, strata = "Status", seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  n <- nrow(df)
  fold <- integer(n)

  strata_vals <- df[[strata]]
  for (lvl in unique(strata_vals)) {
    idx <- which(strata_vals == lvl)
    idx <- sample(idx)
    n_lvl <- length(idx)

    fold[idx] <- rep(seq_len(k), length.out = n_lvl)
  }

  fold
}

# Fit scaling/imputation parameters on the training data only: for each
# numeric column, the mean (used for imputation) and mean/sd (used for
# scaling), both computed from non-missing train values.
fit_preprocessor <- function(train_df, num_vars) {
  stats <- purrr::map(num_vars, function(v) {
    x <- train_df[[v]]
    list(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
  })
  names(stats) <- num_vars
  list(num_vars = num_vars, stats = stats)
}

# Apply previously fit train-set parameters to any data frame: impute NAs
# in numeric columns with the train mean, then center/scale using the
# train mean/sd. Non-numeric columns are left untouched.
apply_preprocessor <- function(params, new_df) {
  for (v in params$num_vars) {
    m <- params$stats[[v]]$mean
    s <- params$stats[[v]]$sd
    x <- new_df[[v]]
    x <- tidyr::replace_na(x, m)
    new_df[[v]] <- (x - m) / s
  }
  new_df
}
