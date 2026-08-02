# Fit a logistic regression of Fail (1 = failed) on the given
# numeric and categorical predictors; returns the glm object.
fit_logistic <- function(df, num_vars, cat_vars) {
  model_df <- df %>%
    dplyr::mutate(Fail = factor(dplyr::if_else(Status == "Fail", 1L, 0L))) %>%
    dplyr::select(Fail, dplyr::all_of(num_vars), dplyr::all_of(cat_vars))
  glm(Fail ~ ., data = model_df, family = binomial)
}
