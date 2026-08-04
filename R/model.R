# Fit a logistic regression of Fail (1 = failed) on the given
# numeric and categorical predictors; returns the glm object.
fit_logistic <- function(df, num_vars, cat_vars) {
  model_df <- df %>%
    dplyr::mutate(Fail = factor(dplyr::if_else(Status == "Fail", 1L, 0L))) %>%
    dplyr::select(Fail, dplyr::all_of(num_vars), dplyr::all_of(cat_vars))
  glm(Fail ~ ., data = model_df, family = binomial)
}

# Same as fit_logistic, but appends explicit interaction terms
# (e.g. "Hours_Studied:Parental_Involvement") to the additive formula.
# `interactions` is a character vector of "var1:var2" terms.
fit_logistic_complex <- function(df, num_vars, cat_vars, interactions) {
  model_df <- df %>%
    dplyr::mutate(Fail = factor(dplyr::if_else(Status == "Fail", 1L, 0L))) %>%
    dplyr::select(Fail, dplyr::all_of(num_vars), dplyr::all_of(cat_vars))
  rhs <- paste(c(num_vars, cat_vars, interactions), collapse = " + ")
  glm(stats::as.formula(paste("Fail ~", rhs)), data = model_df, family = binomial)
}

# Fit LDA of Fail on the given numeric and categorical predictors;
# returns the MASS::lda object.
fit_lda <- function(df, num_vars, cat_vars) {
  model_df <- df %>%
    dplyr::mutate(Fail = factor(dplyr::if_else(Status == "Fail", 1L, 0L))) %>%
    dplyr::select(Fail, dplyr::all_of(num_vars), dplyr::all_of(cat_vars))
  MASS::lda(Fail ~ ., data = model_df)
}

# Fit QDA of Fail on the given numeric and categorical predictors;
# returns the MASS::qda object.
fit_qda <- function(df, num_vars, cat_vars) {
  model_df <- df %>%
    dplyr::mutate(Fail = factor(dplyr::if_else(Status == "Fail", 1L, 0L))) %>%
    dplyr::select(Fail, dplyr::all_of(num_vars), dplyr::all_of(cat_vars))
  MASS::qda(Fail ~ ., data = model_df)
}

# KNN has no reusable "fit" object: this builds the design matrices from
# train/test (dummy-encoded categoricals, scaled numerics via
# fit_preprocessor/apply_preprocessor from R/split.R) and returns class
# predictions for test_df directly.
fit_knn <- function(train_df, test_df, num_vars, cat_vars, k = 5) {
  params <- fit_preprocessor(train_df, num_vars)
  train_scaled <- apply_preprocessor(params, train_df)
  test_scaled  <- apply_preprocessor(params, test_df)

  build_x <- function(d) {
    model_df <- d %>% dplyr::select(dplyr::all_of(num_vars), dplyr::all_of(cat_vars))
    stats::model.matrix(~ . - 1, data = model_df)
  }
  train_x <- build_x(train_scaled)
  test_x  <- build_x(test_scaled)
  train_cl <- factor(dplyr::if_else(train_df$Status == "Fail", 1L, 0L))

  class::knn(train_x, test_x, cl = train_cl, k = k)
}

# Fit a regularized (elastic net) logistic regression via cross-validated
# lambda selection. `alpha = 1` is lasso, `alpha = 0` is ridge.
# Returns the glmnet::cv.glmnet object.
fit_glmnet <- function(df, num_vars, cat_vars, alpha = 1) {
  model_df <- df %>% dplyr::select(dplyr::all_of(num_vars), dplyr::all_of(cat_vars))
  x <- stats::model.matrix(~ . - 1, data = model_df)
  y <- factor(dplyr::if_else(df$Status == "Fail", 1L, 0L))
  glmnet::cv.glmnet(x, y, family = "binomial", alpha = alpha)
}

# Fit a single classification tree of Fail on the given numeric and
# categorical predictors; returns the rpart object.
fit_tree <- function(df, num_vars, cat_vars, ...) {
  model_df <- df %>%
    dplyr::mutate(Fail = factor(dplyr::if_else(Status == "Fail", 1L, 0L))) %>%
    dplyr::select(Fail, dplyr::all_of(num_vars), dplyr::all_of(cat_vars))
  rpart::rpart(Fail ~ ., data = model_df, method = "class", ...)
}

# Fit a random forest classifier of Fail on the given numeric and
# categorical predictors; returns the randomForest object.
fit_rf <- function(df, num_vars, cat_vars, ntree = 500, ...) {
  model_df <- df %>%
    dplyr::mutate(Fail = factor(dplyr::if_else(Status == "Fail", 1L, 0L))) %>%
    dplyr::select(Fail, dplyr::all_of(num_vars), dplyr::all_of(cat_vars))
  randomForest::randomForest(Fail ~ ., data = model_df, ntree = ntree, ...)
}

# Fit an XGBoost classifier of Fail on the given numeric and categorical
# predictors (categoricals dummy-encoded via model.matrix). Returns the
# xgb.Booster object; `nrounds` controls the number of boosting iterations.
fit_xgboost <- function(df, num_vars, cat_vars, nrounds = 100, ...) {
  model_df <- df %>% dplyr::select(dplyr::all_of(num_vars), dplyr::all_of(cat_vars))
  x <- stats::model.matrix(~ . - 1, data = model_df)
  y <- dplyr::if_else(df$Status == "Fail", 1L, 0L)
  dtrain <- xgboost::xgb.DMatrix(data = x, label = y)
  xgboost::xgb.train(
    params = list(objective = "binary:logistic", eval_metric = "logloss", ...),
    data = dtrain,
    nrounds = nrounds,
    verbose = 0
  )
}
