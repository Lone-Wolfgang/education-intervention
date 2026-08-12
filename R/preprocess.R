# Return the most frequent non-missing value of x (the mode).
mode_impute <- function(x) {
  ux <- na.omit(x)
  names(sort(table(ux), decreasing = TRUE))[1]
}

# Fill NAs in the given categorical columns with their mode;
# errors out if any missing values remain.
impute_categoricals <- function(df, cols) {
  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cols), ~ tidyr::replace_na(., mode_impute(.))))
  stopifnot(sum(is.na(df)) == 0)
  df
}

# Drop rows with an implausible Exam_Score (> max_score, e.g. the 101 data
# entry error), so the score stays on a consistent 0-100 scale.
filter_exam_outliers <- function(df, max_score = 100) {
  dplyr::filter(df, Exam_Score <= max_score)
}

# Derive the binary Status target from Exam_Score (>= cutoff is "Pass")
# and re-level the categorical predictors into their natural order.
add_target_and_factors <- function(df, cutoff = 61) {
  ord_levels <- c("Low", "Medium", "High")
  df %>%
    dplyr::mutate(
      Status = factor(dplyr::if_else(Exam_Score >= cutoff, "Pass", "Fail"),
                      levels = c("Pass", "Fail")),
      Parental_Involvement     = factor(Parental_Involvement, ord_levels),
      Access_to_Resources      = factor(Access_to_Resources,  ord_levels),
      Motivation_Level         = factor(Motivation_Level,     ord_levels),
      Family_Income            = factor(Family_Income,        ord_levels),
      Teacher_Quality          = factor(Teacher_Quality,      ord_levels),
      Distance_from_Home       = factor(Distance_from_Home, c("Near","Moderate","Far")),
      Peer_Influence           = factor(Peer_Influence, c("Negative","Neutral","Positive")),
      Parental_Education_Level = factor(Parental_Education_Level,
                                        c("High School","College","Postgraduate")),
      dplyr::across(dplyr::where(is.character), as.factor)
    )
}

# Predictors dropped after the kitchen-sink screen in EDA.Rmd: none of these
# came close to significance in the full model (|z| < 1.4 for all three), and
# each is either a demographic label or a variable with no plausible direct
# mechanism on failing once study time and attendance are in the model.
PRUNED_VARS <- c("Gender", "School_Type", "Sleep_Hours")

# Numeric predictor names with the pruned variables removed.
select_num_vars <- function(num_vars, drop = PRUNED_VARS) {
  setdiff(num_vars, drop)
}

# Categorical predictor names (every factor column except the target) with the
# pruned variables removed.
select_cat_vars <- function(df, drop = PRUNED_VARS) {
  cols <- names(dplyr::select(df, dplyr::where(is.factor), -Status))
  setdiff(cols, drop)
}

# Polynomial contrast truncated to its linear component: the first column of
# contr.poly(), so an ordered factor contributes a single `.L` term instead of
# `.L` plus `.Q`.
contr.linear <- function(n, contrasts = TRUE) {
  out <- stats::contr.poly(n, contrasts = contrasts)
  if (!contrasts) return(out)
  out[, 1, drop = FALSE]
}

# Make contr.linear the default coding for ordered factors, so every ordinal
# predictor enters a model as a single linear Low -> High trend. The quadratic
# (`.Q`) components were all non-significant in the full model, so dropping
# them buys back one df per ordinal with essentially no loss of fit.
#
# Set as a global option rather than as a per-column `contrasts` attribute
# because some predict() methods (MASS::predict.lda, MASS::predict.qda)
# rebuild their model frame from stored xlevels, which silently drops column
# attributes and would re-expand the ordinals back to `.L` + `.Q` at
# prediction time.
use_linear_contrasts <- function() {
  options(contrasts = c(unordered = "contr.treatment", ordered = "contr.linear"))
  invisible(getOption("contrasts"))
}