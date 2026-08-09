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