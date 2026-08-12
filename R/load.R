# Read the raw CSV and encode every categorical column as a factor,
# using ordered factors where the levels have a natural ranking.
# Prints a glimpse/summary and returns the data frame invisibly.
load_ <- function(path) {
  df <- readr::read_csv(path)
  df$Parental_Involvement <- factor(
    df$Parental_Involvement,
    levels <- c("Low", "Medium", "High"),
    ordered = TRUE
  )
  df$Access_to_Resources <- factor(
    df$Access_to_Resources,
    levels <- c("Low", "Medium", "High"),
    ordered = TRUE
  )
  df$Motivation_Level <- factor(
    df$Motivation_Level,
    levels <- c("Low", "Medium", "High"),
    ordered = TRUE
  )
  df$Extracurricular_Activities <- factor(df$Extracurricular_Activities)
  df$Internet_Access <- factor(df$Internet_Access)
  df$Family_Income <- factor(
    df$Family_Income,
    levels <- c("Low", "Medium", "High"),
    ordered = TRUE
  )
  df$Teacher_Quality <- factor(
    df$Teacher_Quality,
    levels <- c("Low", "Medium", "High"),
    ordered = TRUE
  )
  df$School_Type <- factor(df$School_Type)
  df$Peer_Influence <- factor(
    df$Peer_Influence,
    levels <- c("Negative", "Neutral", "Positive"),
    ordered = TRUE
  )
  df$Learning_Disabilities <- factor(df$Learning_Disabilities)
  df$Parental_Education_Level <- factor(
    df$Parental_Education_Level,
    levels <- c("High School", "College", "Postgraduate"),
    ordered = TRUE
  )
  df$Distance_from_Home <- factor(
    df$Distance_from_Home,
    levels <- c("Near", "Moderate", "Far"),
    ordered = TRUE
  )
  df$Gender <- factor(df$Gender)
  
  # Quick console sanity check of types and value ranges.
  glimpse(df)
  summary(df)
  
  invisible(df)
}