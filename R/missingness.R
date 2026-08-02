# Count NAs per column and return only the columns that have any,
# as a long table of variable / n_missing / pct.
count_missing_values <- function(df){
  df %>% 
    dplyr::summarise(across(everything(), ~ sum(is.na(.)))) %>%
    tidyr::pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
    dplyr::mutate(pct = n_missing / nrow(df)) %>%
    dplyr::filter(n_missing > 0)
  
}

# Render the missingness summary as a gt table, worst offenders first.
tabulate_missing_values <- function(miss) {
  miss %>%
    dplyr::arrange(desc(n_missing)) %>%
    gt::gt() %>%
    gt::fmt_percent(pct, decimals = 2) %>%
    gt::tab_header(title = "Missing Values")
}

