# Shared ggplot2 theme for report/HTML output (compact type sizes).
report_theme <- function(base_size = 12, ...) {
  ggthemes::theme_excel_new(base_size = base_size, ...) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.title      = ggplot2::element_text(size = base_size + 4, face = "bold"),
      plot.subtitle   = ggplot2::element_text(size = base_size + 1),
      plot.margin     = ggplot2::margin(8, 8, 8, 8),
      axis.title      = ggplot2::element_text(size = base_size + 1, face = "bold"),
      axis.text       = ggplot2::element_text(size = base_size),
      legend.title    = ggplot2::element_text(size = base_size,     face = "bold"),
      legend.text     = ggplot2::element_text(size = base_size - 1),
      strip.text      = ggplot2::element_text(size = base_size,     face = "bold"),
      plot.caption    = ggplot2::element_text(size = base_size - 2, hjust = 0)
    )
}

# Same theme scaled up for slides/presentations.
ppt_theme <- function(base_size = 20, ...) {
  ggthemes::theme_excel_new(base_size = base_size, ...) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.title      = ggplot2::element_text(size = base_size + 8, face = "bold"),
      plot.subtitle   = ggplot2::element_text(size = base_size + 2),
      plot.margin     = ggplot2::margin(14, 14, 14, 14),
      axis.title      = ggplot2::element_text(size = base_size + 2, face = "bold"),
      axis.text       = ggplot2::element_text(size = base_size),
      legend.title    = ggplot2::element_text(size = base_size,     face = "bold"),
      legend.text     = ggplot2::element_text(size = base_size - 2),
      strip.text      = ggplot2::element_text(size = base_size,     face = "bold"),
      plot.caption    = ggplot2::element_text(size = base_size - 4, hjust = 0)
    )
}

# Make the report theme the default for every plot.
ggplot2::theme_set(report_theme())
