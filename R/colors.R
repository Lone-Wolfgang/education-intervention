# ── Binary (2 levels: e.g., Pass / Fail, Yes / No) ───────────────────────────
pal_binary <- c("#4472C4", "#C00000")

# ── Sequential (continuous or high-cardinality ordered: low → high) ───────────
pal_sequential <- c(low = "#DEEBF7", high = "#2E75B6")

# ── Divergent (centered at a midpoint, e.g., correlations, log-odds) ──────────
pal_divergent <- c(low = "#C00000", mid = "#F7F7F7", high = "#4472C4")

# ── Ordinal (discrete ordered levels, e.g., Low / Medium / High) ─────────────
# Interpolate n ordered colors (green -> amber -> red) for ordinal levels.
pal_ordinal <- function(n) {
  grDevices::colorRampPalette(c("#92D050", "#FFC000", "#C00000"))(n)
}

# ── Categorical (unordered multi-level, up to 8 levels) ──────────────────────
pal_categorical <- c(
  "#4472C4", "#ED7D31", "#70AD47", "#FFC000",
  "#5B9BD5", "#7030A0", "#C00000", "#44546A"
)

# ── ggplot2 scale helpers ─────────────────────────────────────────────────────
# Manual fill/color scales for two-level variables.
scale_fill_binary  <- function(values = pal_binary, ...) ggplot2::scale_fill_manual(values = values, ...)
scale_color_binary <- function(values = pal_binary, ...) ggplot2::scale_color_manual(values = values, ...)

# Continuous low->high gradient scales.
scale_fill_seq  <- function(...) ggplot2::scale_fill_gradient(low = pal_sequential[["low"]], high = pal_sequential[["high"]], ...)
scale_color_seq <- function(...) ggplot2::scale_color_gradient(low = pal_sequential[["low"]], high = pal_sequential[["high"]], ...)

# Diverging gradient scales centered on `midpoint` (default 0).
scale_fill_div  <- function(midpoint = 0, ...) ggplot2::scale_fill_gradient2(low = pal_divergent[["low"]], mid = pal_divergent[["mid"]], high = pal_divergent[["high"]], midpoint = midpoint, ...)
scale_color_div <- function(midpoint = 0, ...) ggplot2::scale_color_gradient2(low = pal_divergent[["low"]], mid = pal_divergent[["mid"]], high = pal_divergent[["high"]], midpoint = midpoint, ...)

# Discrete scales using n ordinal colors.
scale_fill_ord  <- function(n, ...) ggplot2::scale_fill_manual(values = pal_ordinal(n), ...)
scale_color_ord <- function(n, ...) ggplot2::scale_color_manual(values = pal_ordinal(n), ...)

# Discrete scales for unordered categories (up to 8 levels).
scale_fill_cat  <- function(...) ggplot2::scale_fill_manual(values = pal_categorical, ...)
scale_color_cat <- function(...) ggplot2::scale_color_manual(values = pal_categorical, ...)
