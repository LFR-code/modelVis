# plot_kobe.R
# Kobe (phase) plot with status quadrants.

#' Kobe phase plot
#'
#' Creates a plotly Kobe plot showing stock status
#' relative to reference points, with coloured quadrants.
#'
#' @param biomass Numeric vector of B/Bmsy ratios by year.
#' @param mortality Numeric vector of F/Fmsy or U/Umsy
#'   ratios by year.
#' @param years Integer vector of years.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param title Plot title.
#' @return A plotly object.
#' @export
mv_plot_kobe <- function(biomass, mortality, years,
                         xlab = "B/Bmsy",
                         ylab = "F/Fmsy",
                         title = NULL) {
  kc <- .kobe_colours()

  df <- data.frame(
    year = years,
    b    = biomass,
    f    = mortality,
    stringsAsFactors = FALSE
  )
  # Sort by year for chronological trajectory
  df <- df[order(df$year), , drop = FALSE]

  # Quadrant background shapes
  x_max <- max(c(df$b, 2), na.rm = TRUE) * 1.2
  y_max <- max(c(df$f, 2), na.rm = TRUE) * 1.2

  shapes <- list(
    # Green: B > Bmsy, F < Fmsy (bottom-right)
    list(
      type = "rect", x0 = 1, x1 = x_max,
      y0 = 0, y1 = 1,
      fillcolor = kc["green"], layer = "below",
      line = list(width = 0)
    ),
    # Red: B < Bmsy, F > Fmsy (top-left)
    list(
      type = "rect", x0 = 0, x1 = 1,
      y0 = 1, y1 = y_max,
      fillcolor = kc["red"], layer = "below",
      line = list(width = 0)
    ),
    # Yellow: B > Bmsy, F > Fmsy (top-right)
    list(
      type = "rect", x0 = 1, x1 = x_max,
      y0 = 1, y1 = y_max,
      fillcolor = kc["yellow_top"], layer = "below",
      line = list(width = 0)
    ),
    # Yellow: B < Bmsy, F < Fmsy (bottom-left)
    list(
      type = "rect", x0 = 0, x1 = 1,
      y0 = 0, y1 = 1,
      fillcolor = kc["yellow_right"], layer = "below",
      line = list(width = 0)
    )
  )

  # Trajectory line
  p <- plot_ly(data = df, x = ~b, y = ~f)
  p <- add_lines(
    p = p,
    line = list(color = "#333333", width = 1),
    showlegend = FALSE, hoverinfo = "skip"
  )

  # Year markers
  p <- add_markers(
    p = p,
    marker = list(
      color = "#333333", size = 4,
      line = list(color = "white", width = 0.5)
    ),
    text = ~year,
    hovertemplate = paste0(
      "Year: %{text}<br>",
      xlab, ": %{x:.2f}<br>",
      ylab, ": %{y:.2f}<extra></extra>"
    ),
    showlegend = FALSE
  )

  # Terminal year highlight
  n <- nrow(df)
  p <- add_markers(
    p = p,
    x = df$b[n], y = df$f[n],
    marker = list(
      color = "#d62728", size = 10,
      symbol = "star",
      line = list(color = "white", width = 1)
    ),
    name = as.character(df$year[n]),
    hovertemplate = paste0(
      "Terminal year: ", df$year[n], "<br>",
      xlab, ": %{x:.2f}<br>",
      ylab, ": %{y:.2f}<extra></extra>"
    )
  )

  lay <- .mv_layout()
  lay$xaxis$title <- xlab
  lay$yaxis$title <- ylab
  lay$shapes <- shapes
  lay$showlegend <- FALSE
  if (!is.null(title)) lay$title <- title

  p <- do.call(
    what = layout,
    args = c(list(p = p), lay)
  )

  .mv_config(p = p)
}
