# plot_trend.R
# Time-series trend plots with optional CI ribbons.

#' Plot a time-series trend
#'
#' Creates a plotly line plot of a single time series
#' with optional confidence interval ribbon.
#'
#' @param df A data.frame with columns \code{year},
#'   \code{est}, and optionally \code{lwr}, \code{upr}.
#' @param ylab Y-axis label.
#' @param xlab X-axis label.
#' @param title Plot title (NULL for none).
#' @param ci Logical; show CI ribbon if lwr/upr available.
#' @param colour Line colour.
#' @param ref_line Optional horizontal reference line value.
#' @param ref_label Label for the reference line.
#' @param log_buttons Logical; add log/linear toggle.
#' @return A plotly object.
#' @export
mv_plot_trend <- function(df, ylab = "", xlab = "Year",
                          title = NULL, ci = TRUE,
                          colour = "#1f77b4",
                          ref_line = NULL,
                          ref_label = NULL,
                          log_buttons = FALSE) {
  has_ci <- ci &&
    "lwr" %in% names(df) &&
    any(!is.na(df$lwr))

  # Colour with alpha for ribbon
  ribbon_col <- paste0(
    substr(x = colour, start = 1, stop = 7), "33"
  )

  p <- plot_ly(data = df, x = ~year)

  if (has_ci) {
    p <- add_ribbons(
      p = p,
      ymin   = ~lwr,
      ymax   = ~upr,
      line   = list(width = 0),
      fillcolor = ribbon_col,
      showlegend = FALSE,
      hoverinfo  = "skip",
      name   = "95% CI"
    )
  }

  p <- add_lines(
    p = p,
    y     = ~est,
    line  = list(color = colour, width = 2),
    name  = "Estimate",
    hovertemplate = paste0(
      "%{x}: %{y:.3g}<extra></extra>"
    )
  )

  # Reference line
  shapes <- list()
  annotations <- list()
  if (!is.null(ref_line)) {
    shapes <- list(list(
      type = "line",
      x0 = min(df$year), x1 = max(df$year),
      y0 = ref_line, y1 = ref_line,
      line = list(
        color = "#999999", width = 1.5, dash = "dash"
      )
    ))
    if (!is.null(ref_label)) {
      annotations <- list(list(
        x         = max(df$year),
        y         = ref_line,
        text      = ref_label,
        xanchor   = "right",
        yanchor   = "bottom",
        showarrow = FALSE,
        font      = list(size = 10, color = "#999999")
      ))
    }
  }

  lay <- .mv_layout()
  lay$xaxis$title <- xlab
  lay$yaxis$title <- ylab
  if (!is.null(title)) lay$title <- title
  lay$shapes <- shapes
  lay$annotations <- annotations
  lay$showlegend <- FALSE

  if (log_buttons) {
    lay$updatemenus <- mv_log_linear_buttons()
  }

  p <- do.call(
    what = layout,
    args = c(list(p = p), lay)
  )

  .mv_config(p = p)
}


#' Plot multiple time-series trends
#'
#' Creates a plotly plot with multiple grouped lines
#' (e.g., by fleet, sex, or area).
#'
#' @param df A data.frame with columns \code{year},
#'   \code{est}, and a grouping column.
#' @param group Name of the grouping column.
#' @param ylab Y-axis label.
#' @param xlab X-axis label.
#' @param title Plot title.
#' @param colours Named vector of colours for groups.
#'   If NULL, uses \code{mv_colours()}.
#' @param log_buttons Logical; add log/linear toggle.
#' @return A plotly object.
#' @export
mv_plot_trend_multi <- function(df, group = "fleet",
                                ylab = "", xlab = "Year",
                                title = NULL,
                                colours = NULL,
                                log_buttons = FALSE) {
  grp_levels <- unique(df[[group]])
  n_grp <- length(grp_levels)

  if (is.null(colours)) {
    cols <- mv_colours(n = n_grp)
    names(cols) <- grp_levels
  } else {
    cols <- colours
  }

  p <- plot_ly()

  for (i in seq_along(grp_levels)) {
    g <- grp_levels[i]
    sub <- df[df[[group]] == g, , drop = FALSE]
    p <- add_lines(
      p = p,
      x     = sub$year,
      y     = sub$est,
      name  = g,
      line  = list(color = cols[g], width = 2),
      legendgroup = g,
      hovertemplate = paste0(
        g, " %{x}: %{y:.3g}<extra></extra>"
      )
    )
  }

  lay <- .mv_layout()
  lay$xaxis$title <- xlab
  lay$yaxis$title <- ylab
  if (!is.null(title)) lay$title <- title
  lay$showlegend <- TRUE

  if (log_buttons) {
    lay$updatemenus <- mv_log_linear_buttons()
  }

  p <- do.call(
    what = layout,
    args = c(list(p = p), lay)
  )

  .mv_config(p = p)
}
