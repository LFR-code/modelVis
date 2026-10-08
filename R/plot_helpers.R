# plot_helpers.R
# Label annotation helpers and interactive controls for
# plotly plots.

#' Append a unit to an axis/trace label
#'
#' Appends \code{" (unit)"} to \code{label} when \code{unit} is
#' supplied and non-empty; returns \code{label} unchanged otherwise, so
#' a caller whose \code{meta$units} is absent, \code{NULL}, or an empty
#' string for a given quantity renders exactly as it did before units
#' existed.
#'
#' @param label Base axis/trace text, e.g. \code{"Biomass"}.
#' @param unit Unit string, e.g. \code{"kt"}, or \code{NULL}/\code{""}.
#' @return \code{label}, optionally with \code{" (unit)"} appended.
#' @export
mv_unit_label <- function(label, unit) {
  if (is.null(unit) || !nzchar(unit)) {
    return(label)
  }
  paste0(label, " (", unit, ")")
}


#' Top annotation label
#'
#' Creates a plotly annotation positioned above the plot
#' area.
#'
#' @param label Text string.
#' @param offset Vertical offset from top (0 = flush).
#' @return A list for use in \code{layout(annotations = ...)}.
#' @export
mv_top_label <- function(label, offset = 0) {
  list(
    text      = label,
    x         = 0.5,
    y         = 1.02 + offset,
    xref      = "paper",
    yref      = "paper",
    xanchor   = "center",
    yanchor   = "bottom",
    showarrow = FALSE,
    font      = list(size = 14)
  )
}


#' Bottom annotation label
#'
#' @param label Text string.
#' @param offset Vertical offset from bottom.
#' @return A list for use in \code{layout(annotations = ...)}.
#' @export
mv_bottom_label <- function(label, offset = 0) {
  list(
    text      = label,
    x         = 0.5,
    y         = -0.12 - offset,
    xref      = "paper",
    yref      = "paper",
    xanchor   = "center",
    yanchor   = "top",
    showarrow = FALSE,
    font      = list(size = 12)
  )
}


#' Left annotation label
#'
#' @param label Text string.
#' @param offset Horizontal offset from left edge.
#' @return A list for use in \code{layout(annotations = ...)}.
#' @export
mv_left_label <- function(label, offset = 0) {
  list(
    text      = label,
    x         = -0.08 - offset,
    y         = 0.5,
    xref      = "paper",
    yref      = "paper",
    xanchor   = "center",
    yanchor   = "middle",
    textangle = -90,
    showarrow = FALSE,
    font      = list(size = 12)
  )
}


#' Right annotation label
#'
#' @param label Text string.
#' @param offset Horizontal offset from right edge.
#' @return A list for use in \code{layout(annotations = ...)}.
#' @export
mv_right_label <- function(label, offset = 0) {
  list(
    text      = label,
    x         = 1.08 + offset,
    y         = 0.5,
    xref      = "paper",
    yref      = "paper",
    xanchor   = "center",
    yanchor   = "middle",
    textangle = 90,
    showarrow = FALSE,
    font      = list(size = 12)
  )
}


#' Log/linear toggle buttons
#'
#' Creates updatemenus buttons for switching between log
#' and linear y-axis scales.
#'
#' @return A list for use in
#'   \code{layout(updatemenus = ...)}.
#' @export
mv_log_linear_buttons <- function() {
  list(
    list(
      type = "buttons",
      direction = "left",
      x = 0.0,
      y = 1.12,
      xanchor = "left",
      yanchor = "top",
      buttons = list(
        list(
          label  = "Linear",
          method = "relayout",
          args   = list(list(
            "yaxis.type" = "linear"
          ))
        ),
        list(
          label  = "Log",
          method = "relayout",
          args   = list(list(
            "yaxis.type" = "log"
          ))
        )
      )
    )
  )
}


#' Configure plotly modebar
#'
#' Applies standard config to a plotly object with
#' responsive sizing and a clean modebar.
#'
#' @param p A plotly object.
#' @return A configured plotly object.
#' @export
mv_config <- function(p) {
  config(
    p = p,
    displaylogo = FALSE,
    responsive = TRUE,
    modeBarButtonsToRemove = c(
      "select2d", "lasso2d", "autoScale2d"
    )
  )
}

# Internal alias for backward compatibility
.mv_config <- mv_config
