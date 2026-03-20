# plot_comparison.R
# Model comparison overlay plots.

#' Compare time-series across models
#'
#' Creates a plotly plot overlaying a time-series
#' component from multiple mv objects.
#'
#' @param mv_list A list of mv objects.
#' @param component Name of the component to plot
#'   (e.g., "spawning_biomass").
#' @param ylab Y-axis label.
#' @param xlab X-axis label.
#' @param title Plot title.
#' @param ci Logical; show CI ribbons.
#' @param log_buttons Logical; add log/linear toggle.
#' @return A plotly object.
#' @export
mv_plot_compare <- function(mv_list,
                            component = "spawning_biomass",
                            ylab = "", xlab = "Year",
                            title = NULL,
                            ci = TRUE,
                            log_buttons = FALSE) {
  n <- length(mv_list)
  labels <- vapply(
    X = mv_list,
    FUN = function(m) m$meta$label,
    FUN.VALUE = character(1)
  )
  if (any(labels == "")) {
    labels[labels == ""] <- paste0(
      "Model ", which(labels == "")
    )
  }

  cols <- mv_colours(n = n)

  p <- plot_ly()

  for (i in seq_len(n)) {
    df <- mv_list[[i]][[component]]
    if (is.null(df)) next

    col <- cols[i]
    ribbon_col <- paste0(
      substr(x = col, start = 1, stop = 7), "33"
    )

    has_ci <- ci &&
      "lwr" %in% names(df) &&
      any(!is.na(df$lwr))

    if (has_ci) {
      p <- add_ribbons(
        p = p,
        x    = df$year,
        ymin = df$lwr,
        ymax = df$upr,
        fillcolor  = ribbon_col,
        line       = list(width = 0),
        showlegend = FALSE,
        hoverinfo  = "skip",
        legendgroup = labels[i]
      )
    }

    p <- add_lines(
      p = p,
      x    = df$year,
      y    = df$est,
      name = labels[i],
      line = list(color = col, width = 2),
      legendgroup = labels[i],
      hovertemplate = paste0(
        labels[i], " %{x}: %{y:.3g}",
        "<extra></extra>"
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
