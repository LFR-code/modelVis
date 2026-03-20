# plot_bubble.R
# Residual bubble plots by age x year.

#' Residual bubble plot
#'
#' Creates a plotly bubble plot of residuals with size
#' proportional to absolute value and colour indicating
#' sign.
#'
#' @param df A data.frame with columns \code{year},
#'   \code{age} (or \code{length}), and \code{residual}.
#' @param x_col Name of the x-axis column (default "year").
#' @param y_col Name of the y-axis column (default "age").
#' @param ylab Y-axis label.
#' @param xlab X-axis label.
#' @param title Plot title.
#' @param max_size Maximum bubble size in pixels.
#' @return A plotly object.
#' @export
mv_plot_bubble <- function(df, x_col = "year",
                           y_col = "age",
                           ylab = NULL, xlab = NULL,
                           title = NULL,
                           max_size = 20) {
  if (is.null(ylab)) ylab <- y_col
  if (is.null(xlab)) xlab <- x_col

  # Remove NA residuals

  df <- df[!is.na(df$residual), , drop = FALSE]
  if (nrow(df) == 0L) {
    return(plotly::plotly_empty())
  }

  df$abs_resid <- abs(df$residual)
  df$sign <- ifelse(
    test = df$residual >= 0,
    yes  = "Positive",
    no   = "Negative"
  )

  rc <- .resid_colours()

  p <- plot_ly(
    data   = df,
    x      = as.formula(paste0("~", x_col)),
    y      = as.formula(paste0("~", y_col)),
    size   = ~abs_resid,
    color  = ~sign,
    colors = c(Negative = rc$neg, Positive = rc$pos),
    type   = "scatter",
    mode   = "markers",
    marker = list(
      sizemode = "area",
      sizeref  = 2 * max(df$abs_resid) / (max_size^2),
      line     = list(width = 0.5, color = "white")
    ),
    hovertemplate = paste0(
      xlab, ": %{x}<br>",
      ylab, ": %{y}<br>",
      "Residual: %{text}<extra></extra>"
    ),
    text = ~round(x = residual, digits = 2)
  )

  lay <- .mv_layout()
  lay$xaxis$title <- xlab
  lay$yaxis$title <- ylab
  lay$yaxis$autorange <- "reversed"
  if (!is.null(title)) lay$title <- title

  p <- do.call(
    what = layout,
    args = c(list(p = p), lay)
  )

  .mv_config(p = p)
}
