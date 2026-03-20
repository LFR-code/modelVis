# plot_resid.R
# Residual diagnostic panels.

#' Four-panel residual diagnostics
#'
#' Creates a 2x2 subplot with: residuals vs year,
#' residuals vs predicted, QQ plot, and histogram.
#'
#' @param df A data.frame with columns \code{year},
#'   \code{residual}, and optionally \code{pred}.
#' @param title Plot title.
#' @return A plotly object.
#' @export
mv_plot_resid_panel <- function(df, title = NULL) {
  df <- df[!is.na(df$residual), , drop = FALSE]
  if (nrow(df) == 0L) return(plotly::plotly_empty())

  rc <- .resid_colours()
  df$col <- ifelse(
    test = df$residual >= 0,
    yes  = rc$pos,
    no   = rc$neg
  )

  # Panel 1: Residuals vs year
  p1 <- plot_ly(
    data = df, x = ~year, y = ~residual,
    type = "scatter", mode = "markers",
    marker = list(color = ~col, size = 5),
    showlegend = FALSE,
    hovertemplate = "%{x}: %{y:.3f}<extra></extra>"
  )
  p1 <- layout(
    p = p1,
    xaxis = list(title = "Year"),
    yaxis = list(title = "Residual"),
    shapes = list(list(
      type = "line",
      x0 = min(df$year), x1 = max(df$year),
      y0 = 0, y1 = 0,
      line = list(color = "#666666", dash = "dash")
    ))
  )

  # Panel 2: Residuals vs predicted
  if ("pred" %in% names(df) &&
      any(!is.na(df$pred))) {
    p2 <- plot_ly(
      data = df, x = ~pred, y = ~residual,
      type = "scatter", mode = "markers",
      marker = list(color = ~col, size = 5),
      showlegend = FALSE,
      hovertemplate = paste0(
        "Pred: %{x:.3g}<br>Resid: %{y:.3f}",
        "<extra></extra>"
      )
    )
    p2 <- layout(
      p = p2,
      xaxis = list(title = "Predicted"),
      yaxis = list(title = "Residual"),
      shapes = list(list(
        type = "line",
        x0 = min(df$pred, na.rm = TRUE),
        x1 = max(df$pred, na.rm = TRUE),
        y0 = 0, y1 = 0,
        line = list(color = "#666666", dash = "dash")
      ))
    )
  } else {
    # Residuals vs index if no pred
    p2 <- plot_ly(
      data = df, x = ~seq_len(nrow(df)),
      y = ~residual,
      type = "scatter", mode = "markers",
      marker = list(color = ~col, size = 5),
      showlegend = FALSE
    )
    p2 <- layout(
      p = p2,
      xaxis = list(title = "Index"),
      yaxis = list(title = "Residual")
    )
  }

  # Panel 3: QQ plot
  n <- length(df$residual)
  qq_y <- sort(df$residual)
  qq_x <- stats::qnorm(p = ppoints(n = n))
  p3 <- plot_ly(
    x = qq_x, y = qq_y,
    type = "scatter", mode = "markers",
    marker = list(color = "#1f77b4", size = 5),
    showlegend = FALSE,
    hovertemplate = paste0(
      "Theoretical: %{x:.2f}<br>Sample: %{y:.3f}",
      "<extra></extra>"
    )
  )
  # Add 1:1 line
  qq_range <- range(c(qq_x, qq_y))
  p3 <- add_lines(
    p = p3,
    x = qq_range, y = qq_range,
    line = list(color = "#666666", dash = "dash"),
    showlegend = FALSE, hoverinfo = "skip"
  )
  p3 <- layout(
    p = p3,
    xaxis = list(title = "Theoretical quantiles"),
    yaxis = list(title = "Sample quantiles")
  )

  # Panel 4: Histogram
  p4 <- plot_ly(
    x = df$residual,
    type = "histogram",
    marker = list(
      color = "#1f77b4",
      line = list(color = "white", width = 0.5)
    ),
    showlegend = FALSE,
    hovertemplate = paste0(
      "Residual: %{x:.2f}<br>Count: %{y}",
      "<extra></extra>"
    )
  )
  p4 <- layout(
    p = p4,
    xaxis = list(title = "Residual"),
    yaxis = list(title = "Count")
  )

  p_out <- subplot(
    p1, p2, p3, p4,
    nrows  = 2,
    shareX = FALSE,
    shareY = FALSE,
    titleX = TRUE,
    titleY = TRUE,
    margin = 0.08
  )

  if (!is.null(title)) {
    p_out <- layout(
      p = p_out,
      title = title
    )
  }

  .mv_config(p = p_out)
}
