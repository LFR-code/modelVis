# plot_sr.R
# Stock-recruitment relationship plots.

#' Stock-recruitment scatter plot
#'
#' Creates a plotly scatter plot of recruitment vs
#' spawning biomass with year labels.
#'
#' @param df A data.frame from \code{mv$stock_recruit}
#'   with columns \code{year}, \code{ssb},
#'   \code{recruitment}.
#' @param ylab Y-axis label.
#' @param xlab X-axis label.
#' @param title Plot title.
#' @param show_labels Logical; show year labels on points.
#' @return A plotly object.
#' @export
mv_plot_sr <- function(df, ylab = "Recruitment",
                       xlab = "Spawning biomass",
                       title = NULL,
                       show_labels = FALSE) {
  p <- plot_ly(
    data = df,
    x = ~ssb,
    y = ~recruitment
  )

  # Points coloured by year
  p <- add_markers(
    p = p,
    color = ~year,
    colors = c("#1f77b4", "#d62728"),
    marker = list(
      size = 6,
      line = list(color = "white", width = 0.5)
    ),
    text = ~year,
    hovertemplate = paste0(
      "Year: %{text}<br>",
      "SSB: %{x:.3g}<br>",
      "Recruitment: %{y:.3g}<extra></extra>"
    ),
    showlegend = FALSE
  )

  if (show_labels) {
    # Label every 5th year to avoid crowding
    label_idx <- seq(
      from = 1, to = nrow(df), by = 5
    )
    label_df <- df[label_idx, , drop = FALSE]
    p <- add_text(
      p = p,
      data = label_df,
      x = ~ssb,
      y = ~recruitment,
      text = ~year,
      textposition = "top right",
      textfont = list(size = 8),
      showlegend = FALSE,
      hoverinfo = "skip"
    )
  }

  lay <- .mv_layout()
  lay$xaxis$title <- xlab
  lay$yaxis$title <- ylab
  if (!is.null(title)) lay$title <- title
  lay$showlegend <- FALSE

  p <- do.call(
    what = layout,
    args = c(list(p = p), lay)
  )

  .mv_config(p = p)
}
