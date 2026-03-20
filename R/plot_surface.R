# plot_surface.R
# 3D surface and heatmap plots for age x year data.

#' Age x year surface/heatmap plot
#'
#' Creates a plotly heatmap of values across age and year
#' dimensions with optional year animation buttons.
#'
#' @param df A data.frame with columns \code{year},
#'   \code{age}, and \code{est}.
#' @param ylab Y-axis label.
#' @param xlab X-axis label.
#' @param title Plot title.
#' @param palette Colour palette function or vector.
#' @param reversescale Logical; reverse colour scale.
#' @return A plotly object.
#' @export
mv_plot_surface <- function(df, ylab = "Age",
                            xlab = "Year",
                            title = NULL,
                            palette = "Viridis",
                            reversescale = FALSE) {
  # Pivot to matrix
  years <- sort(unique(df$year))
  ages  <- sort(unique(df$age))

  mat <- matrix(
    data = NA_real_,
    nrow = length(ages),
    ncol = length(years)
  )
  for (i in seq_len(nrow(df))) {
    r <- match(x = df$age[i], table = ages)
    c_idx <- match(x = df$year[i], table = years)
    if (!is.na(r) && !is.na(c_idx)) {
      mat[r, c_idx] <- df$est[i]
    }
  }

  p <- plot_ly(
    x = years,
    y = ages,
    z = mat,
    type = "heatmap",
    colorscale = palette,
    reversescale = reversescale,
    hovertemplate = paste0(
      "Year: %{x}<br>Age: %{y}<br>",
      "Value: %{z:.3g}<extra></extra>"
    )
  )

  lay <- .mv_layout()
  lay$xaxis$title <- xlab
  lay$yaxis$title <- ylab
  if (!is.null(title)) lay$title <- title

  p <- do.call(
    what = layout,
    args = c(list(p = p), lay)
  )

  .mv_config(p = p)
}
