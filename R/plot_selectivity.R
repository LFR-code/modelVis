# plot_selectivity.R
# Selectivity-at-age curve plots.

#' Plot selectivity curves by fleet
#'
#' Creates a plotly plot showing selectivity-at-age
#' curves for each fleet, optionally separated by sex.
#'
#' @param df A data.frame with columns \code{age},
#'   \code{sex}, \code{fleet}, \code{est}.
#' @param by_sex Logical; if TRUE, separate panels by sex.
#' @param ylab Y-axis label.
#' @param xlab X-axis label.
#' @param title Plot title.
#' @param colours Named colour vector for fleets. If NULL,
#'   uses \code{mv_fleet_colours()}.
#' @return A plotly object.
#' @export
mv_plot_selectivity <- function(df, by_sex = FALSE,
                                ylab = "Selectivity",
                                xlab = "Age",
                                title = NULL,
                                colours = NULL) {
  fleet_names <- unique(df$fleet)
  if (is.null(colours)) {
    colours <- mv_fleet_colours(
      fleet_names = fleet_names
    )
  }

  if (by_sex) {
    sex_levels <- unique(df$sex)
    plot_list <- vector(
      mode = "list", length = length(sex_levels)
    )
    for (s in seq_along(sex_levels)) {
      sx <- sex_levels[s]
      sub <- df[df$sex == sx, , drop = FALSE]

      p <- plot_ly()
      for (fl in fleet_names) {
        fl_sub <- sub[sub$fleet == fl, , drop = FALSE]
        p <- add_lines(
          p = p,
          x    = fl_sub$age,
          y    = fl_sub$est,
          name = fl,
          line = list(
            color = colours[fl], width = 2
          ),
          legendgroup = fl,
          showlegend  = (s == 1L),
          hovertemplate = paste0(
            fl, " age %{x}: %{y:.3f}",
            "<extra></extra>"
          )
        )
      }

      lay <- .mv_layout()
      lay$annotations <- list(
        mv_top_label(label = sx)
      )
      lay$xaxis$title <- xlab
      if (s == 1L) lay$yaxis$title <- ylab

      p <- do.call(
        what = layout,
        args = c(list(p = p), lay)
      )
      plot_list[[s]] <- p
    }

    p_out <- subplot(
      plot_list,
      nrows  = 1,
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE
    )
    if (!is.null(title)) {
      p_out <- layout(
        p = p_out,
        title = title
      )
    }
    return(.mv_config(p = p_out))
  }

  # Single panel: average across sexes or combined
  p <- plot_ly()
  for (fl in fleet_names) {
    fl_sub <- df[df$fleet == fl, , drop = FALSE]
    # Average across sexes
    avg <- tapply(
      X = fl_sub$est, INDEX = fl_sub$age,
      FUN = mean, na.rm = TRUE
    )
    p <- add_lines(
      p = p,
      x    = as.numeric(names(avg)),
      y    = as.numeric(avg),
      name = fl,
      line = list(color = colours[fl], width = 2),
      hovertemplate = paste0(
        fl, " age %{x}: %{y:.3f}<extra></extra>"
      )
    )
  }

  lay <- .mv_layout()
  lay$xaxis$title <- xlab
  lay$yaxis$title <- ylab
  if (!is.null(title)) lay$title <- title
  lay$showlegend <- TRUE

  p <- do.call(
    what = layout,
    args = c(list(p = p), lay)
  )

  .mv_config(p = p)
}
