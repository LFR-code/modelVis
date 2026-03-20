# plot_obs_pred.R
# Observed vs predicted plots for indices and catches.

#' Plot observed vs predicted time series
#'
#' Creates a plotly plot with observed markers and
#' predicted lines.
#'
#' @param df A data.frame with columns \code{year},
#'   \code{obs}, \code{pred}.
#' @param ylab Y-axis label.
#' @param xlab X-axis label.
#' @param title Plot title.
#' @param obs_colour Colour for observed markers.
#' @param pred_colour Colour for predicted line.
#' @param log_buttons Logical; add log/linear toggle.
#' @return A plotly object.
#' @export
mv_plot_obs_pred <- function(df, ylab = "", xlab = "Year",
                             title = NULL,
                             obs_colour = "#1f77b4",
                             pred_colour = "#ff7f0e",
                             log_buttons = FALSE) {
  p <- plot_ly(data = df, x = ~year)

  # Predicted line
  p <- add_lines(
    p = p,
    y     = ~pred,
    name  = "Predicted",
    line  = list(color = pred_colour, width = 2),
    hovertemplate = paste0(
      "Predicted %{x}: %{y:.3g}<extra></extra>"
    )
  )

  # Observed markers (filter NAs)
  obs_df <- df[!is.na(df$obs) & df$obs > 0, , drop = FALSE]
  if (nrow(obs_df) > 0L) {
    p <- add_markers(
      p = p,
      data  = obs_df,
      x     = ~year,
      y     = ~obs,
      name  = "Observed",
      marker = list(
        color = obs_colour, size = 6,
        line = list(color = "white", width = 0.5)
      ),
      hovertemplate = paste0(
        "Observed %{x}: %{y:.3g}<extra></extra>"
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


#' Faceted observed vs predicted plots
#'
#' Creates a subplot stack of obs vs pred plots, one per
#' fleet or group, arranged in a single column with
#' shared x-axis for year alignment.
#'
#' @param df A data.frame with columns \code{year},
#'   \code{obs}, \code{pred}, and a facet column.
#' @param facet Name of the faceting column.
#' @param ylab Y-axis label.
#' @param ncol Number of subplot columns.
#' @return A plotly object.
#' @export
mv_plot_obs_pred_facet <- function(df, facet = "fleet",
                                   ylab = "",
                                   ncol = 1) {
  facet_levels <- unique(df[[facet]])
  n_facets <- length(facet_levels)
  cols <- mv_colours(n = n_facets)

  plot_list <- vector(mode = "list", length = n_facets)
  for (i in seq_along(facet_levels)) {
    fl <- facet_levels[i]
    sub <- df[df[[facet]] == fl, , drop = FALSE]

    p <- plot_ly(data = sub, x = ~year)

    # Predicted
    p <- add_lines(
      p = p,
      y    = ~pred,
      name = fl,
      line = list(color = cols[i], width = 2),
      legendgroup = fl,
      showlegend = TRUE,
      hovertemplate = paste0(
        fl, " Pred %{x}: %{y:.3g}<extra></extra>"
      )
    )

    # Observed
    obs_sub <- sub[!is.na(sub$obs) & sub$obs > 0,
                   , drop = FALSE]
    if (nrow(obs_sub) > 0L) {
      p <- add_markers(
        p = p,
        data  = obs_sub,
        x     = ~year,
        y     = ~obs,
        name  = paste(fl, "Obs"),
        marker = list(
          color = cols[i], size = 5,
          line = list(color = "white", width = 0.5)
        ),
        legendgroup = fl,
        showlegend = FALSE,
        hovertemplate = paste0(
          fl, " Obs %{x}: %{y:.3g}<extra></extra>"
        )
      )
    }

    lay <- .mv_layout()
    lay$annotations <- list(mv_top_label(label = fl))
    lay$yaxis$title <- ylab

    p <- do.call(
      what = layout,
      args = c(list(p = p), lay)
    )
    plot_list[[i]] <- p
  }

  nrow_sub <- ceiling(n_facets / ncol)
  p_out <- subplot(
    plot_list,
    nrows      = nrow_sub,
    shareX     = TRUE,
    shareY     = FALSE,
    titleX     = FALSE,
    titleY     = TRUE
  )

  .mv_config(p = p_out)
}
