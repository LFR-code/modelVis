# plot_comp.R
# Composition fit plots (age/length compositions).
# Convention: grey bars = observed, coloured points +
# line = predicted (matches SABplots.R plotCompFitYrs).

#' Single composition fit plot
#'
#' Creates a plotly plot showing observed (grey bars) and
#' predicted (coloured points + line) proportions at age
#' or length for a single year/fleet combination.
#'
#' @param df A data.frame with columns for the bin
#'   dimension (\code{age} or \code{length}), \code{obs},
#'   and \code{pred}.
#' @param bin_col Name of the bin column ("age" or
#'   "length").
#' @param ylab Y-axis label.
#' @param title Plot title.
#' @param pred_colour Colour for predicted points + line.
#' @return A plotly object.
#' @export
mv_plot_comp_fit <- function(df, bin_col = "age",
                             ylab = "Proportion",
                             title = NULL,
                             pred_colour = "#1f77b4") {
  p <- plot_ly(
    data = df,
    x = as.formula(paste0("~", bin_col))
  )

  # Observed: grey bars
  p <- add_bars(
    p = p,
    y      = ~obs,
    name   = "Observed",
    marker = list(
      color = "rgba(180,180,180,0.6)",
      line  = list(color = "white", width = 0.5)
    ),
    hovertemplate = paste0(
      bin_col, " %{x}: %{y:.4f}<extra>Obs</extra>"
    )
  )

  # Predicted: coloured points + line. Hovertemplate lives on the marker
  # trace (sized up from a decorative 4px dot to a 7px hit target) --
  # see mv_plot_comp_grid() for why this beats adding a separate
  # invisible marker trace.
  p <- add_lines(
    p = p,
    y    = ~pred,
    name = "Predicted",
    line = list(color = pred_colour, width = 1.5),
    hoverinfo = "skip"
  )
  p <- add_markers(
    p = p,
    y    = ~pred,
    name = "Predicted",
    marker = list(
      color = pred_colour, size = 7,
      line = list(color = "white", width = 0.5)
    ),
    showlegend = FALSE,
    hovertemplate = paste0(
      bin_col, " %{x}: %{y:.4f}<extra>Pred</extra>"
    )
  )

  lay <- .mv_layout()
  lay$xaxis$title <- bin_col
  lay$yaxis$title <- ylab
  if (!is.null(title)) lay$title <- title
  lay$showlegend <- FALSE
  lay$barmode <- "overlay"

  p <- do.call(
    what = layout,
    args = c(list(p = p), lay)
  )

  .mv_config(p = p)
}


#' Grid of composition fits across years (or year-quarters)
#'
#' Creates a subplot grid showing comp fits for multiple
#' periods. Observed proportions shown as grey bars,
#' predicted as coloured points + line.
#'
#' @param df A data.frame with columns for the bin
#'   dimension, a period column, \code{obs}, \code{pred}.
#' @param bin_col Name of the bin column.
#' @param years_per_page Number of periods to show.
#'   Defaults to \code{Inf} (show all).
#' @param ncol Number of subplot columns.
#' @param ylab Y-axis label.
#' @param pred_colour Colour for predicted points + line.
#' @param year_col Name of the period column (default
#'   \code{"year"}; pass \code{"year_q"} for quarterly).
#' @return A plotly object.
#' @export
mv_plot_comp_grid <- function(df, bin_col = "age",
                              years_per_page = Inf,
                              ncol = 4,
                              ylab = "Proportion",
                              pred_colour = "#1f77b4",
                              year_col = "year") {
  all_years <- sort(unique(df[[year_col]]))
  n_years <- length(all_years)

  show_years <- if (is.infinite(years_per_page) || n_years <= years_per_page) {
    all_years
  } else {
    tail(x = all_years, n = years_per_page)
  }

  plot_list <- vector(
    mode = "list", length = length(show_years)
  )
  for (i in seq_along(show_years)) {
    yr <- show_years[i]
    sub <- df[df[[year_col]] == yr, , drop = FALSE]
    sub <- sub[order(sub[[bin_col]]), , drop = FALSE]

    p <- plot_ly(
      data = sub,
      x = as.formula(paste0("~", bin_col))
    )

    # Grey bars for observed
    p <- add_bars(
      p = p, y = ~obs,
      marker = list(
        color = "rgba(180,180,180,0.6)",
        line = list(color = "white", width = 0.3)
      ),
      showlegend = FALSE,
      hovertemplate = paste0(
        yr, " ", bin_col, " %{x}: %{y:.4f}",
        "<extra>Obs</extra>"
      )
    )

    # Coloured points + line for predicted. The hovertemplate lives on the
    # marker trace, sized up a little from a purely decorative dot (3px)
    # to a real hit target (7px) -- cheaper than a separate invisible
    # marker trace (which duplicates the x/y data per panel and roughly
    # doubled dashboard file size across a full length-comp grid).
    p <- add_lines(
      p = p, y = ~pred,
      line = list(
        color = pred_colour, width = 1.5
      ),
      showlegend = FALSE,
      hoverinfo = "skip"
    )
    p <- add_markers(
      p = p, y = ~pred,
      marker = list(
        color = pred_colour, size = 7,
        line = list(color = "white", width = 0.3)
      ),
      showlegend = FALSE,
      hovertemplate = paste0(
        yr, " ", bin_col, " %{x}: %{y:.4f}",
        "<extra>Pred</extra>"
      )
    )

    lay <- .mv_layout()
    lay$annotations <- list(list(
      text      = as.character(yr),
      x         = 0.02,
      y         = 0.97,
      xref      = "paper",
      yref      = "paper",
      xanchor   = "left",
      yanchor   = "top",
      showarrow = FALSE,
      font      = list(size = 9),
      bgcolor   = "rgba(255,255,255,0.6)",
      borderpad = 2L
    ))
    lay$barmode <- "overlay"
    lay$margin <- list(t = 5, r = 5, b = 5, l = 5)
    p <- do.call(
      what = layout,
      args = c(list(p = p), lay)
    )
    plot_list[[i]] <- p
  }

  nrow_sub <- ceiling(length(show_years) / ncol)
  p_out <- subplot(
    plot_list,
    nrows  = nrow_sub,
    shareX = TRUE,
    shareY = TRUE,
    titleX = FALSE,
    titleY = FALSE
  )
  p_out <- layout(
    p = p_out,
    annotations = list(
      mv_bottom_label(label = bin_col),
      mv_left_label(label = ylab)
    )
  )

  .mv_config(p = p_out)
}


#' Average composition fit
#'
#' Creates a plot of mean observed and predicted
#' composition across all years. Grey bars for observed,
#' coloured points + line for predicted.
#'
#' @param df A data.frame with bin, \code{obs}, \code{pred}
#'   columns.
#' @param bin_col Name of the bin column.
#' @param ylab Y-axis label.
#' @param title Plot title.
#' @param pred_colour Colour for predicted.
#' @return A plotly object.
#' @export
mv_plot_comp_avg <- function(df, bin_col = "age",
                             ylab = "Mean proportion",
                             title = NULL,
                             pred_colour = "#1f77b4") {
  bins <- sort(unique(df[[bin_col]]))
  avg_obs <- tapply(
    X = df$obs, INDEX = df[[bin_col]], FUN = mean,
    na.rm = TRUE
  )
  avg_pred <- tapply(
    X = df$pred, INDEX = df[[bin_col]], FUN = mean,
    na.rm = TRUE
  )

  avg_df <- data.frame(
    bin  = bins,
    obs  = as.numeric(avg_obs[as.character(bins)]),
    pred = as.numeric(avg_pred[as.character(bins)]),
    stringsAsFactors = FALSE
  )
  names(avg_df)[1] <- bin_col

  mv_plot_comp_fit(
    df = avg_df, bin_col = bin_col,
    ylab = ylab, title = title,
    pred_colour = pred_colour
  )
}
