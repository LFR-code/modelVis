# plot_comp.R
# Composition fit plots (age/length compositions).

#' Single composition fit plot
#'
#' Creates a plotly plot showing observed (bars) and
#' predicted (line) proportions at age or length for a
#' single year/fleet combination.
#'
#' @param df A data.frame with columns for the bin
#'   dimension (\code{age} or \code{length}), \code{obs},
#'   and \code{pred}.
#' @param bin_col Name of the bin column ("age" or
#'   "length").
#' @param ylab Y-axis label.
#' @param title Plot title.
#' @param obs_colour Colour for observed bars.
#' @param pred_colour Colour for predicted line.
#' @return A plotly object.
#' @export
mv_plot_comp_fit <- function(df, bin_col = "age",
                             ylab = "Proportion",
                             title = NULL,
                             obs_colour = "#1f77b4",
                             pred_colour = "#d62728") {
  p <- plot_ly(
    data = df,
    x = as.formula(paste0("~", bin_col))
  )

  # Observed bars
  p <- add_bars(
    p = p,
    y      = ~obs,
    name   = "Observed",
    marker = list(
      color = obs_colour,
      line  = list(color = "white", width = 0.5)
    ),
    hovertemplate = paste0(
      bin_col, " %{x}: %{y:.4f}<extra>Obs</extra>"
    )
  )

  # Predicted line
  p <- add_lines(
    p = p,
    y    = ~pred,
    name = "Predicted",
    line = list(color = pred_colour, width = 2),
    hovertemplate = paste0(
      bin_col, " %{x}: %{y:.4f}<extra>Pred</extra>"
    )
  )

  lay <- .mv_layout()
  lay$xaxis$title <- bin_col
  lay$yaxis$title <- ylab
  if (!is.null(title)) lay$title <- title
  lay$showlegend <- TRUE
  lay$barmode <- "overlay"

  p <- do.call(
    what = layout,
    args = c(list(p = p), lay)
  )

  .mv_config(p = p)
}


#' Grid of composition fits across years
#'
#' Creates a subplot grid showing comp fits for multiple
#' years, with year animation buttons.
#'
#' @param df A data.frame with columns for the bin
#'   dimension, \code{year}, \code{obs}, \code{pred}.
#' @param bin_col Name of the bin column.
#' @param years_per_page Number of years to show per page.
#' @param ncol Number of subplot columns.
#' @param ylab Y-axis label.
#' @return A plotly object.
#' @export
mv_plot_comp_grid <- function(df, bin_col = "age",
                              years_per_page = 12,
                              ncol = 4,
                              ylab = "Proportion") {
  all_years <- sort(unique(df$year))
  n_years <- length(all_years)

  # If too many years, show subset with slider
  show_years <- if (n_years <= years_per_page) {
    all_years
  } else {
    # Show last years_per_page years
    tail(x = all_years, n = years_per_page)
  }

  plot_list <- vector(
    mode = "list", length = length(show_years)
  )
  for (i in seq_along(show_years)) {
    yr <- show_years[i]
    sub <- df[df$year == yr, , drop = FALSE]

    p <- plot_ly(
      data = sub,
      x = as.formula(paste0("~", bin_col))
    )
    p <- add_bars(
      p = p, y = ~obs,
      marker = list(color = "#1f77b4"),
      showlegend = FALSE,
      hovertemplate = paste0(
        yr, " ", bin_col, " %{x}: %{y:.4f}",
        "<extra>Obs</extra>"
      )
    )
    p <- add_lines(
      p = p, y = ~pred,
      line = list(color = "#d62728", width = 1.5),
      showlegend = FALSE,
      hovertemplate = paste0(
        yr, " ", bin_col, " %{x}: %{y:.4f}",
        "<extra>Pred</extra>"
      )
    )

    lay <- .mv_layout()
    lay$annotations <- list(
      mv_top_label(label = as.character(yr))
    )
    lay$barmode <- "overlay"
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
#' composition across all years.
#'
#' @param df A data.frame with bin, \code{obs}, \code{pred}
#'   columns.
#' @param bin_col Name of the bin column.
#' @param ylab Y-axis label.
#' @param title Plot title.
#' @return A plotly object.
#' @export
mv_plot_comp_avg <- function(df, bin_col = "age",
                             ylab = "Mean proportion",
                             title = NULL) {
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
    ylab = ylab, title = title
  )
}
