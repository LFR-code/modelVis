# plot_ec_check.R
# The Exceptional Circumstances check: does real, observed data still
# fall inside the range a closed-loop simulation projected for it.
# Plotly, like every other mv_plot_*() in this package -- interactive,
# consistent with the rest of the dashboard. (A separate, static
# figure that matches the FSRR's own Figure 5 as closely as possible
# is sableMP2026's own plotECCheck(), not this function -- see that
# package's R/ecCheck.R. This one is the generic, reusable dashboard
# panel; that one is a one-off match to an external document's
# specific look, and belongs with the model package, not modelVis.)

ec_line_pal   <- c("#444444", "#7b5ea8")
ec_ribbon_pal <- c("rgba(100,100,100,0.30)", "rgba(123,94,168,0.25)")


# .mv_ec_catch_panel()
# A genuine simulation envelope panel: a grey ribbon, a solid median
# line, and filled/open points before/after tMPyear. Handles more
# than one named series (a `series` column in df) for a model with
# more than one such quantity to check.
.mv_ec_catch_panel <- function(df, ytitle, series_col = NULL, panel_id = "",
                               default_label = "Simulated", tMPyear = NULL,
                               yaxis_suffix = "", show_legend = TRUE) {
  # subplot() merges every panel's traces into one figure, so
  # legendgroup ids must be unique across panels, not just within one.
  # Envelope and median share their series' legendgroup but stay out
  # of the legend itself; the pre/post split is the whole point of
  # this check, so it keeps its own legend entries.
  p <- plot_ly()
  groups <- if (!is.null(series_col) && series_col %in% names(df))
    unique(df[[series_col]]) else list(NULL)

  # An always-shown "YYYY+" legend entry, before any such data exists,
  # needs a trace with no visible point. add_markers() with NA x/y
  # ("Must supply x and y attributes" for length-0; a mangled,
  # unnamed trace for a single NA row -- both confirmed by direct
  # testing) can't do this reliably. A real, off-canvas point can: an
  # explicit x-axis range fixes the visible window to the data's own
  # span, and a point placed just outside it is a normal, fully-valid
  # trace, so its legend entry renders with the correct marker style
  # while staying invisible on the plot itself.
  yrRange <- range(df$year, na.rm = TRUE)
  pad     <- diff(yrRange) * 0.04
  xRange  <- c(yrRange[1] - pad, yrRange[2] + pad)
  offCanvasX <- yrRange[1] - pad - 1

  for (gi in seq_along(groups)) {
    g   <- groups[[gi]]
    sub <- if (!is.null(g)) df[df[[series_col]] == g, , drop = FALSE] else df
    sub <- sub[order(sub$year), , drop = FALSE]
    lbl <- if (!is.null(g)) g else default_label
    lp  <- if (!is.null(g)) paste0(lbl, " ") else ""
    col <- ec_line_pal[((gi - 1) %% length(ec_line_pal)) + 1]
    fil <- ec_ribbon_pal[((gi - 1) %% length(ec_ribbon_pal)) + 1]
    grp <- paste0("ec_", panel_id, "_", gi)

    if (any(!is.na(sub$lwr)))
      p <- add_ribbons(p = p, x = sub$year, ymin = sub$lwr, ymax = sub$upr,
        line = list(width = 0), fillcolor = fil,
        legendgroup = grp, showlegend = FALSE,
        hovertemplate = paste0(lp, "envelope %{x}: %{y:.3g}<extra></extra>"))
    p <- add_lines(p = p, x = sub$year, y = sub$med,
      line = list(color = col, width = 2),
      legendgroup = grp, showlegend = FALSE,
      hovertemplate = paste0(lp, "median %{x}: %{y:.3g}<extra></extra>"))

    obs_sub <- sub[!is.na(sub$obs), , drop = FALSE]
    op_pre  <- obs_sub[obs_sub$period == "pre", , drop = FALSE]
    op_post <- obs_sub[obs_sub$period == "post", , drop = FALSE]
    # Named by the actual reference year, matching the FSRR's own
    # "StRS index pre-2022"/"StRS index 2022+" convention. Both traces
    # are always added, even with zero rows, so the "YYYY+" legend
    # entry is visible before any post-reference-year data exists yet.
    preLbl  <- if (!is.null(tMPyear)) paste0("pre-", tMPyear) else "pre"
    postLbl <- if (!is.null(tMPyear)) paste0(tMPyear, "+") else "post"
    pre_x  <- if (nrow(op_pre) > 0) op_pre$year else offCanvasX
    pre_y  <- if (nrow(op_pre) > 0) op_pre$obs  else 0
    post_x <- if (nrow(op_post) > 0) op_post$year else offCanvasX
    post_y <- if (nrow(op_post) > 0) op_post$obs  else 0
    p <- add_markers(p = p, x = pre_x, y = pre_y,
      marker = list(color = col, size = 6, symbol = "circle"),
      name = paste0(lp, "observed (", preLbl, ")"),
      legendgroup = grp, showlegend = show_legend,
      hovertemplate = paste0(lp, "observed %{x}: %{y:.3g}<extra></extra>"))
    # For a "-open" symbol, plotly draws the outline from marker.color
    # itself, not marker.line -- the series colour goes on
    # marker.color, with a thin white marker.line only to separate it
    # from an overlapping filled point at the same position.
    p <- add_markers(p = p, x = post_x, y = post_y,
      marker = list(color = col, size = 7, symbol = "circle-open",
                    line = list(color = "white", width = 1)),
      name = paste0(lp, "observed (", postLbl, ")"),
      legendgroup = grp, showlegend = show_legend,
      hovertemplate = paste0(lp, "observed %{x}: %{y:.3g}<extra></extra>"))
  }

  lay <- .mv_layout()
  lay$yaxis$title     <- ytitle
  lay$yaxis$rangemode <- "tozero"
  lay$xaxis$range      <- xRange
  lay$showlegend      <- TRUE
  # A shape added via layout() AFTER subplot() combines the panels is
  # silently dropped by plotly_build() -- confirmed by direct testing,
  # not documented behaviour. Adding it here, to each panel
  # individually before combining, survives subplot() correctly --
  # but subplot(..., shareX = TRUE) merges every row onto one shared
  # "x" axis (there is no "x2": only "y"/"y2" differ per row), so xref
  # must stay "x" for every panel.
  if (!is.null(tMPyear)) {
    # Between the last historical year and the first projection year,
    # not on top of the first projection year's own point.
    splitX <- tMPyear - 0.5
    lay$shapes <- list(list(
      type = "line", x0 = splitX, x1 = splitX, y0 = 0, y1 = 1,
      xref = "x", yref = paste0("y", yaxis_suffix, " domain"),
      line = list(color = "#333333", width = 1, dash = "dash")))
  }
  do.call(layout, c(list(p = p), lay))
} # END .mv_ec_catch_panel()


# .mv_ec_index_panel()
# The survey index panel: a single series, drawn differently in its
# historical and projection halves, because they represent different
# things -- a per-year posterior predictive interval (independent
# draws, not a smoothly-varying quantity) versus a continuous envelope
# with nothing real yet to compare it against.
.mv_ec_index_panel <- function(df, ytitle, tMPyear = NULL, yaxis_suffix = "") {
  df   <- df[order(df$year), , drop = FALSE]
  pre  <- df[!is.na(df$period) & df$period == "pre",  , drop = FALSE]
  post <- df[!is.na(df$period) & df$period == "post", , drop = FALSE]
  col  <- ec_line_pal[1]
  fil  <- ec_ribbon_pal[1]
  grp  <- "ec_idx"

  p <- plot_ly()

  # One vertical segment per historical year, all in a single trace --
  # add_segments() handles that natively, unlike add_ribbons(), which
  # would draw a continuous band implying an interpolation the
  # observation model doesn't make between independent yearly draws.
  if (nrow(pre) > 0)
    p <- add_segments(p = p, x = pre$year, xend = pre$year,
      y = pre$lwr, yend = pre$upr,
      line = list(color = col, width = 1),
      legendgroup = grp, showlegend = TRUE,
      name = "95% predictive interval",
      hovertemplate = "PPI %{x}: %{y:.3g}<extra></extra>")

  # No real observation exists yet for the projection years, so shown
  # as the usual continuous ribbon instead of per-year segments.
  if (nrow(post) > 0)
    p <- add_ribbons(p = p, x = post$year, ymin = post$lwr, ymax = post$upr,
      line = list(width = 0), fillcolor = fil,
      legendgroup = grp, showlegend = nrow(pre) == 0,
      name = "95% predictive interval",
      hovertemplate = "PPI %{x}: %{y:.3g}<extra></extra>")

  obs_sub <- pre[!is.na(pre$obs), , drop = FALSE]
  if (nrow(obs_sub) > 0)
    p <- add_markers(p = p, x = obs_sub$year, y = obs_sub$obs,
      marker = list(color = col, size = 6, symbol = "circle"),
      name = "StRS index (observed)",
      legendgroup = "ec_idx_obs", showlegend = TRUE,
      hovertemplate = "observed %{x}: %{y:.3g}<extra></extra>")

  lay <- .mv_layout()
  lay$yaxis$title     <- ytitle
  lay$yaxis$rangemode <- "tozero"
  lay$showlegend      <- TRUE
  # See .mv_ec_catch_panel()'s own comment on why xref must stay "x"
  # (the sole shared axis under subplot(..., shareX = TRUE)) while
  # yref varies.
  if (!is.null(tMPyear)) {
    splitX <- tMPyear - 0.5
    lay$shapes <- list(list(
      type = "line", x0 = splitX, x1 = splitX, y0 = 0, y1 = 1,
      xref = "x", yref = paste0("y", yaxis_suffix, " domain"),
      line = list(color = "#333333", width = 1, dash = "dash")))
  }
  do.call(layout, c(list(p = p), lay))
} # END .mv_ec_index_panel()


#' Plot the Exceptional Circumstances check
#'
#' Does real, observed data still fall inside the range a closed-loop
#' simulation projected for it. Draws a one- or two-panel figure (a
#' survey-index panel and/or a landed-catch panel, whichever are
#' supplied), interactive plotly like every other panel in this
#' dashboard.
#'
#' `ec_index`'s panel is drawn as a per-year posterior predictive
#' interval for its historical years -- a vertical segment with the
#' real observation as a filled point on top -- and as a continuous
#' ribbon for its projection years, which have no real observation yet
#' to compare against. `ec_catch`'s panel, if supplied, is a genuine
#' simulation envelope instead: a ribbon, a median line, and
#' filled/open points before/after the reference year.
#'
#' This is what `child_mp_results.Rmd`'s "Exceptional Circumstances
#' Check" tab calls internally; exported so a model package (or a
#' user with a bare `ec_index`/`ec_catch` data.frame) can generate the
#' same figure outside a rendered dashboard.
#'
#' @param ec_index A `data.frame` with `year`, `lwr`, `med`, `upr`,
#'   `obs`, `period` (`period` is `"pre"`/`"post"` a reference year) --
#'   or `NULL` to omit the index panel.
#' @param ec_catch Same shape as `ec_index`, for total landed catch, or
#'   `NULL` to omit the catch panel. May optionally carry a `series`
#'   column to plot more than one named series in this panel.
#' @param tMPyear The reference year -- the first year of the
#'   simulation's own projection. If `NULL`, inferred as the first
#'   `"post"` year in whichever of `ec_index`/`ec_catch` is supplied.
#'
#' @return A plotly object.
#'
#' @export
mv_plot_ec_check <- function(ec_index = NULL, ec_catch = NULL,
                             tMPyear = NULL) {
  if (is.null(ec_index) && is.null(ec_catch))
    stop("At least one of ec_index or ec_catch must be supplied.",
        call. = FALSE)

  if (is.null(tMPyear)) {
    if (!is.null(ec_index))
      tMPyear <- min(ec_index$year[ec_index$period == "post"])
    if (is.null(tMPyear) && !is.null(ec_catch))
      tMPyear <- min(ec_catch$year[ec_catch$period == "post"])
  }

  panels <- list()
  if (!is.null(ec_index)) panels[[length(panels) + 1]] <-
    .mv_ec_index_panel(ec_index, "StRS Survey Index", tMPyear = tMPyear,
      yaxis_suffix = if (length(panels) == 0) "" else as.character(length(panels) + 1))
  if (!is.null(ec_catch)) panels[[length(panels) + 1]] <-
    .mv_ec_catch_panel(ec_catch, "Landings (kt)", panel_id = "catch",
      default_label = "Landed catch", tMPyear = tMPyear,
      yaxis_suffix = if (length(panels) == 0) "" else as.character(length(panels) + 1),
      # One series, no ambiguity to resolve -- the legend just repeats
      # what filled/open points already show visually, so the index
      # panel's legend is the only one that earns the space.
      show_legend = FALSE)

  p_out <- subplot(panels, nrows = length(panels), shareX = TRUE,
                   titleY = TRUE, margin = 0.04)
  p_out <- layout(p_out, xaxis = list(title = "Year"))
  .mv_config(p_out)
} # END mv_plot_ec_check()
