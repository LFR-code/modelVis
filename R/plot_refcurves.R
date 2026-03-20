# plot_refcurves.R
# Equilibrium yield and biomass reference curves.

#' Plot equilibrium reference curves
#'
#' Creates two vertically stacked plotly panels showing
#' equilibrium yield (top) and spawning biomass (bottom)
#' as a function of legal harvest rate, with shared
#' x-axis. Reference point values are annotated in the
#' top-right of the yield panel.
#'
#' @param ref_curves A data.frame from \code{mv$ref_curves}
#'   with columns \code{legalU}, \code{yield},
#'   \code{biomass}.
#' @param ref_points A list from \code{mv$ref_points} with
#'   \code{lUmsy}, \code{lMSY}, \code{SBmsy}, etc.
#' @param xlab X-axis label.
#' @param title Plot title.
#' @return A plotly object.
#' @export
mv_plot_refcurves <- function(ref_curves,
                              ref_points = NULL,
                              xlab = "Legal harvest rate",
                              title = NULL) {
  cols <- mv_colours(n = 2)

  # Filter infeasible solutions (negative yield/biomass)
  feasible <- ref_curves$yield >= 0 &
    ref_curves$biomass >= 0 &
    ref_curves$legalU >= 0
  ref_curves <- ref_curves[feasible, , drop = FALSE]

  # --- Top panel: yield vs legalU ---
  p_yield <- plot_ly()
  p_yield <- add_lines(
    p = p_yield,
    x    = ref_curves$legalU,
    y    = ref_curves$yield,
    name = "Yield",
    line = list(color = cols[1], width = 2),
    hovertemplate = paste0(
      "U: %{x:.4f}<br>Yield: %{y:.3g}",
      "<extra></extra>"
    )
  )

  # Umsy vertical line on yield panel
  yield_shapes <- list()
  if (!is.null(ref_points) && !is.null(ref_points$lUmsy)) {
    yield_shapes <- list(list(
      type = "line",
      x0 = ref_points$lUmsy, x1 = ref_points$lUmsy,
      y0 = 0, y1 = 1, yref = "paper",
      line = list(
        color = "#999999", width = 1.5, dash = "dash"
      )
    ))
  }

  # Reference point annotation block (top-right)
  rp_text <- ""
  if (!is.null(ref_points)) {
    rp_lines <- character(0)
    fmt <- function(nm, val) {
      paste0(nm, " = ", formatC(
        x = val, digits = 4, format = "g"
      ))
    }
    if (!is.null(ref_points$lUmsy)) {
      rp_lines <- c(rp_lines,
                     fmt("Umsy", ref_points$lUmsy))
    }
    if (!is.null(ref_points$lMSY)) {
      rp_lines <- c(rp_lines,
                     fmt("MSY", ref_points$lMSY))
    }
    if (!is.null(ref_points$SBmsy)) {
      rp_lines <- c(rp_lines,
                     fmt("SBmsy", ref_points$SBmsy))
    }
    if (!is.null(ref_points$lBmsy)) {
      rp_lines <- c(rp_lines,
                     fmt("lBmsy", ref_points$lBmsy))
    }
    if (!is.null(ref_points$Fmsy)) {
      rp_lines <- c(rp_lines,
                     fmt("Fmsy", ref_points$Fmsy))
    }
    if (!is.null(ref_points$B0)) {
      rp_lines <- c(rp_lines,
                     fmt("B0", ref_points$B0))
    }
    if (!is.null(ref_points$R0)) {
      rp_lines <- c(rp_lines,
                     fmt("R0", ref_points$R0))
    }
    rp_text <- paste(rp_lines, collapse = "<br>")
  }

  yield_annotations <- list()
  if (nchar(rp_text) > 0) {
    yield_annotations <- list(list(
      x         = 0.98,
      y         = 0.98,
      xref      = "paper",
      yref      = "paper",
      text      = rp_text,
      showarrow = FALSE,
      xanchor   = "right",
      yanchor   = "top",
      align     = "right",
      font      = list(size = 10, family = "monospace"),
      bgcolor   = "rgba(255,255,255,0.8)",
      bordercolor = "#cccccc",
      borderwidth = 1
    ))
  }

  lay_yield <- .mv_layout()
  lay_yield$yaxis$title <- "Equilibrium yield"
  lay_yield$shapes <- yield_shapes
  lay_yield$annotations <- yield_annotations
  lay_yield$showlegend <- FALSE

  p_yield <- do.call(
    what = layout,
    args = c(list(p = p_yield), lay_yield)
  )

  # --- Bottom panel: SSB vs legalU ---
  p_bio <- plot_ly()
  p_bio <- add_lines(
    p = p_bio,
    x    = ref_curves$legalU,
    y    = ref_curves$biomass,
    name = "Spawning biomass",
    line = list(color = cols[2], width = 2),
    hovertemplate = paste0(
      "U: %{x:.4f}<br>SSB: %{y:.3g}",
      "<extra></extra>"
    )
  )

  # Umsy vertical line on biomass panel
  bio_shapes <- list()
  if (!is.null(ref_points) && !is.null(ref_points$lUmsy)) {
    bio_shapes <- list(list(
      type = "line",
      x0 = ref_points$lUmsy, x1 = ref_points$lUmsy,
      y0 = 0, y1 = 1, yref = "paper",
      line = list(
        color = "#999999", width = 1.5, dash = "dash"
      )
    ))
  }

  lay_bio <- .mv_layout()
  lay_bio$xaxis$title <- xlab
  lay_bio$yaxis$title <- "Spawning biomass"
  lay_bio$shapes <- bio_shapes
  lay_bio$showlegend <- FALSE

  p_bio <- do.call(
    what = layout,
    args = c(list(p = p_bio), lay_bio)
  )

  # --- Stack vertically ---
  p_out <- subplot(
    p_yield, p_bio,
    nrows  = 2,
    shareX = TRUE,
    titleX = TRUE,
    titleY = TRUE,
    margin = 0.08
  )

  if (!is.null(title)) {
    p_out <- layout(p = p_out, title = title)
  }

  .mv_config(p = p_out)
}
