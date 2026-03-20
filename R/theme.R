# theme.R
# Colour palettes and theming for modelVis plotly plots.

#' modelVis default colour palette
#'
#' Returns a vector of colours for general use in plots.
#'
#' @param n Number of colours needed. If NULL, returns the
#'   full palette.
#' @return Character vector of hex colour codes.
#' @export
mv_colours <- function(n = NULL) {
  pal <- c(
    "#1f77b4",
    "#ff7f0e",
    "#2ca02c",
    "#d62728",
    "#9467bd",
    "#8c564b",
    "#e377c2",
    "#7f7f7f",
    "#bcbd22",
    "#17becf"
  )
  if (is.null(n)) return(pal)
  if (n <= length(pal)) return(pal[seq_len(n)])
  # Interpolate if more colours needed
  colorRampPalette(colors = pal)(n)
}


#' Fleet colour palette
#'
#' Returns a named vector of colours for fleets.
#'
#' @param fleet_names Character vector of fleet names.
#' @return Named character vector of hex colours.
#' @export
mv_fleet_colours <- function(fleet_names) {
  cols <- mv_colours(n = length(fleet_names))
  names(cols) <- fleet_names
  cols
}


#' Sex colour palette
#'
#' Returns a named vector of colours for sexes.
#'
#' @param sex_names Character vector of sex names.
#' @return Named character vector of hex colours.
#' @export
mv_sex_colours <- function(sex_names) {
  # Standard sex colours
  sex_pal <- c(
    Male     = "#4393c3",
    Female   = "#d6604d",
    Combined = "#555555"
  )
  out <- character(length(sex_names))
  names(out) <- sex_names
  for (i in seq_along(sex_names)) {
    nm <- sex_names[i]
    if (nm %in% names(sex_pal)) {
      out[i] <- sex_pal[nm]
    } else {
      out[i] <- mv_colours(n = length(sex_names))[i]
    }
  }
  out
}


#' Positive/negative residual colours
#'
#' @return Named list with \code{pos} and \code{neg} colours.
#' @keywords internal
.resid_colours <- function() {
  list(pos = "#d62728", neg = "#1f77b4")
}


#' Status zone colours for Kobe plot
#'
#' @return Named character vector with quadrant colours.
#' @export
mv_kobe_colours <- function() {
  c(
    green  = "#2ca02c80",
    yellow_right = "#ffff0060",
    yellow_top   = "#ffff0060",
    red    = "#d6272880"
  )
}

# Internal alias
.kobe_colours <- mv_kobe_colours


#' Standard plotly layout options
#'
#' Returns a list of layout settings for consistent
#' appearance across plots.
#'
#' @return A list suitable for \code{plotly::layout()}.
#' @export
mv_layout <- function() {
  list(
    font = list(family = "Arial, sans-serif", size = 12),
    paper_bgcolor = "white",
    plot_bgcolor  = "white",
    margin = list(t = 40, r = 20, b = 60, l = 60),
    xaxis = list(
      gridcolor = "#eeeeee",
      zerolinecolor = "#cccccc"
    ),
    yaxis = list(
      gridcolor = "#eeeeee",
      zerolinecolor = "#cccccc"
    )
  )
}

# Internal alias
.mv_layout <- mv_layout
