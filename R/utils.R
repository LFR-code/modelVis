# utils.R
# Exported utility functions for modelVis extractors.

#' Check if a component has data
#'
#' Returns TRUE if the object is non-NULL and, for arrays,
#' has at least one non-zero non-NA value.
#'
#' @param x An array, matrix, vector, or NULL.
#' @return Logical.
#' @export
mv_has_data <- function(x) {
  if (is.null(x)) return(FALSE)
  if (is.numeric(x)) {
    return(any(!is.na(x) & x != 0))
  }
  TRUE
}


#' Make a CI data.frame from MCMC quantiles
#'
#' Computes point estimate and credible interval from a
#' matrix of posterior samples (iterations x time).
#'
#' @param x Matrix with rows = iterations, cols = time
#'   steps.
#' @param years Integer vector of year labels.
#' @param probs Numeric vector of length 3: lower CI,
#'   point estimate (median), upper CI. Default
#'   \code{c(0.025, 0.5, 0.975)}.
#' @return A data.frame with year, est, lwr, upr.
#' @export
mv_quantile_ts <- function(x, years,
                           probs = c(0.025, 0.5, 0.975)) {
  med_prob <- if (length(probs) >= 2) probs[2] else 0.5
  lwr_prob <- probs[1]
  upr_prob <- probs[length(probs)]

  est <- apply(X = x, MARGIN = 2, FUN = quantile,
               probs = med_prob)
  lwr <- apply(X = x, MARGIN = 2, FUN = quantile,
               probs = lwr_prob)
  upr <- apply(X = x, MARGIN = 2, FUN = quantile,
               probs = upr_prob)
  data.frame(
    year = years,
    est  = as.numeric(est),
    lwr  = as.numeric(lwr),
    upr  = as.numeric(upr),
    stringsAsFactors = FALSE
  )
}


#' Create a year sequence from model dimensions
#'
#' @param fYear First year (integer).
#' @param nT Number of time steps.
#' @return Integer vector of years.
#' @export
mv_year_seq <- function(fYear, nT) {
  seq.int(from = fYear, length.out = nT)
}


#' Add CI columns if missing
#'
#' Ensures lwr and upr columns exist, setting to NA if
#' not already present.
#'
#' @param df A data.frame.
#' @return Modified data.frame with lwr/upr columns.
#' @export
mv_ensure_ci <- function(df) {
  if (!"lwr" %in% names(df)) df$lwr <- NA_real_
  if (!"upr" %in% names(df)) df$upr <- NA_real_
  df
}


#' Format numbers for display
#'
#' @param x Numeric vector.
#' @param digits Number of significant digits.
#' @return Character vector.
#' @keywords internal
.fmt <- function(x, digits = 3) {
  formatC(
    x = x, digits = digits, format = "g",
    big.mark = ","
  )
}


#' Collapse single-stock dimension
#'
#' For single-stock models, drops the area column and sets
#' area to "All" in the standardised output.
#'
#' @param df A data.frame with an \code{area} column.
#' @param single_area Logical; if TRUE, set area to "All".
#' @return Modified data.frame.
#' @keywords internal
.collapse_area <- function(df, single_area = TRUE) {
  if (single_area && "area" %in% names(df)) {
    df$area <- "All"
  }
  df
}
