# mv_object.R
# S3 class constructor, validator, print, and summary methods
# for the standardised "mv" model visualisation object.

#' Create a new mv object
#'
#' Constructs an S3 object of class \code{"mv"} containing
#' standardised model output for visualisation. Called by
#' \code{extract()} methods; not typically called directly.
#'
#' @param meta List of model metadata (type, label, years,
#'   ages, sex_names, fleet_names, area_names, flags).
#' @param ... Named data.frame components (spawning_biomass,
#'   recruitment, index_fits, etc.). NULL components are
#'   permitted and cause their dashboard sections to be
#'   skipped.
#' @return An object of class \code{"mv"}.
#' @export
new_mv <- function(meta, ...) {
  components <- list(...)
  # Validate meta
  required_meta <- c(
    "model_type", "label", "years", "ages",
    "sex_names", "fleet_names", "area_names"
  )
  missing <- setdiff(required_meta, names(meta))
  if (length(missing) > 0L) {
    stop(
      "meta is missing required fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  obj <- c(list(meta = meta), components)
  class(obj) <- "mv"
  .validate_mv(x = obj)
  obj
}


#' Validate an mv object
#'
#' Checks that required columns are present in data.frame
#' components.
#' @param x An mv object.
#' @return Invisible TRUE if valid; otherwise stops with error.
#' @keywords internal
.validate_mv <- function(x) {
  # Time-series components must have year + est
  ts_names <- c(
    "spawning_biomass", "legal_biomass",
    "sublegal_biomass", "total_biomass",
    "recruitment", "harvest_rate",
    "harvest_rate_secondary",
    "legal_hr"
  )
  for (nm in ts_names) {
    df <- x[[nm]]
    if (is.null(df)) next
    if (!is.data.frame(df)) {
      stop(nm, " must be a data.frame", call. = FALSE)
    }
    req <- c("year", "est")
    miss <- setdiff(req, names(df))
    if (length(miss) > 0L) {
      stop(
        nm, " is missing columns: ",
        paste(miss, collapse = ", "),
        call. = FALSE
      )
    }
  }
  # Fit components must have year + obs + pred

  fit_names <- c("index_fits", "catch_fits", "discard_fits")
  for (nm in fit_names) {
    df <- x[[nm]]
    if (is.null(df)) next
    if (!is.data.frame(df)) {
      stop(nm, " must be a data.frame", call. = FALSE)
    }
    req <- c("year", "obs", "pred")
    miss <- setdiff(req, names(df))
    if (length(miss) > 0L) {
      stop(
        nm, " is missing columns: ",
        paste(miss, collapse = ", "),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}


#' @export
print.mv <- function(x, ...) {
  cat("modelVis object (class 'mv')\n")
  cat("  Model type :", x$meta$model_type, "\n")
  cat("  Label      :", x$meta$label, "\n")
  cat("  Years      :",
      min(x$meta$years), "-", max(x$meta$years), "\n")
  cat("  Ages       :",
      min(x$meta$ages), "-", max(x$meta$ages), "\n")
  cat("  Sexes      :",
      paste(x$meta$sex_names, collapse = ", "), "\n")
  cat("  Fleets     :",
      paste(x$meta$fleet_names, collapse = ", "), "\n")
  cat("  Areas      :",
      paste(x$meta$area_names, collapse = ", "), "\n")
  # List available components
  comps <- setdiff(names(x), "meta")
  non_null <- comps[!vapply(
    X = x[comps], FUN = is.null, FUN.VALUE = logical(1)
  )]
  cat("  Components :", length(non_null), "of",
      length(comps), "populated\n")
  invisible(x)
}


#' @export
summary.mv <- function(object, ...) {
  cat("modelVis object summary\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat("Model type :", object$meta$model_type, "\n")
  cat("Label      :", object$meta$label, "\n")
  cat("Years      :",
      min(object$meta$years), "-",
      max(object$meta$years), "\n")
  cat("Ages       :",
      min(object$meta$ages), "-",
      max(object$meta$ages), "\n")
  cat("Sexes      :",
      paste(object$meta$sex_names, collapse = ", "), "\n")
  cat("Fleets     :",
      paste(object$meta$fleet_names, collapse = ", "), "\n")
  cat("Areas      :",
      paste(object$meta$area_names, collapse = ", "), "\n\n")
  # Component summary table
  comps <- setdiff(names(object), "meta")
  cat("Components:\n")
  for (nm in comps) {
    val <- object[[nm]]
    if (is.null(val)) {
      status <- "[NULL - skipped]"
    } else if (is.data.frame(val)) {
      status <- paste0(
        "[", nrow(val), " rows x ",
        ncol(val), " cols]"
      )
    } else if (is.list(val)) {
      status <- paste0(
        "[list: ", length(val), " elements]"
      )
    } else {
      status <- paste0("[", class(val)[1], "]")
    }
    cat("  ", format(nm, width = 25), status, "\n")
  }
  # Flags
  flags <- c(
    "has_length_comps", "has_discards", "has_mcmc"
  )
  cat("\nFlags:\n")
  for (fl in flags) {
    val <- object$meta[[fl]]
    if (!is.null(val)) {
      cat("  ", format(fl, width = 25),
          ifelse(val, "yes", "no"), "\n")
    }
  }
  invisible(object)
}
