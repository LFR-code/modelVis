# extract.R
# S3 generic for extracting standardised mv objects from
# stock assessment model output.

#' Extract standardised model output
#'
#' S3 generic that converts raw model output into a
#' standardised \code{"mv"} object for visualisation.
#' Model packages register their own methods (e.g.,
#' \code{extract.sableOpMod} in the sableOpMod package).
#' The default method attempts auto-detection for
#' unclassed objects.
#'
#' @param fit Model output object (e.g., sableOpMod
#'   reports list).
#' @param label Character label for this model run
#'   (e.g., "Base case 2025").
#' @param ... Additional arguments passed to methods.
#' @return An object of class \code{"mv"}.
#' @export
extract <- function(fit, ...) {
  UseMethod("extract")
}


#' @rdname extract
#' @export
extract.default <- function(fit, label = "", ...) {
  # Auto-detect known model types for unclassed objects
  detected <- .detect_model_type(fit = fit)
  if (!is.null(detected)) {
    # Guard: if class already set, method isn't registered
    if (detected %in% class(fit)) {
      stop(
        "Model type '", detected, "' detected but ",
        "extract.", detected, "() is not registered.\n",
        "Load the model package with ",
        "devtools::load_all() or library().",
        call. = FALSE
      )
    }
    class(fit) <- c(detected, class(fit))
    return(extract(fit = fit, label = label, ...))
  }
  stop(
    "Unrecognised model type. ",
    "No extract() method found for class '",
    paste(class(fit), collapse = "', '"), "'.\n",
    "Ensure the model package (e.g., sableOpMod) ",
    "is loaded and registers extract.modeltype().",
    call. = FALSE
  )
}


#' Detect model type from object structure
#'
#' Checks fingerprints of known model types and returns
#' the class name if matched, or NULL.
#'
#' @param fit A list to test.
#' @return Character class name or NULL.
#' @keywords internal
.detect_model_type <- function(fit) {
  if (!is.list(fit)) return(NULL)
  # sableOpMod: has repOpt and data
  if (!is.null(fit$repOpt) && !is.null(fit$data)) {
    return("sableOpMod")
  }
  # Add more fingerprints here as models are added:
  # if (!is.null(fit$siscaField)) return("siscaModel")
  NULL
}
