# vis_fit.R
# Main entry point for generating diagnostic dashboards.

#' Generate interactive model diagnostics dashboard
#'
#' Extracts standardised output from a fitted stock
#' assessment model and renders an interactive HTML
#' flexdashboard. When MCMC posteriors are available,
#' credible intervals are shown on time-series plots
#' and posterior density panels are included.
#'
#' @param fit A fitted model object (e.g., sableOpMod
#'   reports list).
#' @param label Character label for this model run
#'   (e.g., "Base case 2025").
#' @param comp Optional named list of additional model
#'   objects for comparison. Names become labels.
#' @param mcmc_probs Numeric vector of length 3 giving
#'   the lower, median, and upper quantile probabilities
#'   for MCMC posteriors.
#'   Default \code{c(0.025, 0.5, 0.975)}. The median
#'   (0.5) is used as the risk-neutral point estimate
#'   and the outer values define the 95\% credible
#'   interval. Ignored when posteriors are not available.
#' @param output_file Path for the output HTML file.
#'   If NULL, opens an interactive viewer via
#'   \code{rmarkdown::run()}.
#' @return Invisibly returns the path to the rendered
#'   HTML file (if \code{output_file} is specified) or
#'   NULL (if interactive viewer is used).
#' @export
mv_dashboard <- function(fit,
                         label       = NULL,
                         comp        = NULL,
                         mcmc_probs  = c(0.025, 0.5, 0.975),
                         output_file = NULL) {
  if (is.null(label)) label <- ""

  # 1. Extract mv object (extractor populates lwr/upr
  #    from posteriors when available)
  mv <- extract(
    fit = fit, label = label,
    mcmc_probs = mcmc_probs
  )

  # 2. Extract comparison models if provided
  comp_mv <- NULL
  if (!is.null(comp)) {
    comp_names <- names(comp)
    if (is.null(comp_names)) {
      comp_names <- paste0("Model ", seq_along(comp))
    }
    comp_mv <- lapply(
      X = seq_along(comp),
      FUN = function(i) {
        extract(
          fit = comp[[i]], label = comp_names[i],
          mcmc_probs = mcmc_probs
        )
      }
    )
  }

  # 3. Pass data to Rmd via temporary environment
  tmp <- list(mv = mv, comp_mv = comp_mv)
  assign(x = "tmp", value = tmp, envir = globalenv())
  on.exit(expr = {
    if (exists(x = "tmp", envir = globalenv())) {
      rm(list = "tmp", envir = globalenv())
    }
  })

  # 4. Locate template
  template <- system.file(
    "rmd", "vis_fit.Rmd", package = "modelVis"
  )
  if (template == "") {
    stop(
      "Cannot find dashboard template. ",
      "Is modelVis installed correctly?",
      call. = FALSE
    )
  }

  # 5. Render or run
  if (is.null(output_file)) {
    rmarkdown::run(file = template)
    invisible(NULL)
  } else {
    out <- rmarkdown::render(
      input       = template,
      output_file = basename(output_file),
      output_dir  = dirname(output_file),
      envir       = new.env(parent = globalenv()),
      quiet       = TRUE
    )
    message("Dashboard written to: ", out)
    invisible(out)
  }
}
