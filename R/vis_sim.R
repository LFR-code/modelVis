# vis_sim.R
# Entry point for generating MSE simulation dashboards.

#' Generate interactive simulation dashboard
#'
#' Extracts standardised output from an MS3 simulation
#' blob and renders an interactive HTML flexdashboard
#' showing simulation envelopes, example replicates,
#' and performance summaries.
#'
#' @param blob An MS3 simulation blob (from
#'   \code{runMS3()} / \code{load()}).
#' @param label Character label for this scenario.
#'   If NULL, constructed from scenario + MP name.
#' @param probs Numeric vector of length 3: lower, median,
#'   upper quantile probabilities for simulation envelopes.
#'   Default \code{c(0.025, 0.5, 0.975)}.
#' @param output_file Path for the output HTML file.
#'   If NULL, opens an interactive viewer.
#' @return Invisibly returns the path to the rendered
#'   HTML file (if \code{output_file} is specified) or
#'   NULL (if interactive viewer is used).
#' @export
mv_sim_dashboard <- function(blob,
                             label       = NULL,
                             probs       = c(0.025, 0.5,
                                             0.975),
                             perf_stats  = NULL,
                             output_file = NULL) {

  # 1. Extract mv_sim object
  mv <- extract(
    fit        = blob,
    label      = label,
    probs      = probs,
    perf_stats = perf_stats
  )

  # 2. Pass to Rmd via temporary global variable
  tmp <- list(mv = mv)
  assign(x = "tmp", value = tmp, envir = globalenv())
  on.exit(expr = {
    if (exists(x = "tmp", envir = globalenv())) {
      rm(list = "tmp", envir = globalenv())
    }
  })

  # 3. Locate template
  template <- system.file(
    "rmd", "vis_sim.Rmd", package = "modelVis"
  )
  if (template == "") {
    stop(
      "Cannot find vis_sim.Rmd template. ",
      "Is modelVis installed correctly?",
      call. = FALSE
    )
  }

  # 4. Render or run
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
    message("Simulation dashboard written to: ", out)
    invisible(out)
  }
}
