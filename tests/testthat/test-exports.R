# test-exports.R
# Tests that all functions referenced in Rmd templates
# are exported from modelVis.

test_that("Rmd-referenced modelVis:: functions exist", {
  rmd_dir <- system.file(
    "rmd", package = "modelVis"
  )
  rmd_files <- list.files(
    path = rmd_dir, pattern = "\\.Rmd$",
    full.names = TRUE
  )

  # Extract all modelVis::xxx calls
  all_refs <- character(0)
  for (f in rmd_files) {
    lines <- readLines(con = f, warn = FALSE)
    matches <- regmatches(
      x = lines,
      m = gregexpr(
        pattern = "modelVis::[a-zA-Z_][a-zA-Z0-9_.]*",
        text = lines
      )
    )
    refs <- unlist(matches)
    if (length(refs) > 0) {
      fns <- sub(
        pattern = "^modelVis::",
        replacement = "",
        x = refs
      )
      all_refs <- c(all_refs, fns)
    }
  }
  all_refs <- unique(all_refs)
  expect_true(length(all_refs) > 0)

  ns <- asNamespace("modelVis")
  missing <- character(0)
  for (fn in all_refs) {
    if (!exists(x = fn, envir = ns)) {
      missing <- c(missing, fn)
    }
  }
  expect_equal(
    object = length(missing),
    expected = 0,
    info = paste(
      "Functions referenced in Rmd but not in",
      "modelVis namespace:",
      paste(missing, collapse = ", ")
    )
  )
})


test_that("exported tidy helpers exist", {
  expected <- c(
    "mv_tidy_ts", "mv_tidy_mat", "mv_tidy_3d",
    "mv_tidy_4d", "mv_tidy_5d",
    "mv_quantile_ts", "mv_year_seq",
    "mv_ensure_ci", "mv_has_data",
    "mv_layout", "mv_config",
    "mv_colours", "mv_fleet_colours",
    "mv_sex_colours", "mv_kobe_colours"
  )
  for (fn in expected) {
    expect_true(
      object = is.function(
        getExportedValue(
          ns = "modelVis", name = fn
        )
      ),
      info = paste(fn, "should be exported")
    )
  }
})
