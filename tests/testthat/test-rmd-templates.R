# test-rmd-templates.R
# Tests for Rmd template integrity.

test_that("no duplicate chunk labels across templates", {
  df <- find_chunk_labels()
  expect_true(nrow(df) > 0)

  dupes <- df$label[duplicated(df$label)]
  if (length(dupes) > 0) {
    dupe_info <- df[df$label %in% dupes, , drop = FALSE]
    msg <- paste(
      apply(
        X = dupe_info, MARGIN = 1,
        FUN = function(r) paste(r["file"], r["label"],
                                sep = ":")
      ),
      collapse = ", "
    )
    fail(message = paste("Duplicate chunk labels:", msg))
  }
  succeed()
})


test_that("no ::: calls in Rmd templates", {
  rmd_dir <- system.file(
    "rmd", package = "modelVis"
  )
  rmd_files <- list.files(
    path = rmd_dir, pattern = "[.]Rmd$",
    full.names = TRUE
  )

  violations <- character(0)
  for (f in rmd_files) {
    lines <- readLines(con = f, warn = FALSE)
    hits <- grep(pattern = ":::", x = lines)
    if (length(hits) > 0) {
      violations <- c(
        violations,
        paste0(basename(f), ":", hits)
      )
    }
  }
  expect_equal(
    object = length(violations),
    expected = 0,
    info = paste(
      "Found ::: calls in:",
      paste(violations, collapse = ", ")
    )
  )
})
