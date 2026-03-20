# test-mv-object.R
# Tests for mv object construction and methods.

test_that("new_mv creates valid object with minimal input", {
  meta <- list(
    model_type  = "test",
    label       = "test run",
    years       = 2000:2010,
    ages        = 1:10,
    sex_names   = c("Male", "Female"),
    fleet_names = c("Fleet1", "Fleet2"),
    area_names  = "All"
  )
  sb <- data.frame(
    year = 2000:2010,
    est  = runif(n = 11),
    lwr  = NA_real_,
    upr  = NA_real_,
    area = "All",
    stringsAsFactors = FALSE
  )
  mv <- new_mv(
    meta = meta,
    spawning_biomass = sb,
    recruitment = NULL
  )
  expect_s3_class(object = mv, class = "mv")
  expect_equal(
    object = mv$meta$model_type,
    expected = "test"
  )
  expect_true(is.null(mv$recruitment))
  expect_equal(object = nrow(mv$spawning_biomass),
               expected = 11)
})


test_that("new_mv rejects missing meta fields", {
  bad_meta <- list(
    model_type = "test",
    label      = "test"
    # missing years, ages, sex_names, etc.
  )
  expect_error(
    object = new_mv(meta = bad_meta),
    regexp = "missing required fields"
  )
})


test_that("print.mv runs without error", {
  meta <- list(
    model_type  = "test",
    label       = "test",
    years       = 2000:2005,
    ages        = 1:5,
    sex_names   = "Combined",
    fleet_names = "Fleet1",
    area_names  = "All"
  )
  mv <- new_mv(meta = meta)
  expect_output(
    object = print(mv),
    regexp = "modelVis object"
  )
})


test_that("summary.mv runs without error", {
  meta <- list(
    model_type  = "test",
    label       = "test",
    years       = 2000:2005,
    ages        = 1:5,
    sex_names   = "Combined",
    fleet_names = "Fleet1",
    area_names  = "All"
  )
  mv <- new_mv(meta = meta)
  expect_output(
    object = summary(mv),
    regexp = "Components"
  )
})
