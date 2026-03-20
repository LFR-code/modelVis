# test-tidy-helpers.R
# Tests for exported tidy helper functions.

test_that("mv_tidy_ts creates correct data.frame", {
  df <- mv_tidy_ts(
    x = c(10, 20, 30),
    years = 2000:2002
  )
  expect_equal(object = nrow(df), expected = 3)
  expect_equal(
    object = names(df),
    expected = c("year", "est")
  )
  expect_equal(
    object = df$year,
    expected = 2000:2002
  )
})


test_that("mv_tidy_ts errors on length mismatch", {
  expect_error(
    object = mv_tidy_ts(
      x = 1:3, years = 2000:2005
    ),
    regexp = "length"
  )
})


test_that("mv_tidy_mat creates correct data.frame", {
  m <- matrix(
    data = 1:6, nrow = 2, ncol = 3
  )
  df <- mv_tidy_mat(
    x = m,
    row_name = "age",
    col_name = "sex",
    row_labels = 1:2,
    col_labels = c("M", "F", "C")
  )
  expect_equal(object = nrow(df), expected = 6)
  expect_true("age" %in% names(df))
  expect_true("sex" %in% names(df))
  expect_true("est" %in% names(df))
})


test_that("mv_quantile_ts returns correct structure", {
  mat <- matrix(
    data = rnorm(n = 300),
    nrow = 100, ncol = 3
  )
  df <- mv_quantile_ts(
    x = mat, years = 2000:2002
  )
  expect_equal(object = nrow(df), expected = 3)
  expect_equal(
    object = sort(names(df)),
    expected = c("est", "lwr", "upr", "year")
  )
  # est should be median (between lwr and upr)
  expect_true(all(df$est >= df$lwr))
  expect_true(all(df$est <= df$upr))
})


test_that("mv_ensure_ci adds missing columns", {
  df <- data.frame(year = 1:3, est = runif(n = 3))
  df2 <- mv_ensure_ci(df = df)
  expect_true("lwr" %in% names(df2))
  expect_true("upr" %in% names(df2))
  expect_true(all(is.na(df2$lwr)))
})


test_that("mv_has_data works correctly", {
  expect_false(mv_has_data(x = NULL))
  expect_false(mv_has_data(x = c(0, 0, 0)))
  expect_true(mv_has_data(x = c(0, 1, 0)))
  expect_true(mv_has_data(x = "text"))
})


test_that("mv_year_seq creates correct sequence", {
  yrs <- mv_year_seq(fYear = 1965, nT = 57)
  expect_equal(object = length(yrs), expected = 57)
  expect_equal(object = yrs[1], expected = 1965)
  expect_equal(object = yrs[57], expected = 2021)
})
