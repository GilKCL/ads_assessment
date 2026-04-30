test_that("calc_iqr computes the correct interquartile range for numeric input", {
  x <- c(1, 2, 3, 4, 5)

  result <- calc_iqr(x)

  expect_equal(result, stats::IQR(x))
})


test_that("calc_iqr removes NA values and returns correct IQR", {
  x <- c(1, 2, NA, 4, 5)

  expect_warning(
    result <- calc_iqr(x),
    "missing value"
  )

  expect_equal(result, stats::IQR(c(1, 2, 4, 5)))
})


test_that("calc_iqr errors on NULL input with custom message", {
  expect_error(
    calc_iqr(NULL),
    "x must be numeric, not NULL"
  )
})


test_that("calc_iqr errors on non-numeric input", {
  expect_error(
    calc_iqr(c("a", "b")),
    "numeric"
  )
})


test_that("calc_iqr errors on mixed-type input", {
  expect_error(
    calc_iqr(c(1, 2, "a")),
    "numeric"
  )
})


test_that("calc_iqr accepts integer input", {
  x <- c(1L, 2L, 3L, 4L, 5L)

  expect_silent(calc_iqr(x))
})


test_that("calc_iqr returns NA for empty numeric vector", {
  x <- numeric(0)

  result <- calc_iqr(x)

  expect_true(is.na(result))
})
