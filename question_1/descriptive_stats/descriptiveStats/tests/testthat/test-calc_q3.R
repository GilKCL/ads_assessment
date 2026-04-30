test_that("calc_q3 computes the correct third quartile for numeric input", {
  x <- c(1, 2, 3, 4, 5)

  result <- calc_q3(x)

  expect_equal(result, stats::quantile(x, 0.75))
})


test_that("calc_q3 removes NA values and returns correct third quartile", {
  x <- c(1, 2, NA, 4, 5)

  expect_warning(
    result <- calc_q3(x),
    "missing value"
  )

  expect_equal(result, stats::quantile(c(1, 2, 4, 5), 0.75))
})


test_that("calc_q3 errors on NULL input with custom message", {
  expect_error(
    calc_q3(NULL),
    "x must be numeric, not NULL"
  )
})


test_that("calc_q3 errors on non-numeric input", {
  expect_error(
    calc_q3(c("a", "b")),
    "numeric"
  )
})


test_that("calc_q3 errors on mixed-type input", {
  expect_error(
    calc_q3(c(1, 2, "a")),
    "numeric"
  )
})


test_that("calc_q3 accepts integer input", {
  x <- c(1L, 2L, 3L, 4L, 5L)

  expect_silent(calc_q3(x))
})


test_that("calc_q3 returns NA for empty numeric vector", {
  x <- numeric(0)

  result <- calc_q3(x)

  expect_true(is.na(result))
})
