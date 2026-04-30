test_that("calc_q1 computes the correct first quartile for numeric input", {
  x <- c(1, 2, 3, 4, 5)

  result <- calc_q1(x)

  expect_equal(result, stats::quantile(x, 0.25))
})


test_that("calc_q1 removes NA values and returns correct first quartile", {
  x <- c(1, 2, NA, 4, 5)

  expect_warning(
    result <- calc_q1(x),
    "missing value"
  )

  expect_equal(result, stats::quantile(c(1, 2, 4, 5), 0.25))
})


test_that("calc_q1 errors on NULL input with custom message", {
  expect_error(
    calc_q1(NULL),
    "x must be numeric, not NULL"
  )
})


test_that("calc_q1 errors on non-numeric input", {
  expect_error(
    calc_q1(c("a", "b")),
    "numeric"
  )
})


test_that("calc_q1 errors on mixed-type input", {
  expect_error(
    calc_q1(c(1, 2, "a")),
    "numeric"
  )
})


test_that("calc_q1 accepts integer input", {
  x <- c(1L, 2L, 3L, 4L)

  expect_silent(calc_q1(x))
})


test_that("calc_q1 returns NA for empty numeric vector", {
  x <- numeric(0)

  result <- calc_q1(x)

  expect_true(is.na(result))
})
