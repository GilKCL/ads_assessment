test_that("calc_median computes correct median for numeric input", {
  x <- c(1, 2, 3, 4, 5)

  result <- calc_median(x)

  expect_equal(result, 3)
})


test_that("calc_median removes NA values and returns correct median", {
  x <- c(1, 2, NA, 4, 5)

  expect_warning(
    result <- calc_median(x),
    "missing value"
  )

  expect_equal(result, median(c(1, 2, 4, 5)))
})


test_that("calc_median errors on NULL input with custom message", {
  expect_error(
    calc_median(NULL),
    "x must be numeric, not NULL"
  )
})

test_that("calc_median errors on non-numeric input", {
  expect_error(
    calc_median(c("a", "b")),
    "numeric"
  )
})

test_that("calc_median errors on mixed-type input", {
  expect_error(
    calc_median(c(1, 2, "a")),
    "numeric"
  )
})

test_that("calc_median accepts integer input", {
  x <- c(1L, 2L, 3L, 4L)

  expect_silent(calc_median(x))
})


test_that("calc_median returns NA for empty numeric vector", {
  x <- numeric(0)

  result <- calc_median(x)

  expect_true(is.na(result))
})
