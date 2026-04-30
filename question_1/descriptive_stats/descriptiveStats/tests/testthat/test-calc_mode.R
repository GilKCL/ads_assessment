test_that("calc_mode computes correct mode for numeric input", {
  x <- c(1, 2, 2, 3, 4)

  result <- calc_mode(x)

  expect_equal(result, 2)
})


test_that("calc_mode removes NA values and returns correct mode", {
  x <- c(1, 2, 2, NA, 3)

  expect_warning(
    result <- calc_mode(x),
    "missing value"
  )

  expect_equal(result, 2)
})


test_that("calc_mode errors on NULL input", {
  expect_error(
    calc_mode(NULL),
    "x must be numeric, not NULL"
  )
})

test_that("calc_mode errors on non-numeric input", {
  expect_error(
    calc_mode(c("a", "b")),
    "numeric"
  )
})

test_that("calc_mode errors on mixed-type input", {
  expect_error(
    calc_mode(c(1, 2, "a")),
    "numeric"
  )
})

test_that("calc_mode accepts integer input", {
  x <- c(1L, 2L, 2L, 3L)

  expect_silent(calc_mode(x))
})


test_that("calc_mode returns NA for empty numeric vector", {
  x <- numeric(0)

  result <- calc_mode(x)

  expect_true(is.na(result))
})
