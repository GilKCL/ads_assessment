test_that("calc_mean computes correct mean for numeric input", {
  x <- c(1, 2, 3, 4)

  result <- calc_mean(x)

  expect_equal(result, 2.5)
})


test_that("calc_mean removes NA values and returns correct mean", {
  x <- c(1, 2, NA, 4)

  expect_warning(
    result <- calc_mean(x),
    "missing value"
  )

  expect_equal(result, mean(c(1, 2, 4)))
})


test_that("calc_mean errors on NULL input with custom message", {
  expect_error(
    calc_mean(NULL),
    "x must be numeric, not NULL"
  )
})

test_that("calc_mean errors on non-numeric input", {
  expect_error(
    calc_mean(c("a", "b")),
    "numeric"
  )
})


test_that("calc_mean errors on mixed-type input", {
  expect_error(
    calc_mean(c(1, 2, "a")),
    "numeric"
  )
})


test_that("calc_mean accepts integer input", {
  x <- c(1L, 2L, 3L)

  expect_silent(calc_mean(x))
})


test_that("calc_mean returns NA for empty numeric vector", {
  x <- numeric(0)

  result <- calc_mean(x)

  expect_true(is.na(result))
})
