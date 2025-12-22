test_that("returns numeric vector", {
  t <- withr::with_seed(123, rnorm(100, mean = 5, sd = 0.5))
  out <- get_nominal_timepoints(t)
  expect_type(out, "double")
  expect_true(length(out) >= 1)
  expect_false(anyNA(out))
})

test_that("detects multiple nominal peaks", {
  # nominal peaks at t = 1, 2, 4
  t <- withr::with_seed(
    456, c(rnorm(50, 1, 0.05), rnorm(50, 2, 0.05), rnorm(50, 4, 0.05))
  )
  out <- get_nominal_timepoints(t)
  expect_length(out, 3)
  expect_equal(out[1], 1)
  expect_equal(out[2], 2)
  expect_equal(out[3], 4)
})

test_that("is sensitive to adjust parameter", {
  t <- withr::with_seed(
    456, c(rnorm(50, 1, 0.05), rnorm(50, 2, 0.05), rnorm(50, 4, 0.05))
  )
  out_low_adjust  <- get_nominal_timepoints(t, adjust = 0.3)
  out_high_adjust <- get_nominal_timepoints(t, adjust = 1.5)
  # Lower adjust should generally find more peaks:
  expect_true(length(out_low_adjust) >= length(out_high_adjust))
})

test_that("works with evenly spaced data", {
  t <- seq(0, 10, by = 1)
  out <- get_nominal_timepoints(t)
  expect_true(length(out) >= 1)
  expect_true(all(out >= min(t)))
  expect_true(all(out <= max(t) + 1))
})

test_that("works with small input size", {
  t <- c(1, 1.1, 0.9)
  out <- get_nominal_timepoints(t)
  expect_type(out, "double")
  expect_true(length(out) >= 1)
  expect_false(anyNA(out))
})

test_that("errors on non-numeric input", {
  t <- c("a", "b", "c")
  expect_error(get_nominal_timepoints(t), regexp = "density|numeric")
})


