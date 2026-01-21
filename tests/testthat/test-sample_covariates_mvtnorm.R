test_that("input class is preserved", {
  dat <- data.frame(x = rnorm(50), y = rnorm(50))
  tbl <- tibble::tibble(x = rnorm(50), y = rnorm(50))
  out_dat <- sample_covariates_mvtnorm(data = dat)
  out_tbl <- sample_covariates_mvtnorm(data = tbl)
  expect_s3_class(out_dat, "data.frame")
  expect_s3_class(out_tbl, "tbl_df")
})

test_that("all input columns are preserved", {
  dat <- data.frame(x = rnorm(50), y = rnorm(50))
  out <- sample_covariates_mvtnorm(data = dat)
  expect_equal(ncol(out), ncol(dat))
  expect_named(out, names(dat))
})

test_that("output is numeric for all columns", {
  dat <- data.frame(x = rnorm(40), y = rnorm(40))
  out <- sample_covariates_mvtnorm(dat)
  expect_true(all(vapply(out, is.numeric, logical(1))))
})

test_that("number of rows matches input by default", {
  dat <- data.frame(x = rnorm(50), y = rnorm(50))
  out <- sample_covariates_mvtnorm(data = dat)
  expect_equal(nrow(out), nrow(dat))
})

test_that("n_subjects controls the number of simulated rows", {
  dat <- data.frame(x = rnorm(50), y = rnorm(50))
  out <- sample_covariates_mvtnorm(data = dat, n_subjects = 2)
  expect_equal(nrow(out), 2)
})

test_that("samples from exponential distribution when exponential = TRUE", {
  # TODO: improve testing after adding safety rails around this argument.
  dat <- data.frame(x = rpois(50, 1) + 1, y = rpois(50, 1) + 1)
  out <- sample_covariates_mvtnorm(dat, exponential = TRUE)
  expect_true(all(out$x > 0))
  expect_true(all(out$y > 0))
})

test_that("conditional argument filters data before sampling", {
  # TODO: Think of a good way to test this, since the output can include values
  # outside the conditional range.
  skip()
  dat <- data.frame(
    AGE = c(20, 30, 40, 50, 60),
    WT  = c(45, 55, 65, 75, 85)
  )
  cndl <- list(WT = c(50, 70))
  out <- sample_covariates_mvtnorm(
    data = dat,
    conditional = cndl,
    n_subjects = 50
  )
  expect_true(all(out$WT >= 50 & out$WT <= 70))
})

test_that("conditional argument works with multiple variables", {
  # TODO: Think of a good way to test this, since the output can include values
  # outside the conditional range.
  skip()
  dat <- data.frame(
    age = 18:65,
    height = seq(150, 197, length.out = 48),
    weight = rnorm(48)
  )
  cndl <- list(age = c(25, 35), height = c(160, 180))
  out <- sample_covariates_mvtnorm(dat, n_subjects = 50, conditional = cndl)
  expect_true(all(out$age >= 25 & out$age <= 35))
  expect_true(all(out$height >= 160 & out$height <= 180))
})
