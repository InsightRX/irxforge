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
  dat <- data.frame(
    AGE = c(20, 30, 40, 50, 60),
    WT  = c(45, 55, 65, 75, 85)
  )
  cndl <- list(WT = c(50, 70))
  n <- 1000
  out_uncond <- sample_covariates_mvtnorm(
    data = dat,
    n_subjects = n
  )
  out_cond <- sample_covariates_mvtnorm(
    data = dat,
    conditional = cndl,
    n_subjects = n
  )
  outside_uncond <- sum(out_uncond$WT < 50 | out_uncond$WT > 70)/n
  outside_cond <- sum(out_cond$WT < 50 | out_cond$WT > 70)/n
  expect_true(outside_uncond > outside_cond * 3) # uncond should be higher by wide margin
})

test_that("conditional argument works with multiple variables", {
  dat <- data.frame(
    age = 18:65,
    height = seq(150, 197, length.out = 48),
    weight = rnorm(48)
  )
  cndl <- list(age = c(25, 35), height = c(160, 180))
  n <- 1000
  out_uncond <- sample_covariates_mvtnorm(dat, n_subjects = n)
  out_cond <- sample_covariates_mvtnorm(dat, n_subjects = n, conditional = cndl)
  outside_age_uncond <- sum(out_uncond$age < 25 | out_uncond$age > 35)/n
  outside_age_cond <- sum(out_cond$age < 25 | out_cond$age > 35)/n
  outside_height_uncond <- sum(out_uncond$height < 160 | out_uncond$height > 180)/n
  outside_height_cond <- sum(out_cond$height < 160 | out_cond$height > 180)/n
  expect_true(outside_age_uncond > outside_age_cond * 3)
  expect_true(outside_height_uncond > outside_height_cond * 3)
})

test_that("seed produces reproducible output", {
  dat <- data.frame(x = rnorm(50), y = rnorm(50))
  out1 <- sample_covariates_mvtnorm(dat, n_subjects = 20, seed = 1)
  out2 <- sample_covariates_mvtnorm(dat, n_subjects = 20, seed = 1)
  expect_equal(out1, out2)
})

test_that("different seeds produce different output", {
  dat <- data.frame(x = rnorm(50), y = rnorm(50))
  out1 <- sample_covariates_mvtnorm(dat, n_subjects = 20, seed = 1)
  out2 <- sample_covariates_mvtnorm(dat, n_subjects = 20, seed = 2)
  expect_false(identical(out1, out2))
})
