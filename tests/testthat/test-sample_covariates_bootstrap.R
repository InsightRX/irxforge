test_that("input class is preserved", {
  dat <- data.frame(x = 1:10, y = letters[1:10])
  tbl <- tibble::tibble(x = 1:10, y = letters[1:10])
  out_dat <- sample_covariates_bootstrap(dat)
  out_tbl <- sample_covariates_bootstrap(tbl)
  expect_s3_class(out_dat, "data.frame")
  expect_s3_class(out_tbl, "tbl_df")
})

test_that("column names and types are preserved", {
  dat <- data.frame(
    numeric = rnorm(10),
    integer = 1:10,
    factor = factor(letters[1:10]),
    logical = rep(c(TRUE, FALSE), 5)
  )
  out <- sample_covariates_bootstrap(dat, n_subjects = 20)
  expect_equal(names(out), names(dat))
  expect_equal(
    vapply(out, class, character(1)), vapply(dat, class, character(1))
  )
})

test_that("returns correct number of rows", {
  dat <- data.frame(x = 1:10, y = rnorm(10))
  out <- sample_covariates_bootstrap(dat, n_subjects = 25)
  expect_equal(nrow(out), 25)
  expect_equal(ncol(out), ncol(dat))
})

test_that("conditional argument filters data before sampling", {
  dat <- data.frame(age = 18:65, bmi = rnorm(48))
  cndl <- list(age = c(30, 40))
  out <- sample_covariates_bootstrap(dat, n_subjects = 100, conditional = cndl)
  expect_true(all(out$age >= 30 & out$age <= 40))
})

test_that("conditional argument works with multiple variables", {
  dat <- data.frame(
    age = 18:65,
    height = seq(150, 197, length.out = 48),
    weight = rnorm(48)
  )
  cndl <- list(age = c(25, 35), height = c(160, 180))
  out <- sample_covariates_bootstrap(dat, n_subjects = 50, conditional = cndl)
  expect_true(all(out$age >= 25 & out$age <= 35))
  expect_true(all(out$height >= 160 & out$height <= 180))
})

test_that("returns zero rows when n_subjects is zero", {
  dat <- data.frame(x = 1:10)
  out <- sample_covariates_bootstrap(dat, n_subjects = 0)
  expect_equal(nrow(out), 0)
})

test_that("returns error when filtered data is empty", {
  dat <- data.frame(x = 1:10)
  cndl <- list(x = c(100, 200))
  expect_error(
    sample_covariates_bootstrap(dat, n_subjects = 10, conditional = cndl),
    "No observations present"
  )
})

test_that("na.rm = TRUE (default) drops rows with NAs before sampling", {
  dat <- data.frame(x = c(1:5, NA, 7:10), y = rnorm(10))
  out <- sample_covariates_bootstrap(dat, n_subjects = 20)
  expect_false(anyNA(out$x))
})

test_that("na.rm = FALSE allows NAs to be sampled", {
  dat <- data.frame(x = rep(NA_real_, 10))
  out <- sample_covariates_bootstrap(dat, n_subjects = 5, na.rm = FALSE)
  expect_true(all(is.na(out$x)))
})

test_that("seed produces reproducible output", {
  dat <- data.frame(x = rnorm(100))
  out1 <- sample_covariates_bootstrap(dat, n_subjects = 20, seed = 1)
  out2 <- sample_covariates_bootstrap(dat, n_subjects = 20, seed = 1)
  expect_equal(out1, out2)
})

test_that("different seeds produce different output", {
  dat <- data.frame(x = rnorm(100))
  out1 <- sample_covariates_bootstrap(dat, n_subjects = 20, seed = 1)
  out2 <- sample_covariates_bootstrap(dat, n_subjects = 20, seed = 2)
  expect_false(identical(out1, out2))
})
