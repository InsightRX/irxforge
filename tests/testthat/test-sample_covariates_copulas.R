make_copula_xs_data <- function(n = 200, seed = 1) {
  withr::with_seed(seed, {
    age <- stats::rnorm(n, 55, 12)
    wt <- 70 + 0.3 * (age - 55) + stats::rnorm(n, 0, 12) # correlated with age
    creat <- pmax(0.3, 0.8 + 0.01 * (wt - 70) + stats::rnorm(n, 0, 0.2))
    data.frame(AGE = age, WEIGHT = wt, CREAT = creat)
  })
}

test_that("returns the expected structure and column names", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_xs_data()
  out <- sample_covariates_copulas(dat, n_subjects = 50, seed = 1)
  expect_s3_class(out, "data.frame")
  expect_named(out, names(dat))
  expect_equal(nrow(out), 50)
  expect_false(anyNA(out))
})

test_that("input class is preserved", {
  skip_if_not_installed("rvinecopulib")
  dat <- tibble::as_tibble(make_copula_xs_data())
  out <- sample_covariates_copulas(dat, n_subjects = 20, seed = 1)
  expect_s3_class(out, "tbl_df")
})

test_that("n_subjects may exceed the number of observed subjects", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_xs_data(n = 60)
  out <- sample_covariates_copulas(dat, n_subjects = 500, seed = 1)
  expect_equal(nrow(out), 500)
})

test_that("truncate clamps to the observed range", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_xs_data()
  rng <- range(dat$CREAT)
  out <- sample_covariates_copulas(dat, n_subjects = 300, truncate = TRUE, seed = 2)
  expect_true(all(out$CREAT >= rng[[1]] & out$CREAT <= rng[[2]]))
})

test_that("conditional restricts the sampled population", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_xs_data()
  out <- sample_covariates_copulas(
    dat, conditional = list(AGE = c(50, 60)), n_subjects = 100, seed = 1
  )
  expect_true(all(out$AGE >= 50 & out$AGE <= 60))
})

test_that("reproduces the observed dependence structure", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_xs_data(n = 400)
  out <- sample_covariates_copulas(
    dat, n_subjects = 4000, selcrit = "loglik", seed = 3
  )
  obs_cor <- cor(dat$AGE, dat$WEIGHT)
  sim_cor <- cor(out$AGE, out$WEIGHT)
  expect_equal(sim_cor, obs_cor, tolerance = 0.15)
})

test_that("non-numeric covariates are rejected", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_xs_data()
  dat$SEX <- rep(c("M", "F"), length.out = nrow(dat))
  expect_error(sample_covariates_copulas(dat), "continuous")
})

test_that("replicates add a .replicate column", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_xs_data()
  out <- sample_covariates_copulas(dat, n_subjects = 20, replicates = 3, seed = 1)
  expect_true(".replicate" %in% names(out))
  expect_equal(sort(unique(out$.replicate)), c(1, 2, 3))
  expect_equal(nrow(out), 60)
})

test_that("seed makes the simulation reproducible", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_xs_data()
  out1 <- sample_covariates_copulas(dat, n_subjects = 30, seed = 7)
  out2 <- sample_covariates_copulas(dat, n_subjects = 30, seed = 7)
  expect_equal(out1, out2)
})

test_that("dispatcher routes method = 'copulas'", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_xs_data()
  out <- sample_covariates(method = "copulas", data = dat, n_subjects = 25, seed = 1)
  expect_named(out, names(dat))
  expect_equal(nrow(out), 25)
})
