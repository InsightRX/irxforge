make_copula_test_data <- function(n_subjects = 40, times = c(0, 2, 4, 6), seed = 1) {
  withr::with_seed(seed, {
    rows <- lapply(seq_len(n_subjects), function(i) {
      wt0 <- stats::rnorm(1, 75, 15)
      cr0 <- 0.6 + 0.01 * (wt0 - 75) + stats::rnorm(1, 0, 0.2) # correlated with WT
      wt_slope <- stats::rnorm(1, 0, 0.5)
      cr_slope <- stats::rnorm(1, 0, 0.03)
      data.frame(
        ID = i,
        TIME = times,
        AGE = stats::rnorm(1, 55, 12),
        WEIGHT = wt0 + wt_slope * times + stats::rnorm(length(times), 0, 1),
        CREAT = pmax(0.2, cr0 + cr_slope * times + stats::rnorm(length(times), 0, 0.05))
      )
    })
    do.call(rbind, rows)
  })
}

test_that("returns long-format output with the expected structure", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_test_data()
  grid <- c(0, 2, 4, 6)
  out <- sample_covariates_copulas_timevarying(
    dat, static_covs = "AGE", time_varying_covs = c("WEIGHT", "CREAT"),
    time_grid = grid, n_subjects = 10, seed = 1
  )
  expect_s3_class(out, "data.frame")
  expect_named(out, c("ID", "TIME", "AGE", "WEIGHT", "CREAT"))
  expect_equal(length(unique(out$ID)), 10)
  expect_equal(nrow(out), 10 * length(grid))
  expect_false(anyNA(out))
})

test_that("input class is preserved", {
  skip_if_not_installed("rvinecopulib")
  dat <- tibble::as_tibble(make_copula_test_data())
  out <- sample_covariates_copulas_timevarying(
    dat, time_varying_covs = c("WEIGHT", "CREAT"),
    time_grid = c(0, 4), n_subjects = 5, seed = 1
  )
  expect_s3_class(out, "tbl_df")
})

test_that("time_grid yields a shared grid for all subjects", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_test_data()
  grid <- c(0, 1, 5)
  out <- sample_covariates_copulas_timevarying(
    dat, time_varying_covs = c("WEIGHT", "CREAT"),
    time_grid = grid, n_subjects = 8, seed = 1
  )
  per_subject <- split(out$TIME, out$ID)
  expect_true(all(vapply(per_subject, function(t) identical(sort(t), grid), logical(1))))
})

test_that("truncate clamps reconstructed values to the observed range", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_test_data()
  rng <- range(dat$CREAT)
  out <- sample_covariates_copulas_timevarying(
    dat, time_varying_covs = c("WEIGHT", "CREAT"),
    time_grid = c(0, 3, 6), n_subjects = 30, truncate = TRUE, seed = 2
  )
  expect_true(all(out$CREAT >= rng[[1]] & out$CREAT <= rng[[2]]))
})

test_that("replicates add a .replicate column", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_test_data()
  out <- sample_covariates_copulas_timevarying(
    dat, time_varying_covs = c("WEIGHT", "CREAT"),
    time_grid = c(0, 4), n_subjects = 5, replicates = 2, seed = 1
  )
  expect_true(".replicate" %in% names(out))
  expect_equal(sort(unique(out$.replicate)), c(1, 2))
})

test_that("categorical / non-numeric covariates are rejected", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_test_data()
  dat$SEX <- rep(c("M", "F"), length.out = nrow(dat))
  expect_error(
    sample_covariates_copulas_timevarying(
      dat, static_covs = "SEX", time_varying_covs = c("WEIGHT", "CREAT"),
      time_grid = c(0, 4)
    ),
    "continuous"
  )
})

test_that("degree must be a positive integer", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_test_data()
  expect_error(
    sample_covariates_copulas_timevarying(
      dat, time_varying_covs = "WEIGHT", time_grid = c(0, 4), degree = 0
    ),
    "positive integer"
  )
})

test_that("seed makes the simulation reproducible", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_test_data()
  out1 <- sample_covariates_copulas_timevarying(
    dat, time_varying_covs = c("WEIGHT", "CREAT"),
    time_grid = c(0, 4), n_subjects = 6, seed = 99
  )
  out2 <- sample_covariates_copulas_timevarying(
    dat, time_varying_covs = c("WEIGHT", "CREAT"),
    time_grid = c(0, 4), n_subjects = 6, seed = 99
  )
  expect_equal(out1, out2)
})

test_that("noise obscures generated subjects but keeps statics constant in time", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_test_data()
  args <- list(
    data = dat, static_covs = "AGE", time_varying_covs = c("WEIGHT", "CREAT"),
    time_grid = c(0, 2, 4), n_subjects = 20, seed = 5
  )
  plain <- do.call(sample_covariates_copulas_timevarying, args)
  noisy <- do.call(sample_covariates_copulas_timevarying, c(args, noise = 0.05))
  # noise changes the generated values
  expect_false(isTRUE(all.equal(plain$CREAT, noisy$CREAT)))
  # static covariate stays constant within each subject
  per_subject_age <- tapply(noisy$AGE, noisy$ID, function(x) length(unique(round(x, 8))))
  expect_true(all(per_subject_age == 1))
})

test_that("dispatcher routes method = 'copulas_timevarying'", {
  skip_if_not_installed("rvinecopulib")
  dat <- make_copula_test_data()
  out <- sample_covariates(
    method = "copulas_timevarying", data = dat,
    time_varying_covs = c("WEIGHT", "CREAT"),
    time_grid = c(0, 4), n_subjects = 5, seed = 1
  )
  expect_named(out, c("ID", "TIME", "WEIGHT", "CREAT"))
})
