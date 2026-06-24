test_that("bootstrap noise jitters continuous covariates but not categoricals", {
  dat <- data.frame(
    WT = rep(70, 60),
    SEX = rep("M", 60),
    stringsAsFactors = FALSE
  )
  out <- sample_covariates_bootstrap(
    dat, n_subjects = 60, cat_covs = "SEX", noise = 0.1, seed = 1
  )
  expect_false(all(out$WT == 70)) # continuous jittered
  expect_true(all(out$SEX == "M")) # categorical untouched
  expect_true(all(out$WT > 0)) # multiplicative noise preserves positivity
  # ~10% SD on the log scale (loose band for sampling variability at n = 60)
  expect_gt(stats::sd(log(out$WT)), 0.05)
  expect_lt(stats::sd(log(out$WT)), 0.2)
})

test_that("bootstrap noise = NULL leaves sampled values unchanged", {
  dat <- data.frame(WT = c(60, 70, 80, 90))
  out_default <- sample_covariates_bootstrap(dat, n_subjects = 4, seed = 1)
  out_null <- sample_covariates_bootstrap(dat, n_subjects = 4, noise = NULL, seed = 1)
  expect_equal(out_default, out_null)
  expect_true(all(out_default$WT %in% dat$WT))
})

test_that("invalid noise is rejected", {
  dat <- data.frame(WT = c(60, 70))
  expect_error(sample_covariates_bootstrap(dat, noise = -1), "non-negative")
  expect_error(sample_covariates_bootstrap(dat, noise = c(0.1, 0.2)), "single")
})

test_that("bootstrap conditional filters categorical covariates by value", {
  dat <- data.frame(
    WT = c(50, 60, 70, 80),
    SEX = c("M", "F", "F", "M"),
    stringsAsFactors = FALSE
  )
  out <- sample_covariates_bootstrap(
    dat, n_subjects = 20, cat_covs = "SEX",
    conditional = list(SEX = "F"), seed = 1
  )
  expect_true(all(out$SEX == "F"))
})

test_that("baseline_method = 'bootstrap' draws baselines from observed values", {
  dat <- data.frame(
    ID = rep(1:6, each = 2),
    TIME = rep(c(0, 1), 6),
    WT = rep(c(50, 60, 70, 80, 90, 100), each = 2)
  )
  out <- sample_covariates_mice_timevarying(
    dat, time_varying_covs = "WT", n_subjects = 6,
    baseline_method = "bootstrap", time_grid = c(0, 1), seed = 1
  )
  base <- out$WT[out$TIME == 0]
  # Bootstrap baselines are exact observed values (no MICE mean-shrinkage).
  expect_true(all(base %in% dat$WT))
})

test_that("baseline noise obscures bootstrapped baseline values", {
  dat <- data.frame(
    ID = rep(1:6, each = 2),
    TIME = rep(c(0, 1), 6),
    WT = rep(c(50, 60, 70, 80, 90, 100), each = 2)
  )
  out <- suppressWarnings(sample_covariates_lme_timevarying(
    dat, time_varying_covs = "WT", n_subjects = 6,
    baseline_method = "bootstrap", noise = 0.05, time_grid = c(0, 1), seed = 1
  ))
  base <- out$WT[out$TIME == 0]
  expect_false(all(base %in% dat$WT)) # jittered off the observed grid
  expect_true(all(base > 0))
})
