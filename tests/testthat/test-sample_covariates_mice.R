test_that("input class is preserved", {
  dat <- data.frame(
    AGE = c(20, 30, 40, 50), WT  = c(55, 70, 65, 80), SEX = c("M", "F", "F", "M")
  )
  tbl <- tibble::tibble(
    AGE = c(20, 30, 40, 50), WT  = c(55, 70, 65, 80), SEX = c("M", "F", "F", "M")
  )
  out_dat <- sample_covariates_mice(data = dat, cat_covs = "SEX")
  out_tbl <- sample_covariates_mice(data = tbl, cat_covs = "SEX")
  expect_s3_class(out_dat, "data.frame")
  expect_s3_class(out_tbl, "tbl_df")
})

test_that("all input columns are preserved", {
  dat <- data.frame(
    AGE = c(20, 30, 40, 50), WT  = c(55, 70, 65, 80), SEX = c("M", "F", "F", "M")
  )
  out <- sample_covariates_mice(data = dat, cat_covs = "SEX")
  expect_equal(ncol(out), ncol(dat))
  expect_named(out, names(dat))
})

test_that("MICE bookkeeping columns are dropped", {
  dat <- data.frame(
    AGE = c(20, 30, 40, 50), WT  = c(55, 70, 65, 80), SEX = c("M", "F", "F", "M")
  )
  out <- sample_covariates_mice(data = dat, cat_covs = "SEX")
  expect_false(any(c(".id", ".imp", "Type") %in% names(out)))
})

test_that("number of rows matches input by default", {
  dat <- data.frame(
    AGE = c(20, 30, 40, 50), WT  = c(55, 70, 65, 80), SEX = c("M", "F", "F", "M")
  )
  out <- sample_covariates_mice(data = dat, cat_covs = "SEX")
  expect_equal(nrow(out), nrow(dat))
})

test_that("categorical covariates are returned as factors", {
  dat <- data.frame(
    AGE = c(20, 30, 40, 50), WT  = c(55, 70, 65, 80), SEX = c("M", "F", "F", "M")
  )
  out <- sample_covariates_mice(data = dat, cat_covs = "SEX")
  expect_s3_class(out$SEX, "factor")
})

test_that("handles missing values in input data", {
  dat <- data.frame(
    AGE = c(20, NA, 40, 50), WT  = c(55, 70, NA, 80), SEX = c("M", NA, "F", "M")
  )
  out <- sample_covariates_mice(data = dat, cat_covs = "SEX")
  expect_false(anyNA(out))
})

test_that("n_subjects controls the number of simulated rows", {
  dat <- data.frame(
    AGE = c(20, 30, 40, 50), WT  = c(55, 70, 65, 80), SEX = c("M", "F", "F", "M")
  )
  out <- sample_covariates_mice(data = dat, cat_covs = "SEX", n_subjects = 2)
  expect_equal(nrow(out), 2)
})

test_that("conditional argument filters data before sampling", {
  dat <- data.frame(
    AGE = c(20, 30, 40, 50, 60),
    WT  = c(45, 55, 65, 75, 85),
    SEX = c("M", "F", "F", "M", "F")
  )
  cndl <- list(WT = c(50, 70))
  out <- sample_covariates_mice(
    data = dat,
    cat_covs = "SEX",
    conditional = cndl,
    n_subjects = 5
  )
  expect_true(all(out$WT >= 50 & out$WT <= 70))
})

test_that("conditional argument works with multiple variables", {
  dat <- data.frame(
    AGE = c(20, 30, 40, 50, 60),
    WT  = c(45, 55, 65, 75, 85),
    SEX = c("M", "F", "F", "M", "F")
  )
  cndl <- list(AGE = c(25, 35), WT = c(50, 80))
  out <- sample_covariates_bootstrap(
    dat, cat_covs = "SEX", n_subjects = 50, conditional = cndl
  )
  expect_true(all(out$AGE >= 25 & out$AGE <= 35))
  expect_true(all(out$WT >= 50 & out$WT <= 80))
})
