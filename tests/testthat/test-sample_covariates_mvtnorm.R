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

# --- Direct distribution specification (means + sigma / sds) ---

test_that("means + sigma samples correct number of rows and columns", {
  mu <- c(x = 10, y = 20)
  S <- matrix(c(4, 1, 1, 9), nrow = 2, dimnames = list(c("x","y"), c("x","y")))
  out <- sample_covariates_mvtnorm(means = mu, sigma = S, n_subjects = 100)
  expect_equal(nrow(out), 100)
  expect_equal(ncol(out), 2)
  expect_named(out, c("x", "y"))
})

test_that("means + sigma samples near the specified mean (large n)", {
  mu <- c(AGE = 40, WT = 70)
  S <- diag(c(25, 100))
  out <- sample_covariates_mvtnorm(means = mu, sigma = S, n_subjects = 2000, seed = 42)
  expect_equal(mean(out$AGE), 40, tolerance = 1.5)
  expect_equal(mean(out$WT),  70, tolerance = 3)
})

test_that("means + sd constructs diagonal covariance and samples correctly", {
  mu  <- c(x = 5, y = 50)
  sds <- c(x = 1, y = 10)
  out <- sample_covariates_mvtnorm(means = mu, sd = sds, n_subjects = 2000, seed = 7)
  expect_named(out, c("x", "y"))
  expect_equal(mean(out$x), 5,  tolerance = 0.25)
  expect_equal(mean(out$y), 50, tolerance = 2.5)
  expect_equal(sd(out$x),   1,  tolerance = 0.12)
  expect_equal(sd(out$y),  10,  tolerance = 1.2)
})

test_that("means + sd: length mismatch raises error", {
  expect_error(
    sample_covariates_mvtnorm(means = c(a = 1, b = 2), sd = c(1, 2, 3), n_subjects = 10),
    "`sd` must have the same length"
  )
})

test_that("means + sd: reorders sd to match means when both are named", {
  mu  <- c(y = 50, x = 5)           # y first
  sds <- c(x = 1,  y = 10)          # x first — should be reordered
  out <- sample_covariates_mvtnorm(means = mu, sd = sds, n_subjects = 2000, seed = 7)
  expect_named(out, c("y", "x"))
  expect_equal(sd(out$y), 10, tolerance = 1.2)
  expect_equal(sd(out$x),  1, tolerance = 0.12)
})

test_that("means + sd: name mismatch raises error", {
  expect_error(
    sample_covariates_mvtnorm(means = c(a = 1, b = 2), sd = c(a = 1, c = 2), n_subjects = 10),
    "Names of `sd` must match"
  )
})

test_that("means + sd: unnamed sd is assumed to be in the right order", {
  mu  <- c(x = 5, y = 50)
  out <- sample_covariates_mvtnorm(means = mu, sd = c(1, 10), n_subjects = 2000, seed = 7)
  expect_named(out, c("x", "y"))
  expect_equal(sd(out$x),  1, tolerance = 0.12)
  expect_equal(sd(out$y), 10, tolerance = 1.2)
})

test_that("means + sigma: reorders sigma to match means when both are named", {
  mu <- c(y = 20, x = 10)           # y first
  S  <- matrix(c(4, 1, 1, 9), nrow = 2, dimnames = list(c("x","y"), c("x","y")))
  out <- sample_covariates_mvtnorm(means = mu, sigma = S, n_subjects = 100, seed = 1)
  expect_named(out, c("y", "x"))
})

test_that("means + sigma: name mismatch raises error", {
  mu <- c(a = 1, b = 2)
  S  <- matrix(c(1, 0, 0, 1), nrow = 2, dimnames = list(c("a","c"), c("a","c")))
  expect_error(
    sample_covariates_mvtnorm(means = mu, sigma = S, n_subjects = 10),
    "Names of `sigma` must match"
  )
})

test_that("means + sigma: unnamed sigma is assumed to be in the right order", {
  mu <- c(x = 10, y = 20)
  S  <- diag(2)                      # no dimnames
  out <- sample_covariates_mvtnorm(means = mu, sigma = S, n_subjects = 100, seed = 1)
  expect_named(out, c("x", "y"))
})

test_that("means without sigma or sd raises error", {
  expect_error(
    sample_covariates_mvtnorm(means = c(x = 1), n_subjects = 10),
    "either `sigma` or `sd` must also be provided"
  )
})

test_that("data = NULL without means raises error", {
  expect_error(
    sample_covariates_mvtnorm(data = NULL, n_subjects = 10),
    "Either `data` or `means`"
  )
})

test_that("warning is issued when both data and means are provided", {
  dat <- data.frame(x = rnorm(50), y = rnorm(50))
  mu  <- c(x = 0, y = 0)
  S   <- diag(2)
  expect_warning(
    sample_covariates_mvtnorm(data = dat, means = mu, sigma = S, n_subjects = 10),
    "`data` is ignored"
  )
})

test_that("n_subjects is required when data is NULL", {
  mu <- c(x = 0)
  S  <- matrix(1)
  expect_error(
    sample_covariates_mvtnorm(means = mu, sigma = S),
    "n_subjects.*must be specified"
  )
})

test_that("seed produces reproducible output with means + sigma", {
  mu <- c(x = 0, y = 1)
  S  <- diag(2)
  out1 <- sample_covariates_mvtnorm(means = mu, sigma = S, n_subjects = 30, seed = 5)
  out2 <- sample_covariates_mvtnorm(means = mu, sigma = S, n_subjects = 30, seed = 5)
  expect_equal(out1, out2)
})
