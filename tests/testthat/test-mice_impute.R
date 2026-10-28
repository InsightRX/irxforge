make_test_data <- function(n = 200, seed = 1) {
  set.seed(seed)
  age <- round(stats::runif(n, 20, 80))
  wt <- round(40 + 0.5 * age + stats::rnorm(n, 0, 5), 1)
  sex <- factor(ifelse(stats::runif(n) < stats::plogis((wt - 70) / 5), "M", "F"))
  race <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
  data.frame(AGE = age, WT = wt, SEX = sex, RACE = race)
}

test_that("mice_impute returns m complete data sets with same structure", {
  dat <- make_test_data()
  dat$WT[1:20] <- NA
  dat$SEX[21:40] <- NA
  dat$RACE[41:60] <- NA
  out <- mice_impute(dat, m = 3, maxit = 2)
  expect_length(out, 3)
  for (x in out) {
    expect_false(anyNA(x))
    expect_named(x, names(dat))
    expect_equal(nrow(x), nrow(dat))
    expect_equal(levels(x$SEX), levels(dat$SEX))
    expect_equal(levels(x$RACE), levels(dat$RACE))
    # observed values are untouched
    expect_equal(x$WT[!is.na(dat$WT)], dat$WT[!is.na(dat$WT)])
    expect_equal(x$SEX[!is.na(dat$SEX)], dat$SEX[!is.na(dat$SEX)])
  }
})

test_that("pmm only imputes observed values", {
  dat <- make_test_data()
  dat$WT[1:50] <- NA
  out <- mice_impute(dat, m = 1, maxit = 3)[[1]]
  expect_true(all(out$WT[1:50] %in% dat$WT[51:200]))
})

test_that("norm imputes values not necessarily observed", {
  dat <- make_test_data()
  dat$WT[1:50] <- NA
  out <- mice_impute(dat, method = c(WT = "norm"), m = 1, maxit = 3)[[1]]
  expect_false(anyNA(out$WT))
  expect_false(all(out$WT[1:50] %in% dat$WT[51:200]))
})

test_that("imputations preserve correlation between covariates", {
  dat <- make_test_data(n = 400)
  obs_cor <- stats::cor(dat$AGE, dat$WT)
  dat$WT[1:200] <- NA
  for (meth in c("pmm", "norm")) {
    out <- mice_impute(dat, method = c(WT = meth), m = 1, maxit = 5)[[1]]
    imp_cor <- stats::cor(out$AGE[1:200], out$WT[1:200])
    expect_gt(imp_cor, obs_cor - 0.2)
  }
})

test_that("logreg imputation follows relation with predictors", {
  dat <- make_test_data(n = 1000)
  truth <- dat$SEX
  dat$SEX[1:500] <- NA
  out <- mice_impute(dat, m = 1, maxit = 5)[[1]]
  # imputed sex should be associated with weight in the same direction
  p_male_heavy <- mean(out$SEX[1:500][out$WT[1:500] > 75] == "M")
  p_male_light <- mean(out$SEX[1:500][out$WT[1:500] < 65] == "M")
  expect_gt(p_male_heavy, p_male_light)
})

test_that("polyreg imputes all levels with approximately observed frequencies", {
  dat <- make_test_data(n = 1000)
  dat$RACE[1:500] <- NA
  out <- mice_impute(dat, m = 1, maxit = 3)[[1]]
  freq <- prop.table(table(out$RACE[1:500]))
  expect_setequal(names(freq), c("A", "B", "C"))
  expect_true(all(freq > 0.2))
})

test_that("perfect prediction does not break logreg", {
  dat <- data.frame(
    WT = c(50, 52, 54, 56, 80, 82, 84, 86, 51, 85),
    SEX = factor(c("F", "F", "F", "F", "M", "M", "M", "M", NA, NA))
  )
  out <- mice_impute(dat, m = 1, maxit = 2)[[1]]
  expect_false(anyNA(out$SEX))
})

test_that("categorical variable with single observed level is imputed as that level", {
  dat <- data.frame(
    WT = c(50, 60, 70, 80),
    SEX = factor(c("F", "F", NA, NA), levels = c("F", "M"))
  )
  out <- mice_impute(dat, m = 1, maxit = 1)[[1]]
  expect_equal(as.character(out$SEX), rep("F", 4))
  expect_equal(levels(out$SEX), c("F", "M"))
})

test_that("predictors argument limits variables used for prediction", {
  dat <- make_test_data()
  dat$WT[1:20] <- NA
  dat$ID <- seq_len(nrow(dat))
  expect_no_error(
    mice_impute(dat, predictors = c("AGE", "SEX"), m = 1, maxit = 1)
  )
})

test_that("collinear and constant predictors are handled", {
  dat <- make_test_data()
  dat$AGE2 <- dat$AGE * 2
  dat$CONST <- 1
  dat$WT[1:20] <- NA
  out <- mice_impute(dat, m = 1, maxit = 2)[[1]]
  expect_false(anyNA(out))
})

test_that("variables without method are not imputed and not used as predictors", {
  dat <- make_test_data()
  dat$NOTE <- c(NA, rep("x", nrow(dat) - 1))
  dat$WT[1:20] <- NA
  out <- mice_impute(dat, m = 1, maxit = 1)[[1]]
  expect_false(anyNA(out$WT))
  expect_true(is.na(out$NOTE[1]))
})

test_that("informative errors", {
  dat <- data.frame(WT = c(NA_real_, NA_real_), AGE = c(1, 2))
  expect_error(mice_impute(dat), "no observed values")
  dat <- data.frame(WT = c(NA, 2), AGE = c(1, 2))
  expect_error(mice_impute(dat, method = c(WT = "cart")), "Unsupported")
  dat <- data.frame(SEX = factor(c(NA, "M")), AGE = c(1, 2))
  expect_error(mice_impute(dat, method = c(SEX = "pmm")), "requires numeric")
})

test_that("mice_impute is reproducible with seed", {
  dat <- make_test_data()
  dat$WT[1:20] <- NA
  set.seed(42)
  out1 <- mice_impute(dat, m = 1, maxit = 2)
  set.seed(42)
  out2 <- mice_impute(dat, m = 1, maxit = 2)
  expect_identical(out1, out2)
})

test_that("mice_match_donors picks among nearest donors", {
  yhat_obs <- c(1, 2, 3, 10, 11, 12, 100)
  idx <- replicate(50, mice_match_donors(yhat_obs, 11.2, donors = 3))
  expect_true(all(idx %in% c(4, 5, 6)))
  idx <- mice_match_donors(yhat_obs, c(-5, 500), donors = 1)
  expect_equal(idx, c(1L, 7L))
})

test_that("categorical imputation is numerically stable for sparse, high-dimensional data", {
  for (s in 1:10) {
    set.seed(s)
    dat <- as.data.frame(replicate(10, stats::rnorm(6)))
    dat$RACE <- factor(c("A", "B", "C", NA, NA, NA))
    out <- mice_impute(dat, m = 1, maxit = 3)[[1]]
    expect_false(anyNA(out$RACE))
  }
})

test_that("extreme linear predictors do not produce NaN probabilities", {
  dat <- data.frame(
    X = c(0, 1, 2, 1e6, -1e6),
    RACE = factor(c("A", "B", "C", NA, NA))
  )
  out <- mice_impute(dat, method = c(X = ""), m = 1, maxit = 1)[[1]]
  expect_false(anyNA(out$RACE))
})

test_that("linear predictors overflowing to Inf do not produce NaN probabilities", {
  dat <- data.frame(
    X = c(0, 1e-8, 2e-8, 1e300, -1e300),
    RACE = factor(c("A", "B", "C", NA, NA))
  )
  for (s in 1:20) {
    set.seed(s)
    out <- mice_impute(dat, method = c(X = ""), m = 1, maxit = 1)[[1]]
    expect_false(anyNA(out$RACE))
  }
})
