test_that("mvtnorm calls sample_covariates_mvtnorm()", {
  local_mocked_bindings(sample_covariates_mvtnorm = function(...) "success")
  expect_equal(sample_covariates(method = "mvtnorm"), "success")
})

test_that("mice calls sample_covariates_mice()", {
  local_mocked_bindings(sample_covariates_mice = function(...) "success")
  expect_equal(sample_covariates(method = "mice"), "success")
})

test_that("bootstrap calls sample_covariates_bootstrap()", {
  local_mocked_bindings(sample_covariates_bootstrap = function(...) "success")
  expect_equal(sample_covariates(method = "bootstrap"), "success")
})

test_that("errors on invalid method", {
  expect_error(sample_covariates(method = "invalid"), class = "rlang_error")
})
