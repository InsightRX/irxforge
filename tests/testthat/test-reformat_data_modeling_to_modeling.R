test_that("column names are converted to upper case", {
  dat <- data.frame(id = 1, evid = 0, dv = 5)
  out <- reformat_data_modeling_to_modeling(dat)
  expect_true(all(c("ID", "EVID", "DV") %in% names(out)))
})

test_that("DV is set to zero when EVID is not 0", {
  dat <- data.frame(ID = c(1, 1), EVID = c(0, 1), DV = c(10, 20))
  out <- reformat_data_modeling_to_modeling(dat)
  expect_equal(out$DV, c(10, 0))
})

test_that("MDV column is created when missing", {
  dat <- data.frame(ID = c(1, 1), EVID = c(0, 1), DV = c(10, 0))
  out <- reformat_data_modeling_to_modeling(dat)
  expect_true("MDV" %in% names(out))
  expect_equal(out$MDV, c(0, 1))
})

test_that("existing MDV column is preserved (even if it's wrong)", {
  # TODO: Validate MDV correctness?
  dat <- data.frame(ID = c(1, 1), EVID = c(0, 1), DV = c(10, 0), MDV = c(1, 0))
  out <- reformat_data_modeling_to_modeling(dat)
  expect_equal(out$MDV, dat$MDV)
})

test_that("existing GROUP column is preserved", {
  dat <- data.frame(ID = c(1, 1), EVID = c(0, 1), DV = c(10, 0), GROUP = c(1, 0))
  out <- reformat_data_modeling_to_modeling(dat)
  expect_equal(out$GROUP, dat$GROUP)
})

test_that("existing GROUP column is preserved even when dictionary is provided", {
  dat <- data.frame(ID = c(1, 1), EVID = c(0, 1), DV = c(10, 0), GROUP = c(1, 0))
  out <- reformat_data_modeling_to_modeling(dat, dictionary = list(group = "ID"))
  expect_equal(out$GROUP, dat$GROUP)
})

test_that("GROUP defaults to 1 when missing and no dictionary is provided", {
  dat <- data.frame(ID = c(1, 2), EVID = c(0, 0), DV = c(5, 6))
  out <- reformat_data_modeling_to_modeling(dat)
  expect_true("GROUP" %in% names(out))
  expect_equal(out$GROUP, c(1, 1))
})

test_that("GROUP is created from dictionary mapping when missing", {
  dat <- data.frame(ID = c(1, 2), EVID = c(0, 0), DV = c(5, 6), ARM = c(1, 2))
  dict <- list(group = "ARM")
  out <- reformat_data_modeling_to_modeling(dat, dict)
  expect_true("GROUP" %in% names(out))
  expect_equal(out$GROUP, dat$ARM)
})
