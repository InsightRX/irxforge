test_that("basic reformatting creates NONMEM-style dataset", {
  nm_cols <- c("ID", "TIME", "CMT", "EVID", "MDV", "DV", "AMT", "GROUP", "ORIGID")
  dat <- data.frame(
    ID = c(1, 1, 1, 2, 2),
    TIME = c(0, 1, 2, 0, 1),
    AMT = c(100, 100, 100, 200, 200),
    DV = c(NA, 5, 3, NA, 4),
    GROUP = c("A", "A", "A", "B", "B")
  )
  out <- reformat_data_nca_to_modeling(data = dat)
  
  # Required NONMEM columns exist
  expect_true(all(nm_cols %in% names(out)))
  # IDs are sequential integers
  expect_equal(unique(out$ID), c(1, 2))
  # Both dose and observation records are present
  expect_true(any(out$EVID == 1)) # doses
  expect_true(any(out$EVID == 0)) # observations
  # Doses have AMT > 0 and DV = 0
  expect_true(all(out$DV[out$EVID == 1] == 0))
  expect_true(all(out$AMT[out$EVID == 1] > 0))
  # Observations have AMT = 0
  expect_true(all(out$AMT[out$EVID == 0] == 0))
})

test_that("GROUP is created when missing from data", {
  dat <- data.frame(
    ID = c("01", "01", "02"),
    TIME = c(0, 1, 0),
    AMT = c(100, 100, 200),
    DV = c(NA, 5, NA)
  )
  out <- reformat_data_nca_to_modeling(data = dat)
  expect_true("GROUP" %in% names(out))
  expect_equal(unique(out$GROUP), 1)
})

test_that("non-numeric DV values are removed", {
  dat <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(0, 1, 2, 3),
    AMT = c(100, 100, 100, 100),
    DV = c(NA, "<LLOQ", 4, TRUE)
  )
  out <- reformat_data_nca_to_modeling(data = dat)
  expect_false(any(out$DV == -99))
  expect_equal(out$TIME, c(0, 2))
  expect_equal(out$DV, c(0, 4))
})

test_that("covariates are retained (and converted to numeric factors)", {
  dat <- data.frame(
    ID = c(1, 2),
    TIME = c(0, 0),
    AMT = c(100, 100),
    DV = c(NA, 5),
    SEX = c("M", "F"),
    WT = c(70, 60)
  )
  out <- reformat_data_nca_to_modeling(data = dat, covariates = c("SEX", "WT"))
  expect_true(all(c("SEX", "WT") %in% names(out)))
  expect_true(is.numeric(out$SEX)) # Character covariates converted to numeric
  expect_true(is.numeric(out$WT))
})

test_that("dose records are reduced to one per subject when dose is column-wise", {
  dat <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 1, 2),
    AMT = c(100, 100, 100),
    DV = c(NA, 4, 3)
  )
  out <- reformat_data_nca_to_modeling(data = dat)
  expect_equal(sum(out$EVID == 1), 1) # Only one dose record per subject at t = 0
  expect_equal(out$TIME[out$EVID == 1], 0)
})
