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

test_that("repeat_doses = NULL produces no ADDL/II columns", {
  dat <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 12, 24),
    AMT = c(100, 100, 100),
    DV = c(NA, 5, 3)
  )
  out <- reformat_data_nca_to_modeling(data = dat)
  expect_false("ADDL" %in% names(out))
  expect_false("II" %in% names(out))
})

test_that("repeat_doses with explicit n adds correct ADDL/II", {
  dat <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 12, 24),
    AMT = c(100, 100, 100),
    DV = c(NA, 5, 3)
  )
  out <- reformat_data_nca_to_modeling(data = dat, repeat_doses = list(n = 5, interval = 12))
  dose_rows <- out[out$EVID == 1, ]
  obs_rows  <- out[out$EVID == 0, ]
  expect_equal(dose_rows$ADDL, 4)
  expect_equal(dose_rows$II,   12)
  expect_true(all(obs_rows$ADDL == 0))
  expect_true(all(obs_rows$II   == 0))
})

test_that("repeat_doses without n infers ADDL per subject from max obs time", {
  dat <- data.frame(
    ID   = c(1, 1, 1,  2, 2, 2),
    TIME = c(0, 12, 24, 0, 6, 12),
    AMT  = c(100, 100, 100, 200, 200, 200),
    DV   = c(NA, 5, 3, NA, 8, 6)
  )
  out <- reformat_data_nca_to_modeling(data = dat, repeat_doses = list(interval = 12))
  dose_rows <- out[out$EVID == 1, ]
  # Subject 1: max obs time = 24, ceiling(24/12) - 1 = 1
  expect_equal(dose_rows$ADDL[dose_rows$ID == 1], 1)
  # Subject 2: max obs time = 12, ceiling(12/12) - 1 = 0 (pmax guard)
  expect_equal(dose_rows$ADDL[dose_rows$ID == 2], 0)
  expect_true(all(dose_rows$II == 12))
  obs_rows <- out[out$EVID == 0, ]
  expect_true(all(obs_rows$ADDL == 0))
  expect_true(all(obs_rows$II   == 0))
})

test_that("repeat_doses without interval raises an error", {
  dat <- data.frame(
    ID = c(1, 1),
    TIME = c(0, 12),
    AMT = c(100, 100),
    DV = c(NA, 5)
  )
  expect_error(
    reformat_data_nca_to_modeling(data = dat, repeat_doses = list(n = 3)),
    "interval"
  )
})

test_that("categorical_mapping as character vector encodes specified columns with mode=0", {
  dat <- data.frame(
    ID = c(1, 1, 2, 2, 3, 3),
    TIME = c(0, 1, 0, 1, 0, 1),
    AMT = c(100, 100, 200, 200, 100, 100),
    DV = c(NA, 5, NA, 4, NA, 3),
    SEX = c("M", "M", "F", "F", "M", "M"),
    RACE = c("WHITE", "WHITE", "BLACK", "BLACK", "WHITE", "WHITE")
  )
  out <- reformat_data_nca_to_modeling(
    data = dat,
    covariates = c("SEX", "RACE"),
    categorical_mapping = c("SEX", "RACE")
  )
  # SEX: M is most common (4 rows) -> 0, F -> 1
  expect_true(all(out$SEX[out$ORIGID == 1] == "0"))
  expect_true(all(out$SEX[out$ORIGID == 2] == "1"))
  expect_true(all(out$SEX[out$ORIGID == 3] == "0"))
  # Mapping attribute attached
  mapping <- attr(out, "categorical_mapping")
  expect_true(is.data.frame(mapping))
  expect_equal(names(mapping), c("column", "original_value", "encoded_value"))
  expect_true("SEX" %in% mapping$column)
  expect_true("RACE" %in% mapping$column)
})

test_that("categorical_mapping as data.frame applies user mapping in NCA reformatting", {
  dat <- data.frame(
    ID = c(1, 1, 2, 2),
    TIME = c(0, 1, 0, 1),
    AMT = c(100, 100, 200, 200),
    DV = c(NA, 5, NA, 4),
    SEX = c("M", "M", "F", "F")
  )
  user_map <- data.frame(
    column = c("SEX", "SEX"),
    original_value = c("M", "F"),
    encoded_value = c(0, 1)
  )
  out <- reformat_data_nca_to_modeling(
    data = dat,
    covariates = c("SEX"),
    categorical_mapping = user_map
  )
  mapping <- attr(out, "categorical_mapping")
  expect_true(is.data.frame(mapping))
  expect_equal(nrow(mapping), 2)
})

test_that("categorical_mapping = NULL preserves existing blanket conversion", {
  dat <- data.frame(
    ID = c(1, 2),
    TIME = c(0, 0),
    AMT = c(100, 100),
    DV = c(NA, 5),
    SEX = c("M", "F")
  )
  out <- reformat_data_nca_to_modeling(data = dat, covariates = c("SEX"))
  expect_true(is.numeric(out$SEX))
  expect_null(attr(out, "categorical_mapping"))
})

test_that("reformat_data_nca_to_modeling handles multiple doses per subject", {
  # Create data with multiple dose events per subject (row-wise dosing)
  dat <- data.frame(
    USUBJID = c(1, 1, 1, 1, 1, 1,
                2, 2, 2, 2, 2, 2),
    PCTPTNUM = c(0, 1, 2, 24, 25, 26,
                 0, 1, 2, 24, 25, 26),
    PCORRES = c(0, 10, 5, 0, 12, 6,
                0, 15, 8, 0, 18, 9),
    EXDOSE = c(100, NA, NA, 100, NA, NA,
               200, NA, NA, 200, NA, NA)
  )

  dictionary <- list(
    subject_id = "USUBJID",
    conc = "PCORRES",
    time = "PCTPTNUM",
    dose = "EXDOSE"
  )

  result <- reformat_data_nca_to_modeling(
    data = dat,
    dictionary = dictionary
  )

  dose_rows <- result[result$EVID == 1, ]

  # Should have 4 dose events total (2 per subject)
  expect_equal(nrow(dose_rows), 4)

  # Check dose times are preserved
  expect_true(all(c(0, 24) %in% dose_rows$TIME[dose_rows$ID == 1]))
  expect_true(all(c(0, 24) %in% dose_rows$TIME[dose_rows$ID == 2]))

  # Check dose amounts
  expect_equal(sum(dose_rows$AMT[dose_rows$ID == 1]), 200)
  expect_equal(sum(dose_rows$AMT[dose_rows$ID == 2]), 400)
})
