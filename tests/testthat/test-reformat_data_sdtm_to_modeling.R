# Helper to create minimal SDTM test data for 2 subjects.
# Uses CDISC-compatible formats: numeric USUBJID segments, matching
# drug names across PC and EX, and valid EXDOSFRQ values.
create_sdtm_test_data <- function() {
  dm <- data.frame(
    STUDYID = "01",
    DOMAIN = "DM",
    USUBJID = c("01-01-001", "01-01-002"),
    SUBJID = c("001", "002"),
    RFSTDTC = c("2020-01-01", "2020-01-01"),
    RFENDTC = c("2020-02-01", "2020-02-01"),
    RFXSTDTC = c("2020-01-01", "2020-01-01"),
    RFXENDTC = c("2020-02-01", "2020-02-01"),
    RFICDTC = "",
    RFPENDTC = "",
    DTHDTC = "",
    DTHFL = "",
    SITEID = c(1, 1),
    AGE = c(45, 60),
    AGEU = "YEARS",
    SEX = c("M", "F"),
    RACE = c("WHITE", "BLACK OR AFRICAN AMERICAN"),
    ETHNIC = c("NOT HISPANIC OR LATINO", "HISPANIC OR LATINO"),
    ARMCD = c("TRT", "TRT"),
    ARM = c("DrugA 100mg", "DrugA 100mg"),
    ACTARMCD = c("TRT", "TRT"),
    ACTARM = c("DrugA 100mg", "DrugA 100mg"),
    COUNTRY = c("USA", "USA"),
    DMDTC = c("2019-12-15", "2019-12-20"),
    DMDY = c(-17, -12),
    stringsAsFactors = FALSE
  )

  ex <- data.frame(
    STUDYID = "01",
    DOMAIN = "EX",
    USUBJID = c("01-01-001", "01-01-002"),
    EXSEQ = c(1, 1),
    EXTRT = c("DrugA", "DrugA"),
    EXDOSE = c(100, 100),
    EXDOSU = "mg",
    EXDOSFRM = "TABLET",
    EXDOSFRQ = "QD",
    EXROUTE = "ORAL",
    VISITNUM = c(1, 1),
    VISIT = "DAY 1",
    VISITDY = c(1, 1),
    EXSTDTC = c("2020-01-01", "2020-01-01"),
    EXENDTC = c("2020-01-01", "2020-01-01"),
    EXSTDY = c(1, 1),
    EXENDY = c(1, 1),
    stringsAsFactors = FALSE
  )

  pc <- data.frame(
    STUDYID = "01",
    DOMAIN = "PC",
    USUBJID = rep(c("01-01-001", "01-01-002"), each = 4),
    PCSEQ = rep(1:4, 2),
    PCTESTCD = "DRUGA",
    PCTEST = "DrugA",
    PCORRES = c("0.5", "2.1", "1.8", "0.9", "0.6", "2.5", "2.0", "1.1"),
    PCORRESU = "ug/mL",
    PCSTRESC = c("0.5", "2.1", "1.8", "0.9", "0.6", "2.5", "2.0", "1.1"),
    PCSTRESN = c(0.5, 2.1, 1.8, 0.9, 0.6, 2.5, 2.0, 1.1),
    PCSTRESU = "ug/mL",
    PCNAM = "Test Lab",
    PCSPEC = "PLASMA",
    PCLLOQ = 0.01,
    VISIT = "DAY 1",
    VISITNUM = 1,
    PCDTC = c(
      "2020-01-01T00:30:00", "2020-01-01T02:00:00",
      "2020-01-01T04:00:00", "2020-01-01T08:00:00",
      "2020-01-01T00:30:00", "2020-01-01T02:00:00",
      "2020-01-01T04:00:00", "2020-01-01T08:00:00"
    ),
    PCDY = 1,
    PCTPT = c("30 min", "2h", "4h", "8h", "30 min", "2h", "4h", "8h"),
    PCTPTNUM = c(0.5, 2, 4, 8, 0.5, 2, 4, 8),
    stringsAsFactors = FALSE
  )

  list(dm = dm, ex = ex, pc = pc)
}

# Helper to build ADSL from DM (mimics what a user might provide)
create_adsl_from_dm <- function(dm) {
  adsl <- dm
  adsl$TRTSDT <- as.Date(adsl$RFXSTDTC)
  adsl$TRTSDTM <- as.POSIXct(adsl$RFXSTDTC, tz = "UTC")
  adsl$TRT01P <- adsl$ARM
  adsl$TRT01A <- adsl$ACTARM
  adsl
}

expected_columns <- c(
  "ID", "TIME", "DV", "MDV", "EVID", "SS", "II", "AMT",
  "SEXN", "AGE", "WT", "ROUTE", "FORM", "COHORT", "SITEID",
  "RACE", "ETHNIC", "COUNTRY"
)

test_that("basic SDTM reformatting with dm, ex, pc produces NONMEM-style dataset", {
  data <- create_sdtm_test_data()
  result <- suppressWarnings(reformat_data_sdtm_to_modeling(data))

  expect_s3_class(result, "data.frame")
  expect_true(all(expected_columns %in% names(result)))
  expect_gt(nrow(result), 0)
})

test_that("dose records have EVID=1 and observations have EVID=0", {
  data <- create_sdtm_test_data()
  result <- suppressWarnings(reformat_data_sdtm_to_modeling(data))

  dose_rows <- result[result$EVID == 1, ]
  obs_rows <- result[result$EVID == 0, ]

  expect_gt(nrow(dose_rows), 0)
  expect_gt(nrow(obs_rows), 0)
  # Dose records have AMT set
  expect_true(all(!is.na(dose_rows$AMT)))
  expect_true(all(dose_rows$AMT > 0))
  # Observation records have AMT = NA
  expect_true(all(is.na(obs_rows$AMT)))
})

test_that("MDV is 1 for dose records and 0 for observations with non-missing DV", {
  data <- create_sdtm_test_data()
  result <- suppressWarnings(reformat_data_sdtm_to_modeling(data))

  dose_rows <- result[result$EVID == 1, ]
  obs_rows <- result[result$EVID == 0 & !is.na(result$DV), ]

  expect_true(all(dose_rows$MDV == 1))
  expect_true(all(obs_rows$MDV == 0))
})

test_that("ADSL derived from DM produces same result as explicit ADSL", {
  data <- create_sdtm_test_data()

  # Case 1: using DM (ADSL derived internally)
  result_dm <- suppressWarnings(reformat_data_sdtm_to_modeling(data))

  # Case 2: using explicit ADSL
  data_adsl <- list(
    pc = data$pc,
    ex = data$ex,
    adsl = create_adsl_from_dm(data$dm)
  )
  result_adsl <- suppressWarnings(reformat_data_sdtm_to_modeling(data_adsl))

  expect_equal(dim(result_dm), dim(result_adsl))
  expect_equal(names(result_dm), names(result_adsl))
  expect_equal(result_dm$ID, result_adsl$ID)
  expect_equal(result_dm$DV, result_adsl$DV)
  expect_equal(result_dm$AMT, result_adsl$AMT)
  expect_equal(result_dm$EVID, result_adsl$EVID)
})

test_that("column names are uppercased internally (lowercase input works)", {
  data <- create_sdtm_test_data()
  # lowercase all column names
  for (key in names(data)) {
    names(data[[key]]) <- tolower(names(data[[key]]))
  }
  result <- suppressWarnings(reformat_data_sdtm_to_modeling(data))

  expect_s3_class(result, "data.frame")
  expect_true(all(expected_columns %in% names(result)))
})

test_that("covariates are correctly derived from DM", {
  data <- create_sdtm_test_data()
  result <- suppressWarnings(reformat_data_sdtm_to_modeling(data))

  # SEXN: M=1, F=2
  subj1 <- result[result$ID == "001", ]
  subj2 <- result[result$ID == "002", ]
  expect_equal(unique(subj1$SEXN), 1)
  expect_equal(unique(subj2$SEXN), 2)

  # AGE carried through
  expect_equal(unique(subj1$AGE), 45)
  expect_equal(unique(subj2$AGE), 60)

  # RACE carried through
  expect_equal(unique(subj1$RACE), "WHITE")
})

test_that("ROUTE and FORM are lowercase", {
  data <- create_sdtm_test_data()
  result <- suppressWarnings(reformat_data_sdtm_to_modeling(data))

  dose_rows <- result[result$EVID == 1, ]
  expect_true(all(dose_rows$ROUTE == "oral"))
  expect_true(all(dose_rows$FORM == "tablet"))
})

test_that("WT is NA when VS domain is not provided", {
  data <- create_sdtm_test_data()
  result <- suppressWarnings(reformat_data_sdtm_to_modeling(data))

  expect_true(all(is.na(result$WT)))
})

test_that("param_lookup is built dynamically from PC data", {
  data <- create_sdtm_test_data()
  # The test data uses PCTESTCD = "DRUGA", not "DRUGX"
  # This should work without error (previously hardcoded to DRUGX)
  result <- suppressWarnings(reformat_data_sdtm_to_modeling(data))
  expect_gt(nrow(result), 0)
})

test_that("observations with missing DV are filtered out", {
  data <- create_sdtm_test_data()
  # Set one observation DV to NA
  data$pc$PCSTRESN[1] <- NA
  result <- suppressWarnings(reformat_data_sdtm_to_modeling(data))

  # No rows with EVID=0 and DV=NA should remain
  obs_na <- result[result$EVID == 0 & is.na(result$DV), ]
  expect_equal(nrow(obs_na), 0)
})

test_that("datetimes with seconds are handled correctly", {
  data <- create_sdtm_test_data()
  # PC data already has seconds (e.g. "2020-01-01T00:30:00")
  # This should not error with admiral >= 1.4.0
  expect_no_error(suppressWarnings(reformat_data_sdtm_to_modeling(data)))
})

test_that("subjects with zero dose are excluded", {
  data <- create_sdtm_test_data()
  # Set one subject's dose to 0
  data$ex$EXDOSE[2] <- 0
  result <- suppressWarnings(reformat_data_sdtm_to_modeling(data))

  # Only subject 001 should remain (002 had zero dose)
  expect_equal(sort(unique(result$ID)), "001")
})

test_that("two subjects produce correct number of unique IDs", {
  data <- create_sdtm_test_data()
  result <- suppressWarnings(reformat_data_sdtm_to_modeling(data))

  expect_equal(length(unique(result$ID)), 2)
})

test_that("TIME values correspond to PCTPTNUM for observations", {
  data <- create_sdtm_test_data()
  result <- suppressWarnings(reformat_data_sdtm_to_modeling(data))

  obs <- result[result$EVID == 0, ]
  # All observation times should be from the PCTPTNUM values: 0.5, 2, 4, 8
  expect_true(all(obs$TIME %in% c(0.5, 2, 4, 8)))
})

test_that("TIME is 0 for dose records", {
  data <- create_sdtm_test_data()
  result <- suppressWarnings(reformat_data_sdtm_to_modeling(data))

  doses <- result[result$EVID == 1, ]
  expect_true(all(doses$TIME == 0))
})
