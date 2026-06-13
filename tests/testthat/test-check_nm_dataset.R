# Helper: minimal valid NONMEM dataset
make_nm <- function(n_id = 2, n_obs = 3) {
  do.call(rbind, lapply(seq_len(n_id), function(id) {
    dose <- data.frame(
      ID = id, TIME = 0, DV = NA_real_, MDV = 1, EVID = 1,
      AMT = 100, CMT = 1, stringsAsFactors = FALSE
    )
    obs <- data.frame(
      ID = id,
      TIME = seq_len(n_obs),
      DV = runif(n_obs, 1, 10),
      MDV = 0, EVID = 0, AMT = 0, CMT = 1,
      stringsAsFactors = FALSE
    )
    rbind(dose, obs)
  }))
}

# Helper: dataset with renamed columns (for dictionary tests)
make_nm_renamed <- function(n_id = 2, n_obs = 3) {
  d <- make_nm(n_id = n_id, n_obs = n_obs)
  names(d) <- c("SUBJ", "TAD", "CONC", "MISS", "FLAG", "DOSE", "COMP")
  d
}

test_that("clean dataset passes all applicable checks", {
  d <- make_nm()
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_true(all(res$result %in% c("PASS", "NA")))
  expect_s3_class(res, "data.frame")
  expect_named(res, c("check", "description", "result"))
})

test_that("missing required columns are flagged", {
  d <- data.frame(ID = 1, TIME = 0) # no DV

  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "required_cols"], "FAIL")
})

test_that("duplicate column names are flagged", {
  d <- make_nm()
  names(d)[7] <- "ID" # duplicate
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "unique_colnames"], "FAIL")
})

test_that("special characters in column names are flagged", {
  d <- make_nm()
  names(d)[1] <- "ID#1"
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "colname_chars"], "FAIL")
})

test_that("reserved NONMEM names are detected", {
  d <- make_nm()
  d$THETA <- 1
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "reserved_names"], "FAIL")
})

test_that("extra_reserved catches PK parameter names", {
  d <- make_nm()
  d$CL <- 5
  # Without extra_reserved, CL is not flagged (it's not in the base list)
  res1 <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res1$result[res1$check == "reserved_names"], "PASS")
  # With extra_reserved
  res2 <- check_nm_dataset(d, extra_reserved = c("CL", "V"), verbose = FALSE)
  expect_equal(res2$result[res2$check == "reserved_names"], "FAIL")
})

test_that("negative TIME is flagged", {
  d <- make_nm()
  d$TIME[2] <- -1
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "time_nonneg"], "FAIL")
})

test_that("invalid EVID values are flagged", {
  d <- make_nm()
  d$EVID[1] <- 5
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "evid_values"], "FAIL")
})

test_that("AMT checks work correctly", {
  d <- make_nm()
  # AMT > 0 for observation record
  d$AMT[2] <- 50
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "amt_zero_obs"], "FAIL")
  expect_equal(res$result[res$check == "amt_evid_consistent"], "FAIL")
})

test_that("AMT missing for dosing records is flagged", {
  d <- make_nm()
  d$AMT[1] <- NA
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "amt_nonmissing_dose"], "FAIL")
})

test_that("DV present for dosing records is flagged", {
  d <- make_nm()
  d$DV[1] <- 999  # dose record should not have a real DV

  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "dv_missing_dose"], "FAIL")
})

test_that("MDV = 1 for valid observation (DV > 0) is flagged", {
  d <- make_nm()
  d$MDV[2] <- 1  # observation with DV > 0 should have MDV = 0
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "mdv_nonmissing_obs"], "FAIL")
  expect_equal(res$result[res$check == "mdv_dv_consistent"], "FAIL")
})

test_that("MDV/DV consistency treats DV <= 0 as BLQ (MDV should be 1)", {
  d <- make_nm()
  # Set an observation DV to a negative value (e.g. log-transformed BLQ)
  # and MDV = 0 — this should FAIL because DV <= 0 requires MDV = 1
  d$DV[2] <- -2
  d$MDV[2] <- 0
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "mdv_dv_consistent"], "FAIL")

  # Now set MDV = 1 for the BLQ record — should PASS
  d$MDV[2] <- 1
  res2 <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res2$result[res2$check == "mdv_dv_consistent"], "PASS")
  expect_equal(res2$result[res2$check == "mdv_nonmissing_obs"], "PASS")
})

test_that("exclusion flag checks", {
  d <- make_nm()
  d$EXCL <- c(0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L)
  res <- check_nm_dataset(d, exclusion_flag = "EXCL", verbose = FALSE)
  expect_equal(res$result[res$check == "exclusion_flag"], "PASS")

  # NA in exclusion flag
  d$EXCL[1] <- NA

  res2 <- check_nm_dataset(d, exclusion_flag = "EXCL", verbose = FALSE)
  expect_equal(res2$result[res2$check == "exclusion_flag"], "FAIL")
})

test_that("ID leading zeros flagged", {
  d <- make_nm(n_id = 1)
  d$ID <- "001"
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "id_leading_zeros"], "FAIL")
})

test_that("decreasing TIME within ID is flagged", {
  d <- make_nm(n_id = 1)
  d$TIME <- c(0, 3, 2, 1)
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "time_order"], "FAIL")
})

test_that("character columns are flagged", {
  d <- make_nm()
  d$STUDY <- "A"
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "char_columns"], "FAIL")
})

test_that("string 'NA' values are detected", {
  d <- make_nm()
  d$DV <- as.character(d$DV)
  d$DV[2] <- "NA"
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "na_coding"], "FAIL")
})

test_that("dataset ordering is checked", {
  d <- make_nm(n_id = 2)
  d <- d[sample(nrow(d)), ]  # shuffle
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "data_ordered"], "FAIL")
})

test_that("ADDL/II pairing is checked", {
  d <- make_nm()
  d$ADDL <- 0
  # II missing
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "addl_ii_paired"], "FAIL")
})

test_that("ADDL overlap is detected", {
  d <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 24, 48),
    DV = c(NA, 5, 3),
    MDV = c(1, 0, 0),
    EVID = c(1, 0, 0),
    AMT = c(100, 0, 0),
    CMT = c(1, 1, 1),
    ADDL = c(5, 0, 0),  # expands to TIME = 0, 12, 24, 36, 48 -- overlaps next dose
    II = c(12, 0, 0),
    stringsAsFactors = FALSE
  )
  # No second dose record here, so no overlap to detect.
  # Add a second dose:
  d2 <- rbind(d[1, ], data.frame(
    ID = 1, TIME = 24, DV = NA, MDV = 1, EVID = 1,
    AMT = 100, CMT = 1, ADDL = 0, II = 0, stringsAsFactors = FALSE
  ), d[2:3, ])
  res <- check_nm_dataset(d2, verbose = FALSE)
  # First dose: TIME=0, ADDL=5, II=12 => last expanded at 60 >= next dose at 24
  expect_equal(res$result[res$check == "addl_overlap"], "FAIL")
})

test_that("ADDL overlap detection works on unsorted datasets", {
  # Build an overlapping dataset but shuffle the rows so it's not sorted
  d <- data.frame(
    ID   = c(1, 1, 1, 1),
    TIME = c(0, 24, 24, 48),
    DV   = c(NA, NA, 5, 3),
    MDV  = c(1, 1, 0, 0),
    EVID = c(1, 1, 0, 0),
    AMT  = c(100, 100, 0, 0),
    CMT  = c(1, 1, 1, 1),
    ADDL = c(5, 0, 0, 0),
    II   = c(12, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  # TIME=0, ADDL=5, II=12 => last expanded at 60 >= next dose at 24
  # Shuffle rows so dose at TIME=24 comes before dose at TIME=0
  d_shuffled <- d[c(3, 2, 4, 1), ]
  res <- check_nm_dataset(d_shuffled, verbose = FALSE)
  expect_equal(res$result[res$check == "addl_overlap"], "FAIL")

  # Non-overlapping case: reduce ADDL so expansion ends before next dose
  d2 <- d
  d2$ADDL[1] <- 1  # last expanded at TIME = 12, next dose at 24 → OK
  d2_shuffled <- d2[c(3, 2, 4, 1), ]
  res2 <- check_nm_dataset(d2_shuffled, verbose = FALSE)
  expect_equal(res2$result[res2$check == "addl_overlap"], "PASS")
})

test_that("covariate missing values are detected", {
  d <- make_nm()
  d$WT <- c(70, 80, NA, 75, 65, 90, 85, 70)
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "covariate_missing"], "FAIL")
})

test_that("covariate sentinel values (-999) are detected", {
  d <- make_nm()
  d$WT <- c(70, 80, -999, 75, 65, 90, 85, 70)
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "covariate_missing"], "FAIL")
})

test_that("verbose output produces messages", {
  d <- make_nm()
  expect_message(check_nm_dataset(d, verbose = TRUE), "PASS")
})

test_that("SS values checked for dosing records", {
  d <- make_nm()
  d$SS <- 0
  d$SS[1] <- 2  # invalid: SS should be 0 or 1
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "ss_values"], "FAIL")
})

test_that("RATE column checks", {
  d <- make_nm()
  d$RATE <- 0
  d$RATE[1] <- -3  # invalid
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "rate_dose_values"], "FAIL")
})

test_that("duplicate records are flagged", {
  d <- make_nm(n_id = 1)
  d <- rbind(d, d[2, ])  # duplicate observation record
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "unique_records"], "FAIL")
})

# ── Dictionary tests ──────────────────────────────────────────────────────────

test_that("dictionary maps renamed columns and passes clean dataset", {
  d <- make_nm_renamed()
  dict <- list(
    ID = "SUBJ", TIME = "TAD", DV = "CONC", MDV = "MISS",
    EVID = "FLAG", AMT = "DOSE", CMT = "COMP"
  )
  res <- check_nm_dataset(d, dictionary = dict, verbose = FALSE)
  # All applicable checks should pass (or be NA), none should fail
  expect_true(all(res$result %in% c("PASS", "NA")))
})

test_that("dictionary resolves required columns correctly", {
  # Without dictionary, renamed dataset fails required_cols
  d <- make_nm_renamed()
  res_no_dict <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res_no_dict$result[res_no_dict$check == "required_cols"], "FAIL")

  # With dictionary, it passes
  dict <- list(ID = "SUBJ", TIME = "TAD", DV = "CONC")
  res_dict <- check_nm_dataset(d, dictionary = dict, verbose = FALSE)
  expect_equal(res_dict$result[res_dict$check == "required_cols"], "PASS")
})

test_that("dictionary catches errors in renamed columns", {
  d <- make_nm_renamed()
  d$TAD[2] <- -1  # negative TIME (via TAD)
  dict <- list(
    ID = "SUBJ", TIME = "TAD", DV = "CONC", MDV = "MISS",
    EVID = "FLAG", AMT = "DOSE", CMT = "COMP"
  )
  res <- check_nm_dataset(d, dictionary = dict, verbose = FALSE)
  expect_equal(res$result[res$check == "time_nonneg"], "FAIL")
})

test_that("dictionary works for AMT/EVID consistency", {
  d <- make_nm_renamed()
  d$DOSE[3] <- 50  # AMT > 0 for an observation (EVID = 0)
  dict <- list(
    ID = "SUBJ", TIME = "TAD", DV = "CONC", MDV = "MISS",
    EVID = "FLAG", AMT = "DOSE", CMT = "COMP"
  )
  res <- check_nm_dataset(d, dictionary = dict, verbose = FALSE)
  expect_equal(res$result[res$check == "amt_zero_obs"], "FAIL")
  expect_equal(res$result[res$check == "amt_evid_consistent"], "FAIL")
})

test_that("partial dictionary works (unmapped columns keep standard names)", {
  d <- make_nm()
  names(d)[1] <- "SUBJ"  # only rename ID
  dict <- list(ID = "SUBJ")
  res <- check_nm_dataset(d, dictionary = dict, verbose = FALSE)
  expect_equal(res$result[res$check == "required_cols"], "PASS")
  expect_true(all(res$result %in% c("PASS", "NA")))
})

test_that("dictionary is case-insensitive for keys", {
  d <- make_nm_renamed()
  dict <- list(
    id = "SUBJ", time = "TAD", dv = "CONC", mdv = "MISS",
    evid = "FLAG", amt = "DOSE", cmt = "COMP"
  )
  res <- check_nm_dataset(d, dictionary = dict, verbose = FALSE)
  expect_equal(res$result[res$check == "required_cols"], "PASS")
})

test_that("unknown dictionary keys produce a warning", {
  d <- make_nm()
  expect_warning(
    check_nm_dataset(d, dictionary = list(FOOBAR = "X"), verbose = FALSE),
    "not recognised"
  )
})

test_that("dictionary verbose output shows mapping", {
  d <- make_nm()
  names(d)[1] <- "SUBJ"
  expect_message(
    check_nm_dataset(d, dictionary = list(ID = "SUBJ"), verbose = TRUE),
    "Dictionary mapping: ID -> SUBJ"
  )
})

# ── Non-numeric TIME robustness ───────────────────────────────────────────────

test_that("non-numeric TIME with ADDL/II does not crash the check", {
  # Two dose records so the overlap-expansion arithmetic is reached; a character
  # TIME used to crash `time_vec + ADDL * II` and abort the whole function.
  d <- data.frame(
    ID = 1,
    TIME = c("0", "5", "10"),       # character clock/date-style TIME
    DV = c(NA, NA, 8),
    MDV = c(1, 1, 0),
    EVID = c(1, 1, 0),
    AMT = c(100, 100, 0),
    CMT = 1,
    ADDL = c(5, 0, 0),
    II = c(12, 0, 0),
    stringsAsFactors = FALSE
  )
  res <- check_nm_dataset(d, verbose = FALSE)   # must not error
  expect_s3_class(res, "data.frame")
  expect_equal(res$result[res$check == "time_numeric"], "FAIL")
})

test_that("unique_records is skipped (NA) when TIME is non-numeric", {
  d <- make_nm(n_id = 1)
  d <- rbind(d, d[2, ])             # duplicate on a numeric key
  d$TIME <- as.character(d$TIME)    # character clock-of-day TIME → unreliable key
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "unique_records"], "NA")
})

test_that("unique_records is skipped (NA) when ID does not resolve", {
  d <- make_nm()
  names(d)[names(d) == "ID"] <- "NSID"   # no canonical ID, no dictionary
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "unique_records"], "NA")
})

test_that("char_columns names the offending column(s)", {
  d <- make_nm()
  d$AMT_UNIT <- "mg"               # character label/unit sidecar column
  res <- check_nm_dataset(d, verbose = FALSE)
  expect_equal(res$result[res$check == "char_columns"], "FAIL")
  expect_match(res$description[res$check == "char_columns"], "AMT_UNIT")
})
