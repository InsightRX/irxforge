test_that("anonymize_dataset creates anonymized NONMEM-style data", {
  dat <- data.frame(
    SUBJ = c("A", "A", "A", "B", "B", "B"),
    TAD = c(0, 1, 2, 0, 1, 2),
    EVENT = c(1, 0, 0, 1, 0, 0),
    DOSE = c(100, 0, 0, 200, 0, 0),
    CONC = c(0, 4, 3, 0, 8, 6),
    INF_RATE = c(0, 0, 0, 10, 0, 0),
    WT = c(70, 70, 71, 80, 80, 81),
    SCR = c(1.0, 1.1, 1.1, 0.8, 0.8, 0.9),
    PATIENT_NAME = c("Ann", "Ann", "Ann", "Bob", "Bob", "Bob")
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)

  local_mocked_bindings(
    sample_covariates_mice_timevarying = function(
      data,
      id_var,
      time_var,
      time_varying_covs,
      design_match,
      design_id_var,
      n_subjects,
      ...
    ) {
      expect_equal(design_match, "propensity")
      expect_equal(design_id_var, ".design_id")
      data.frame(
        SUBJ = c(1, 1, 1, 2, 2, 2),
        .design_id = c("B", "B", "B", "A", "A", "A"),
        TAD = c(0, 1, 2, 0, 1, 2),
        WT = c(75, 76, 76, 65, 65, 66),
        SCR = c(0.9, 0.9, 1.0, 1.2, 1.1, 1.1)
      )
    },
    simulate_anonymized_concentrations = function(model_file, data, seed = NULL) {
      expect_equal(model_file, model)
      expect_equal(data$ID, c(1, 1, 1, 2, 2, 2))
      expect_equal(data$AMT, c(200, 0, 0, 100, 0, 0))
      expect_equal(data$RATE, c(10, 0, 0, 0, 0, 0))
      expect_true(all(data$DV == 0))
      data.frame(ID = c(1, 1, 2, 2), TIME = c(1, 2, 1, 2), DV_SIM = c(7, 8, 3, 4))
    }
  )

  out <- anonymize_dataset(
    data = dat,
    covariates = c("WT", "SCR"),
    model_file = model,
    dictionary = list(
      ID = "SUBJ",
      TIME = "TAD",
      EVID = "EVENT",
      AMT = "DOSE",
      DV = "CONC",
      RATE = "INF_RATE"
    ),
    seed = 11
  )

  expect_named(out, c("ID", "TIME", "EVID", "AMT", "RATE", "DV", "WT", "SCR"))
  expect_equal(unique(out$ID), c(1, 2))
  expect_false("PATIENT_NAME" %in% names(out))
  expect_equal(out$DV, c(0, 7, 8, 0, 3, 4))
  expect_equal(out$WT, c(75, 76, 76, 65, 65, 66))
})

test_that("sigdig rounds covariates and simulated concentrations", {
  dat <- data.frame(
    SUBJ = c("A", "A", "B", "B"),
    TAD = c(0, 1, 0, 1),
    EVENT = c(1, 0, 1, 0),
    DOSE = c(100, 0, 200, 0),
    CONC = c(0, 4, 0, 8),
    WT = c(70, 70, 80, 80),
    SCR = c(1.0, 1.1, 0.8, 0.9)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)

  mock_bindings <- list(
    sample_covariates_mice_timevarying = function(data, id_var, time_var,
                                                  time_varying_covs, design_match,
                                                  design_id_var, n_subjects, ...) {
      data.frame(
        SUBJ = c(1, 1, 2, 2),
        .design_id = c("A", "A", "B", "B"),
        TAD = c(0, 1, 0, 1),
        WT = c(70.123456, 70.123456, 80.987654, 80.987654),
        SCR = c(1.0123456, 1.0123456, 0.87654321, 0.87654321)
      )
    },
    simulate_anonymized_concentrations = function(model_file, data, seed = NULL) {
      data.frame(ID = c(1, 2), TIME = c(1, 1), DV_SIM = c(4.7654321, 8.1234567))
    }
  )

  args <- list(
    data = dat, covariates = c("WT", "SCR"), model_file = model,
    dictionary = list(ID = "SUBJ", TIME = "TAD", EVID = "EVENT",
                      AMT = "DOSE", DV = "CONC")
  )

  with_mocked_bindings(
    {
      rounded <- do.call(anonymize_dataset, c(args, sigdig = 4))
      expect_equal(rounded$WT, signif(c(70.123456, 70.123456, 80.987654, 80.987654), 4))
      expect_equal(rounded$SCR, signif(c(1.0123456, 1.0123456, 0.87654321, 0.87654321), 4))
      expect_equal(rounded$DV[rounded$EVID == 0], signif(c(4.7654321, 8.1234567), 4))

      unrounded <- do.call(anonymize_dataset, c(args, list(sigdig = NULL)))
      expect_equal(unrounded$WT[1], 70.123456)
      expect_equal(unrounded$DV[unrounded$EVID == 0][1], 4.7654321)

      expect_error(do.call(anonymize_dataset, c(args, sigdig = 0)), "positive integer")
    },
    .package = "irxforge",
    sample_covariates_mice_timevarying = mock_bindings$sample_covariates_mice_timevarying,
    simulate_anonymized_concentrations = mock_bindings$simulate_anonymized_concentrations
  )
})

test_that("anonymize_dataset drops below-LLOQ observations", {
  dat <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 1, 2),
    EVID = c(1, 0, 0),
    AMT = c(100, 0, 0),
    DV = c(0, 5, 4),
    WT = c(70, 70, 70)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)

  local_mocked_bindings(
    sample_covariates_mice_timevarying = function(...) {
      data.frame(ID = c(1, 1, 1), .design_id = c(1, 1, 1), TIME = c(0, 1, 2), WT = 70)
    },
    simulate_anonymized_concentrations = function(...) {
      data.frame(ID = c(1, 1), TIME = c(1, 2), DV_SIM = c(0.5, 2))
    }
  )

  out <- anonymize_dataset(
    data = dat,
    covariates = "WT",
    model_file = model,
    lloq = 1,
    censoring = "drop"
  )

  expect_equal(out$TIME, c(0, 2))
  expect_equal(out$DV, c(0, 2))
})

test_that("anonymize_dataset flags below-LLOQ observations", {
  dat <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 1, 2),
    EVID = c(1, 0, 0),
    AMT = c(100, 0, 0),
    DV = c(0, 5, 4),
    WT = c(70, 70, 70)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)

  local_mocked_bindings(
    sample_covariates_mice_timevarying = function(...) {
      data.frame(ID = c(1, 1, 1), .design_id = c(1, 1, 1), TIME = c(0, 1, 2), WT = 70)
    },
    simulate_anonymized_concentrations = function(...) {
      data.frame(ID = c(1, 1), TIME = c(1, 2), DV_SIM = c(0.5, 2))
    }
  )

  out <- anonymize_dataset(
    data = dat,
    covariates = "WT",
    model_file = model,
    lloq = 1,
    censoring = "cens"
  )

  expect_equal(out$DV, c(0, 0.5, 2))
  expect_equal(out$CENS, c(0, 1, 0))
})

test_that("anonymize_dataset matches simulated DV by ID and TIME, not position", {
  dat <- data.frame(
    ID = c(1, 1, 1, 2, 2, 2),
    TIME = c(0, 1, 2, 0, 1, 2),
    EVID = c(1, 0, 0, 1, 0, 0),
    AMT = c(100, 0, 0, 100, 0, 0),
    DV = c(0, 5, 4, 0, 6, 7),
    WT = c(70, 70, 70, 80, 80, 80)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)

  local_mocked_bindings(
    sample_covariates_mice_timevarying = function(...) {
      data.frame(
        ID = c(1, 1, 1, 2, 2, 2),
        .design_id = c(1, 1, 1, 2, 2, 2),
        TIME = c(0, 1, 2, 0, 1, 2),
        WT = c(70, 70, 70, 80, 80, 80)
      )
    },
    # Return rows in a scrambled order: a positional copy would mis-assign.
    simulate_anonymized_concentrations = function(...) {
      data.frame(
        ID = c(2, 1, 2, 1),
        TIME = c(2, 2, 1, 1),
        DV_SIM = c(40, 20, 30, 10)
      )
    }
  )

  out <- anonymize_dataset(dat, covariates = "WT", model_file = model)

  # Each DV lands on its own ID/TIME regardless of sim row order.
  expect_equal(out$DV, c(0, 10, 20, 0, 30, 40))
})

test_that("anonymize_dataset tolerates NA simulated concentrations", {
  dat <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 1, 2),
    EVID = c(1, 0, 0),
    AMT = c(100, 0, 0),
    DV = c(0, 5, 4),
    WT = c(70, 70, 70)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)

  local_mocked_bindings(
    sample_covariates_mice_timevarying = function(...) {
      data.frame(ID = c(1, 1, 1), .design_id = c(1, 1, 1), TIME = c(0, 1, 2), WT = 70)
    },
    simulate_anonymized_concentrations = function(...) {
      data.frame(ID = c(1, 1), TIME = c(1, 2), DV_SIM = c(NA_real_, 2))
    }
  )

  # NA DV must not crash the cens path nor inject all-NA rows on drop.
  cens <- anonymize_dataset(dat, covariates = "WT", model_file = model, lloq = 1, censoring = "cens")
  expect_equal(cens$CENS, c(0, 0, 0))
  drop <- anonymize_dataset(dat, covariates = "WT", model_file = model, lloq = 1, censoring = "drop")
  expect_equal(nrow(drop), 3)
})

test_that("anonymize_dataset validates inputs", {
  dat <- data.frame(ID = 1, TIME = 0, EVID = 1, AMT = 100, DV = 0, WT = 70)
  model <- tempfile(fileext = ".ferx")
  file.create(model)

  expect_error(
    anonymize_dataset(dat, covariates = "ID", model_file = model),
    "cannot include mapped NONMEM"
  )
  expect_error(
    anonymize_dataset(dat, covariates = "WT", model_file = "missing.ferx"),
    "existing FeRx model"
  )
})
