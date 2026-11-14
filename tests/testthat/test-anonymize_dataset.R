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
  expect_equal(
    attr(out, "design_map"),
    data.frame(ID = c(1, 2), DESIGN_ID = c("B", "A"))
  )
})

test_that("n_candidates selects the covariate sample closest to observed data", {
  dat <- data.frame(
    SUBJ = c("A", "A", "A", "B", "B", "B"),
    TAD = c(0, 1, 2, 0, 1, 2),
    EVENT = c(1, 0, 0, 1, 0, 0),
    DOSE = c(100, 0, 0, 200, 0, 0),
    CONC = c(0, 4, 3, 0, 8, 6),
    WT = c(70, 70, 71, 80, 80, 81),
    SCR = c(1.0, 1.1, 1.1, 0.8, 0.8, 0.9)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)

  far <- data.frame(
    SUBJ = c(1, 1, 1, 2, 2, 2),
    .design_id = c("A", "A", "A", "B", "B", "B"),
    TAD = c(0, 1, 2, 0, 1, 2),
    WT = rep(120, 6),
    SCR = rep(3, 6)
  )
  near <- data.frame(
    SUBJ = c(1, 1, 1, 2, 2, 2),
    .design_id = c("A", "A", "A", "B", "B", "B"),
    TAD = c(0, 1, 2, 0, 1, 2),
    WT = c(70, 70, 71, 80, 80, 81), # identical to observed
    SCR = c(1.0, 1.1, 1.1, 0.8, 0.8, 0.9)
  )
  call_idx <- 0

  local_mocked_bindings(
    sample_covariates_mice_timevarying = function(...) {
      call_idx <<- call_idx + 1
      if (call_idx == 1) far else near
    },
    simulate_anonymized_concentrations = function(model_file, data, seed = NULL) {
      data.frame(ID = c(1, 1, 2, 2), TIME = c(1, 2, 1, 2), DV_SIM = c(7, 8, 3, 4))
    }
  )

  out <- anonymize_dataset(
    data = dat,
    covariates = c("WT", "SCR"),
    model_file = model,
    dictionary = list(ID = "SUBJ", TIME = "TAD", EVID = "EVENT",
                      AMT = "DOSE", DV = "CONC"),
    n_candidates = 2,
    score_on = "covariate",
    seed = 1
  )

  expect_equal(call_idx, 2) # both candidates were drawn
  expect_equal(out$WT, c(70, 70, 71, 80, 80, 81)) # the near candidate was picked
  expect_equal(attr(out, "similarity_score"), 0, tolerance = 1e-8)
})

test_that("score_on = 'concentration' picks the candidate with the closest sim conc", {
  dat <- data.frame(
    SUBJ = c("A", "A", "A", "B", "B", "B"),
    TAD = c(0, 1, 2, 0, 1, 2),
    EVENT = c(1, 0, 0, 1, 0, 0),
    DOSE = c(100, 0, 0, 200, 0, 0),
    CONC = c(0, 10, 8, 0, 12, 9), # observed concentrations
    WT = c(70, 70, 71, 80, 80, 81)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)

  candidate <- data.frame(
    SUBJ = c(1, 1, 1, 2, 2, 2),
    .design_id = c("A", "A", "A", "B", "B", "B"),
    TAD = c(0, 1, 2, 0, 1, 2),
    WT = c(72, 72, 73, 78, 78, 79)
  )
  sim_idx <- 0

  local_mocked_bindings(
    sample_covariates_mice_timevarying = function(...) candidate,
    simulate_anonymized_concentrations = function(model_file, data, seed = NULL) {
      sim_idx <<- sim_idx + 1
      dv <- if (sim_idx == 1) c(100, 100, 100, 100) else c(10, 8, 12, 9)
      data.frame(ID = c(1, 1, 2, 2), TIME = c(1, 2, 1, 2), DV_SIM = dv)
    }
  )

  out <- anonymize_dataset(
    data = dat,
    covariates = "WT",
    model_file = model,
    dictionary = list(ID = "SUBJ", TIME = "TAD", EVID = "EVENT",
                      AMT = "DOSE", DV = "CONC"),
    n_candidates = 2,
    score_on = "concentration",
    seed = 1
  )

  expect_equal(sim_idx, 2) # every candidate was simulated
  # the second candidate's concentrations match the observed data, so it wins
  expect_equal(out$DV[out$EVID == 0], c(10, 8, 12, 9))
  expect_equal(attr(out, "similarity_score"), 0, tolerance = 1e-8)
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
    loq = 1,
    blq_method = "remove"
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
    loq = 1,
    blq_method = "cens"
  )

  expect_equal(out$DV, c(0, 0, 2)) # below-LOQ value set to 0
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

  # NA DV must not crash the cens path nor inject all-NA rows on remove.
  cens <- anonymize_dataset(dat, covariates = "WT", model_file = model, loq = 1, blq_method = "cens")
  expect_equal(cens$CENS, c(0, 0, 0))
  removed <- anonymize_dataset(dat, covariates = "WT", model_file = model, loq = 1, blq_method = "remove")
  expect_equal(nrow(removed), 3)
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

test_that("non-mice methods reject categorical covariates", {
  dat <- data.frame(
    ID = c(1, 1, 2, 2),
    TIME = c(0, 1, 0, 1),
    EVID = c(1, 0, 1, 0),
    AMT = c(100, 0, 200, 0),
    DV = c(0, 5, 0, 8),
    SEX = c("M", "M", "F", "F")
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)
  expect_error(
    anonymize_dataset(dat, covariates = "SEX", model_file = model, method = "lme"),
    "continuous"
  )
  expect_error(
    anonymize_dataset(dat, covariates = "SEX", model_file = model, method = "copulas"),
    "continuous"
  )
})

test_that("method routes to the requested time-varying sampler", {
  dat <- data.frame(
    ID = c(1, 1, 1, 2, 2, 2),
    TIME = c(0, 1, 2, 0, 1, 2),
    EVID = c(1, 0, 0, 1, 0, 0),
    AMT = c(100, 0, 0, 200, 0, 0),
    DV = c(0, 5, 4, 0, 8, 6),
    WT = c(70, 70, 71, 80, 80, 81)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)
  candidate <- data.frame(
    ID = c(1, 1, 1, 2, 2, 2),
    .design_id = c("1", "1", "1", "2", "2", "2"),
    TIME = c(0, 1, 2, 0, 1, 2),
    WT = c(72, 72, 73, 78, 78, 79)
  )
  called <- character()
  local_mocked_bindings(
    sample_covariates_lme_timevarying = function(...) {
      called <<- c(called, "lme")
      candidate
    },
    simulate_anonymized_concentrations = function(model_file, data, seed = NULL) {
      data.frame(ID = c(1, 1, 2, 2), TIME = c(1, 2, 1, 2), DV_SIM = c(5, 4, 8, 6))
    }
  )
  out <- anonymize_dataset(
    dat, covariates = "WT", model_file = model, method = "lme", seed = 1
  )
  expect_equal(called, "lme")
  expect_equal(out$WT, c(72, 72, 73, 78, 78, 79))
})

test_that("simulated DV is matched by CMT for multiple compartments at same ID/TIME", {
  data <- data.frame(
    ID = c(1, 1, 1, 1, 1),
    TIME = c(0, 1, 1, 2, 2),
    EVID = c(1, 0, 0, 0, 0),
    CMT = c(1, 2, 3, 2, 3),
    DV = c(0, NA, NA, NA, NA)
  )
  # FeRx output in a different order than the input
  sim <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(2, 1, 2, 1),
    CMT = c(3, 3, 2, 2),
    DV_SIM = c(23, 13, 22, 12)
  )
  out <- apply_simulated_concentrations(data, sim)
  expect_equal(out$DV, c(0, 12, 13, 22, 23))
})

test_that("blank CMT on observation records is dropped from the match key", {
  data <- data.frame(
    ID = c(1, 1, 1, 2, 2),
    TIME = c(0, 1, 2, 0, 1),
    EVID = c(1, 0, 0, 1, 0),
    CMT = c(1, NA, NA, 1, NA),
    DV = c(0, NA, NA, 0, NA)
  )
  # FeRx output in a different order, with CMT blank or filled
  sim <- data.frame(ID = c(2, 1, 1), TIME = c(1, 2, 1), CMT = NA, DV_SIM = c(8, 6, 5))
  out <- apply_simulated_concentrations(data, sim)
  expect_equal(out$DV, c(0, 5, 6, 0, 8))

  sim$CMT <- 1
  out <- apply_simulated_concentrations(data, sim)
  expect_equal(out$DV, c(0, 5, 6, 0, 8))
})

test_that("blank CMT does not disable compartment matching for other records", {
  data <- data.frame(
    ID = c(1, 1, 1, 1, 1, 1),
    TIME = c(0, 1, 1, 2, 2, 3),
    EVID = c(1, 0, 0, 0, 0, 0),
    CMT = c(1, 2, 3, 2, 3, NA),
    DV = c(0, NA, NA, NA, NA, NA)
  )
  # CMT 2/3 rows at shared ID/TIME come back in reverse order
  sim <- data.frame(
    ID = c(1, 1, 1, 1, 1),
    TIME = c(3, 2, 2, 1, 1),
    CMT = c(NA, 3, 2, 3, 2),
    DV_SIM = c(31, 23, 22, 13, 12)
  )
  out <- apply_simulated_concentrations(data, sim)
  expect_equal(out$DV, c(0, 12, 13, 22, 23, 31))

  # FeRx fills the blank CMT with compartment 1
  sim$CMT[1] <- 1
  out <- apply_simulated_concentrations(data, sim)
  expect_equal(out$DV, c(0, 12, 13, 22, 23, 31))

  # FeRx assigns the blank record a different default compartment. Dropping
  # CMT would match the shared ID/TIME rows by occurrence only and swap them,
  # so an error is raised instead of a silent misassignment.
  sim$CMT[1] <- 2
  expect_error(
    apply_simulated_concentrations(data, sim),
    "could not be uniquely matched to observation records by ID, TIME, CMT"
  )
})

test_that("blank CMT falls back to ID/TIME when FeRx uses another default compartment", {
  data <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 1, 2),
    EVID = c(1, 0, 0),
    CMT = c(1, NA, NA),
    DV = c(0, NA, NA)
  )
  sim <- data.frame(ID = c(1, 1), TIME = c(2, 1), CMT = c(2, 2), DV_SIM = c(6, 5))
  out <- apply_simulated_concentrations(data, sim)
  expect_equal(out$DV, c(0, 5, 6))
})

test_that("records sharing ID/TIME without CMT are matched in order of occurrence", {
  data <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 1, 1),
    EVID = c(1, 0, 0),
    DV = c(0, NA, NA)
  )
  sim <- data.frame(ID = c(1, 1), TIME = c(1, 1), DV_SIM = c(5, 6))
  out <- apply_simulated_concentrations(data, sim)
  expect_equal(out$DV, c(0, 5, 6))
})

test_that("EVID == 0 records flagged MDV == 1 are skipped, matching FeRx", {
  data <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(0, 1, 2, 3),
    EVID = c(1, 0, 0, 0),
    MDV = c(1, 0, 1, 0),
    DV = c(0, 0, 0, 0)
  )
  # FeRx returns no row for the MDV == 1 sample at TIME 2
  sim <- data.frame(ID = c(1, 1), TIME = c(1, 3), DV_SIM = c(5, 7))
  out <- apply_simulated_concentrations(data, sim)
  expect_equal(out$DV, c(0, 5, NA, 7))
  expect_equal(recompute_anonymized_mdv(out)$MDV, c(1L, 0L, 1L, 0L))

  # Positional path (no ID/TIME in FeRx output)
  out_pos <- apply_simulated_concentrations(data, sim["DV_SIM"])
  expect_equal(out_pos$DV, c(0, 5, NA, 7))
})

test_that("FeRx rows for every EVID == 0 record are still accepted", {
  data <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 1, 2),
    EVID = c(1, 0, 0),
    MDV = c(1, 1, 0),
    DV = c(0, 0, 0)
  )
  sim <- data.frame(ID = c(1, 1), TIME = c(1, 2), DV_SIM = c(5, 7))
  out <- apply_simulated_concentrations(data, sim)
  expect_equal(out$DV, c(0, 5, 7))
})

test_that("row-count mismatch error names the MDV == 1 records", {
  data <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(0, 1, 2, 3),
    EVID = c(1, 0, 0, 0),
    MDV = c(1, 0, 1, 0),
    DV = c(0, 0, 0, 0)
  )
  sim <- data.frame(ID = 1, TIME = 1, DV_SIM = 5)
  expect_error(
    apply_simulated_concentrations(data, sim),
    "FeRx returned 1 rows for 3 `EVID == 0` records\\. 1 of them are flagged `MDV == 1`"
  )
  data$MDV <- NULL
  expect_error(
    apply_simulated_concentrations(data, sim),
    "FeRx returned 1 rows for 3 `EVID == 0` records; expected one simulated row"
  )
})

test_that("score_on = 'concentration' errors without positive observed concentrations", {
  dat <- data.frame(
    SUBJ = c("A", "A", "B", "B"),
    TAD = c(0, 1, 0, 1),
    EVENT = c(1, 0, 1, 0),
    DOSE = c(100, 0, 200, 0),
    CONC = c(0, 0, 0, 0),
    WT = c(70, 70, 80, 80)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)
  expect_error(
    anonymize_dataset(
      data = dat,
      covariates = "WT",
      model_file = model,
      dictionary = list(ID = "SUBJ", TIME = "TAD", EVID = "EVENT",
                        AMT = "DOSE", DV = "CONC"),
      n_candidates = 2,
      score_on = "concentration",
      seed = 1
    ),
    "positive observed concentrations"
  )
})

test_that("score_on = 'concentration' falls back to first candidate if none can be scored", {
  dat <- data.frame(
    SUBJ = c("A", "A", "A", "B", "B", "B"),
    TAD = c(0, 1, 2, 0, 1, 2),
    EVENT = c(1, 0, 0, 1, 0, 0),
    DOSE = c(100, 0, 0, 200, 0, 0),
    CONC = c(0, 10, 8, 0, 12, 9),
    WT = c(70, 70, 71, 80, 80, 81)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)
  candidate <- data.frame(
    SUBJ = c(1, 1, 1, 2, 2, 2),
    .design_id = c("A", "A", "A", "B", "B", "B"),
    TAD = c(0, 1, 2, 0, 1, 2),
    WT = c(72, 72, 73, 78, 78, 79)
  )
  local_mocked_bindings(
    sample_covariates_mice_timevarying = function(...) candidate,
    simulate_anonymized_concentrations = function(model_file, data, seed = NULL) {
      data.frame(ID = c(1, 1, 2, 2), TIME = c(1, 2, 1, 2), DV_SIM = c(0, 0, 0, 0))
    }
  )
  expect_warning(
    out <- anonymize_dataset(
      data = dat,
      covariates = "WT",
      model_file = model,
      dictionary = list(ID = "SUBJ", TIME = "TAD", EVID = "EVENT",
                        AMT = "DOSE", DV = "CONC"),
      n_candidates = 2,
      score_on = "concentration",
      seed = 1
    ),
    "first candidate"
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(dat))
})

test_that("MDV is recomputed from the simulated concentrations", {
  dat <- data.frame(
    ID = c(1, 1, 1, 1),
    TIME = c(0, 1, 2, 3),
    EVID = c(1, 0, 0, 0),
    AMT = c(100, 0, 0, 0),
    DV = c(0, 5, 0, 4),
    MDV = c(1, 0, 1, 0), # original BLQ observation at TIME 2 has MDV = 1
    WT = c(70, 70, 70, 70)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)

  local_mocked_bindings(
    sample_covariates_mice_timevarying = function(...) {
      data.frame(ID = 1, .design_id = 1, TIME = c(0, 1, 2, 3), WT = 70)
    },
    simulate_anonymized_concentrations = function(...) {
      data.frame(ID = c(1, 1, 1), TIME = c(1, 2, 3), DV_SIM = c(NA_real_, 3, 0.5))
    }
  )

  out <- anonymize_dataset(dat, covariates = "WT", model_file = model)
  # dose -> 1, missing simulated DV -> 1, positive simulated DV -> 0
  expect_equal(out$MDV, c(1L, 1L, 0L, 0L))

  cens <- anonymize_dataset(
    dat, covariates = "WT", model_file = model, loq = 1, blq_method = "cens"
  )
  expect_equal(cens$CENS, c(0L, 0L, 0L, 1L))
  expect_equal(cens$MDV, c(1L, 1L, 0L, 0L))

  removed <- anonymize_dataset(
    dat, covariates = "WT", model_file = model, loq = 1, blq_method = "remove"
  )
  expect_equal(removed$MDV, c(1L, 1L, 0L))
})

test_that("anonymize_dataset rejects time axes that restart, and replicates", {
  dat <- data.frame(
    ID = c(1, 1, 1, 1), TIME = c(0, 1, 0, 1), EVID = c(1, 0, 4, 0),
    AMT = c(100, 0, 100, 0), DV = c(0, 5, 0, 4), WT = 70
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)
  expect_error(
    anonymize_dataset(dat, covariates = "WT", model_file = model),
    "decreases within 1 subject\\(s\\): 1\\..*time-reset records"
  )
  dat$EVID[3] <- 1
  dat$TIME <- c(0, 1, 2, 3)
  expect_error(
    anonymize_dataset(dat, covariates = "WT", model_file = model, replicates = 2),
    "`replicates` is not supported"
  )
  # `replicates` forwarded through a wrapper's `...` is still evaluated in the
  # caller's environment
  wrapper <- function(...) {
    reps <- 2
    anonymize_dataset(dat, covariates = "WT", model_file = model, ...)
  }
  n_reps <- 3
  expect_error(wrapper(replicates = n_reps), "`replicates` is not supported")
})

test_that("covariate features ignore unused factor ID levels", {
  dat <- data.frame(
    ID = factor(c("b", "b", "c", "c"), levels = c("a", "b", "c")),
    TIME = c(0, 1, 0, 1),
    WT = c(70, 71, 80, 81)
  )
  feats <- build_covariate_features(
    data = dat, id_var = "ID", time_var = "TIME",
    covariates = "WT", cat_covs = character(0), cat_levels = list()
  )
  expect_equal(nrow(feats), 2)
})

test_that("cat_covs marks numerically coded covariates as categorical", {
  dat <- data.frame(
    ID = c(1, 1, 2, 2), TIME = c(0, 1, 0, 1), EVID = c(1, 0, 1, 0),
    AMT = c(100, 0, 100, 0), DV = c(0, 5, 0, 4), SEX = c(0, 0, 1, 1),
    WT = c(70, 70, 80, 80)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)
  expect_error(
    anonymize_dataset(dat, covariates = c("WT", "SEX"), model_file = model,
                      method = "lme", cat_covs = "SEX"),
    "supports only continuous"
  )
  expect_error(
    anonymize_dataset(dat, covariates = "WT", model_file = model, cat_covs = "SEX"),
    "subset of `covariates`"
  )

  passed_cat_covs <- NULL
  local_mocked_bindings(
    sample_covariates_mice_timevarying = function(data, cat_covs, ...) {
      passed_cat_covs <<- cat_covs
      data.frame(
        ID = c(1, 1, 2, 2), .design_id = c(1, 1, 2, 2), TIME = c(0, 1, 0, 1),
        WT = c(71, 71, 79, 79), SEX = factor(c("0", "0", "1", "1"))
      )
    },
    simulate_anonymized_concentrations = function(...) {
      data.frame(ID = c(1, 2), TIME = c(1, 1), DV_SIM = c(5, 4))
    }
  )
  out <- anonymize_dataset(
    dat, covariates = c("WT", "SEX"), model_file = model, cat_covs = "SEX"
  )
  expect_equal(passed_cat_covs, "SEX")
  expect_type(out$SEX, "double")
  expect_equal(out$SEX, c(0, 0, 1, 1))
})

test_that("anonymize_dataset accepts an empty covariate set", {
  dat <- data.frame(
    ID = c(11, 11, 11, 22, 22, 33, 33, 33),
    TIME = c(0, 1, 2, 0, 4, 0, 1, 3),
    DV = c(0, 4, 3, 0, 8, 0, 5, 2),
    EVID = c(1, 0, 0, 1, 0, 1, 0, 0),
    AMT = c(100, 0, 0, 200, 0, 300, 0, 0),
    CMT = c(1, 2, 2, 1, 2, 1, 2, 2),
    RATE = 0,
    MDV = c(1, 0, 0, 1, 0, 1, 0, 0)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)

  local_mocked_bindings(
    sample_covariates_mice_timevarying = function(...) {
      stop("covariate sampler must not be called without covariates")
    },
    simulate_anonymized_concentrations = function(model_file, data, seed = NULL) {
      obs <- data[data$EVID == 0, ]
      data.frame(ID = obs$ID, TIME = obs$TIME, CMT = obs$CMT, DV_SIM = obs$TIME * 10)
    }
  )

  out <- anonymize_dataset(dat, covariates = character(0), model_file = model, seed = 3)

  expect_named(out, c("ID", "TIME", "EVID", "AMT", "RATE", "DV", "CMT", "MDV"))
  expect_setequal(unique(out$ID), 1:3)
  expect_equal(nrow(out), nrow(dat))
  # every observed design is cloned exactly once onto a renumbered subject
  designs <- vapply(split(out, out$ID), function(d) {
    paste(d$TIME, d$AMT, collapse = ";")
  }, character(1))
  orig <- vapply(split(dat, dat$ID), function(d) {
    paste(d$TIME, d$AMT, collapse = ";")
  }, character(1))
  expect_setequal(unname(designs), unname(orig))
  # the design map relates each anonymized subject to its source design
  design_map <- attr(out, "design_map")
  expect_named(design_map, c("ID", "DESIGN_ID"))
  expect_setequal(design_map$ID, 1:3)
  expect_setequal(design_map$DESIGN_ID, c(11, 22, 33))
  expect_equal(
    unname(designs[as.character(design_map$ID)]),
    unname(orig[as.character(design_map$DESIGN_ID)])
  )
  expect_equal(out$DV[out$EVID == 0], out$TIME[out$EVID == 0] * 10)
  expect_equal(out$DV[out$EVID == 1], rep(0, 3))
  # reproducible with a seed
  expect_equal(
    anonymize_dataset(dat, covariates = character(0), model_file = model, seed = 3),
    out
  )

  # concentration scoring still works; covariate scoring has nothing to score
  scored <- anonymize_dataset(
    dat, covariates = character(0), model_file = model,
    n_candidates = 2, seed = 3
  )
  expect_true(is.finite(attr(scored, "similarity_score")))
  expect_error(
    anonymize_dataset(
      dat, covariates = character(0), model_file = model,
      n_candidates = 2, score_on = "covariate"
    ),
    "requires at least one covariate"
  )
  expect_error(
    anonymize_dataset(dat, covariates = NULL, model_file = model),
    "must be a character vector"
  )
})

test_that("monotone time check reports offending subjects", {
  dat <- data.frame(
    ID = c(1, 1, 1, 2, 2, 2, 3, 3),
    TIME = c(0, 1, 2, 0, 5, 3, 0, 1),
    EVID = c(1, 0, 0, 1, 0, 0, 1, 0)
  )
  expect_error(
    check_anonymize_monotone_time(dat, "ID", "TIME", "EVID"),
    "decreases within 1 subject\\(s\\): 2\\.$"
  )
  # no EVID 3/4 -> no reset hint
  err <- tryCatch(
    check_anonymize_monotone_time(dat, "ID", "TIME", "EVID"),
    error = conditionMessage
  )
  expect_false(grepl("EVID 3 or 4", err))

  # ties and missing times are allowed
  ok <- data.frame(
    ID = c(1, 1, 1, 1), TIME = c(0, 0, NA, 2), EVID = c(1, 0, 2, 0)
  )
  expect_true(check_anonymize_monotone_time(ok, "ID", "TIME", "EVID"))

  # many offending subjects are truncated
  many <- data.frame(ID = rep(1:12, each = 2), TIME = rep(c(1, 0), 12), EVID = 0)
  expect_error(
    check_anonymize_monotone_time(many, "ID", "TIME", "EVID"),
    "12 subject\\(s\\): 1, 2, .*, 10 \\(and 2 more\\)"
  )
})

test_that("anonymize_dataset accepts reset records with a monotone time axis", {
  dat <- data.frame(
    ID = c(1, 1, 1, 1, 2, 2, 2, 2),
    TIME = c(0, 1, 24, 25, 0, 1, 24, 25),
    EVID = c(1, 0, 4, 0, 1, 0, 3, 0),
    AMT = c(100, 0, 100, 0, 100, 0, 0, 0),
    DV = c(0, 5, 0, 4, 0, 6, 0, 5),
    WT = c(70, 70, 70, 70, 80, 80, 80, 80)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)
  local_mocked_bindings(
    simulate_anonymized_concentrations = function(model_file, data, seed = NULL) {
      data.frame(
        ID = data$ID[data$EVID == 0],
        TIME = data$TIME[data$EVID == 0],
        DV_SIM = 3
      )
    }
  )
  out <- anonymize_dataset(dat, covariates = "WT", model_file = model, seed = 1)
  expect_true(any(out$EVID %in% c(3, 4)))
  expect_equal(nrow(out), nrow(dat))
})

test_that("cloned designs are time-ordered and exposed via the design map", {
  # subject 7 has tied times recorded out of EVID order and a missing time;
  # the clone keeps tied records in source order and puts NA times last
  dat <- data.frame(
    ID = c(7, 7, 7, 7, 9, 9, 9),
    TIME = c(0, NA, 0, 2, 0, 1, 3),
    EVID = c(0, 2, 1, 0, 1, 0, 0),
    AMT = c(0, 0, 100, 0, 50, 0, 0),
    DV = c(0, 0, 0, 5, 0, 4, 2),
    WT = c(70, 70, 70, 71, 60, 60, 61)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)

  local_mocked_bindings(
    sample_covariates_mice_timevarying = function(...) {
      data.frame(
        ID = c(1, 1, 1, 2, 2, 2, 2),
        .design_id = c("9", "9", "9", "7", "7", "7", "7"),
        TIME = c(0, 1, 3, 0, 0, 2, NA),
        WT = c(65, 65, 66, 72, 72, 73, 73)
      )
    },
    simulate_anonymized_concentrations = function(model_file, data, seed = NULL) {
      obs <- data[data$EVID == 0, ]
      data.frame(ID = obs$ID, TIME = obs$TIME, DV_SIM = seq_len(nrow(obs)))
    }
  )

  out <- anonymize_dataset(dat, covariates = "WT", model_file = model, seed = 1)

  expect_equal(
    attr(out, "design_map"),
    data.frame(ID = c(1, 2), DESIGN_ID = c(9, 7))
  )
  subj2 <- out[out$ID == 2, ]
  expect_equal(subj2$TIME, c(0, 0, 2, NA))
  expect_equal(subj2$EVID, c(0, 1, 0, 2))
})

test_that("prepare_simulation_model_file adapts a fitting model to the anonymized data", {
  model <- tempfile(fileext = ".ferx")
  writeLines(c(
    "# fitting model",
    "[parameters]",
    "  theta TVCL(0.1, 0.001, 10)",
    "",
    "[data]",
    "  path = ../data/original.csv  # relative to the model",
    "  AMT = DOSE",
    "  ODV = dv",
    "  DV = lndv",
    "  TIME = tafd",
    "  WT = weight",
    "  CRCL = crcl_raw",
    "  SCR = WT",
    "",
    "[initial_values]",
    "  CL = 0.1",
    "",
    "[data_selection]",
    "  ignore = FLAG == 1",
    "",
    "[fit_options]",
    "  method = foce"
  ), model)
  data_path <- tempfile(fileext = ".csv")
  out <- prepare_simulation_model_file(
    model_file = model,
    data_path = data_path,
    columns = c("ID", "TIME", "EVID", "AMT", "DV", "weight", "SCR")
  )
  lines <- readLines(out)

  expect_false(any(grepl("data_selection|ignore = FLAG", lines)))
  expect_false(any(grepl("initial_values|CL = 0.1", lines)))
  abs_path <- normalizePath(data_path, winslash = "/", mustWork = FALSE)
  expect_true(paste0("  path = \"", abs_path, "\"") %in% lines)
  expect_false(any(grepl("original.csv", lines)))
  # structural maps and maps from dropped columns are removed
  expect_false(any(grepl("AMT = DOSE|ODV = dv|DV = lndv|TIME = tafd|crcl_raw", lines)))
  # a map from a surviving covariate is kept; one colliding with a column is not
  expect_true("  WT = weight" %in% lines)
  expect_false("  SCR = WT" %in% lines)
  # unrelated content is copied verbatim
  expect_true(all(c(
    "# fitting model", "[parameters]", "  theta TVCL(0.1, 0.001, 10)",
    "[data]", "[fit_options]", "  method = foce"
  ) %in% lines))
})

test_that("prepare_simulation_model_file leaves models without [data] untouched", {
  model <- tempfile(fileext = ".ferx")
  src <- c("[parameters]", "  theta TVCL(0.1, 0.001, 10)", "[fit_options]", "  method = foce")
  writeLines(src, model)
  out <- prepare_simulation_model_file(model, tempfile(fileext = ".csv"), c("ID", "TIME", "DV"))
  expect_equal(readLines(out), src)
})

test_that("prepare_simulation_model_file adds a path to a [data] block without one", {
  model <- tempfile(fileext = ".ferx")
  writeLines(c("[data]", "  DV = CONC", "[parameters]"), model)
  data_path <- tempfile(fileext = ".csv")
  out <- prepare_simulation_model_file(model, data_path, c("ID", "TIME", "DV"))
  abs_path <- normalizePath(data_path, winslash = "/", mustWork = FALSE)
  expect_equal(
    readLines(out),
    c("[data]", paste0("  path = \"", abs_path, "\""), "[parameters]")
  )
})

test_that("simulate_anonymized_concentrations passes a simulation-ready model copy to ferx", {
  skip_if_not_installed("ferx")
  model <- tempfile(fileext = ".ferx")
  writeLines(c("[data]", "  path = orig.csv", "  AMT = DOSE", "[data_selection]", "  ignore = X == 1"), model)
  captured <- NULL
  local_mocked_bindings(
    ferx_simulate = function(model, data, n_sim, seed = NULL) {
      captured <<- list(model = model, model_lines = readLines(model), data = data)
      data.frame(ID = 1, TIME = 1, DV_SIM = 1)
    },
    .package = "ferx"
  )
  dat <- data.frame(ID = 1, TIME = c(0, 1), EVID = c(1, 0), AMT = c(100, 0), DV = 0)
  simulate_anonymized_concentrations(model, dat, seed = 1)
  expect_false(identical(captured$model, model))
  expect_length(captured$model_lines, 2)
  expect_equal(captured$model_lines[1], "[data]")
  expect_match(captured$model_lines[2], basename(captured$data), fixed = TRUE)
  # temporary files are cleaned up
  expect_false(file.exists(captured$model))
})

test_that("prepare_simulation_model_file keeps simultaneous/swapped covariate renames", {
  model <- tempfile(fileext = ".ferx")
  writeLines(c("[data]", "  path = orig.csv", "  HTCM = WT", "  WT = HT"), model)
  out <- prepare_simulation_model_file(
    model, tempfile(fileext = ".csv"), c("ID", "TIME", "DV", "HT", "WT")
  )
  expect_true(all(c("  HTCM = WT", "  WT = HT") %in% readLines(out)))

  # a full swap is kept as well
  writeLines(c("[data]", "  path = orig.csv", "  WT = HT", "  HT = WT"), model)
  out <- prepare_simulation_model_file(
    model, tempfile(fileext = ".csv"), c("ID", "TIME", "DV", "HT", "WT")
  )
  expect_true(all(c("  WT = HT", "  HT = WT") %in% readLines(out)))
})

test_that("retain_data_maps drops renames that collide after the others are applied", {
  # WT = HT collides once HTCM = WT is dropped (its source is absent: dropped
  # earlier), so only the unrelated rename survives.
  expect_equal(
    retain_data_maps(c("WT", "BMI2"), c("HT", "BMI"), c("ID", "HT", "WT", "BMI")),
    c(FALSE, TRUE)
  )
  # dropping one rename un-vacates its source and cascades
  expect_equal(
    retain_data_maps(c("A", "B"), c("B", "C"), c("A", "B", "C")),
    c(FALSE, FALSE)
  )
  expect_equal(
    retain_data_maps(c("A", "B", "Z"), c("B", "C", "A"), c("A", "B", "C")),
    c(TRUE, TRUE, TRUE)
  )
  # duplicate targets: first one wins
  expect_equal(
    retain_data_maps(c("X", "X"), c("A", "B"), c("A", "B")),
    c(TRUE, FALSE)
  )
})

test_that("covariates without observed values are rejected up front", {
  model <- tempfile(fileext = ".ferx")
  file.create(model)
  dat <- data.frame(
    ID = c(1, 1, 2, 2),
    TIME = c(0, 1, 0, 1),
    EVID = c(1, 0, 1, 0),
    AMT = c(100, 0, 100, 0),
    DV = c(0, 5, 0, 6),
    WT = c(70, 70, 80, 80),
    HT = c(NA, 170, NA, 180),
    AST = c(NA, 20, NA, NA),
    ALT = NA_real_,
    CREA = NA_real_
  )
  local_mocked_bindings(
    sample_covariates_mice_timevarying = function(...) {
      stop("sampler should not be reached")
    }
  )
  err <- tryCatch(
    anonymize_dataset(
      dat, covariates = c("WT", "HT", "AST", "ALT", "CREA"), model_file = model
    ),
    error = conditionMessage
  )
  expect_match(err, "no observed values: ALT, CREA", fixed = TRUE)
  expect_match(
    err,
    "no observed values at baseline (first record per subject): HT, AST",
    fixed = TRUE
  )
  expect_false(grepl("sampler should not be reached", err))

  # only baseline-missing covariates: message does not claim no values overall
  err <- tryCatch(
    anonymize_dataset(dat, covariates = c("WT", "HT"), model_file = model),
    error = conditionMessage
  )
  expect_match(err, "at baseline")
  expect_false(grepl("no observed values: ", err, fixed = TRUE))
})

test_that("baseline rows are the first record per subject ordered by time", {
  # rows out of time order: the earliest record per subject is the baseline
  dat <- data.frame(
    ID = c(1, 1, 2, 2),
    TIME = c(1, 0, 1, 0),
    HT = c(NA, 170, NA, NA)
  )
  expect_true(check_anonymize_observed_covariates(dat, "HT", "ID", "TIME"))
  dat$HT <- c(170, NA, 180, NA)
  expect_error(
    check_anonymize_observed_covariates(dat, "HT", "ID", "TIME"),
    "no observed values at baseline"
  )
  expect_true(check_anonymize_observed_covariates(dat, character(0), "ID", "TIME"))
})

test_that("prepare_simulation_model_file handles `//` and `#` comments like FeRx", {
  model <- tempfile(fileext = ".ferx")
  writeLines(c(
    "[data] // source dataset",
    "  path = orig.csv // relative",
    "  WT = weight // body weight",
    "  AMT = DOSE # dose column",
    "[data_selection]   // fitting-only filter",
    "  ignore = FLAG == 1",
    "[fit_options] # options",
    "  method = foce // estimation"
  ), model)
  data_path <- tempfile(fileext = ".csv")
  out <- prepare_simulation_model_file(
    model, data_path, c("ID", "TIME", "EVID", "AMT", "DV", "weight")
  )
  abs_path <- normalizePath(data_path, winslash = "/", mustWork = FALSE)
  expect_equal(readLines(out), c(
    "[data] // source dataset",
    paste0("  path = \"", abs_path, "\""),
    "  WT = weight // body weight",
    "[fit_options] # options",
    "  method = foce // estimation"
  ))
})

test_that("prepare_simulation_model_file makes a relative [priors] from_fit absolute", {
  model_dir <- tempfile("model_dir")
  dir.create(file.path(model_dir, "fits"), recursive = TRUE)
  fit_file <- file.path(model_dir, "fits", "prev-fit.yaml")
  file.create(fit_file)
  model <- file.path(model_dir, "model.ferx")
  writeLines(c(
    "[priors]  // import",
    "  from_fit = \"fits/prev-fit.yaml\" # previous run",
    "[fit_options]",
    "  method = foce"
  ), model)
  out <- prepare_simulation_model_file(model, tempfile(fileext = ".csv"), c("ID", "TIME", "DV"))
  lines <- readLines(out)
  expect_equal(lines[1], "[priors]  // import")
  abs_fit <- normalizePath(fit_file, winslash = "/")
  expect_equal(lines[2], paste0("  from_fit = \"", abs_fit, "\""))
  expect_true(file.exists(sub("^\\s*from_fit = \"(.*)\"$", "\\1", lines[2])))

  # absolute references are left untouched
  writeLines(c("[priors]", paste0("  from_fit = \"", abs_fit, "\"")), model)
  out <- prepare_simulation_model_file(model, tempfile(fileext = ".csv"), c("ID", "TIME", "DV"))
  expect_equal(readLines(out), c("[priors]", paste0("  from_fit = \"", abs_fit, "\"")))
})

test_that("strip_ferx_comment and is_absolute_path follow FeRx/OS conventions", {
  expect_equal(
    strip_ferx_comment(c("a = b // c # d", "a = b # c // d", "a = b")),
    c("a = b ", "a = b ", "a = b")
  )
  expect_true(all(is_absolute_path(c("/x/y", "~/x", "C:/x", "C:\\x", "\\\\srv\\x"))))
  expect_false(any(is_absolute_path(c("x.yaml", "fits/x.yaml", "./x", "../x"))))
})

test_that("unknown `...` arguments are rejected up front as unused", {
  dat <- data.frame(
    ID = c(1, 1, 2, 2), TIME = c(0, 1, 0, 1), EVID = c(1, 0, 1, 0),
    AMT = c(100, 0, 100, 0), DV = c(0, 5, 0, 4), WT = c(70, 70, 80, 80)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)
  local_mocked_bindings(
    simulate_anonymized_concentrations = function(...) {
      stop("simulation should not be reached")
    }
  )
  expect_error(
    anonymize_dataset(dat, covariates = "WT", model_file = model, maxiter = 10),
    "unused argument `maxiter` in `anonymize_dataset\\(\\)`.*this build of irxforge"
  )
  expect_error(
    anonymize_dataset(
      dat, covariates = "WT", model_file = model, foo = 1, bar = 2
    ),
    "unused arguments `foo`, `bar`"
  )
  # names are checked before any `...` argument is evaluated
  expect_error(
    anonymize_dataset(
      dat, covariates = "WT", model_file = model, foo = stop("boom")
    ),
    "unused argument `foo`"
  )
  # `trend` belongs to the LME sampler, not to MICE
  expect_error(
    anonymize_dataset(
      dat, covariates = "WT", model_file = model, method = "mice", trend = "time"
    ),
    "unused argument `trend`.*method = \"mice\""
  )
  # arguments anonymize_dataset() sets itself cannot be passed again
  expect_error(
    anonymize_dataset(dat, covariates = "WT", model_file = model, n_subjects = 5),
    "Cannot pass `n_subjects`.*set by `anonymize_dataset\\(\\)` itself"
  )
  expect_error(
    anonymize_dataset(
      dat, covariates = "WT", model_file = model, method = "mice", m = 2
    ),
    "Cannot pass `m`"
  )
})

test_that("check_anonymize_dots accepts arguments of the selected sampler", {
  expect_silent(check_anonymize_dots(NULL, n_dots = 0, method = "mice"))
  # sampler formals and internal MICE arguments
  expect_silent(
    check_anonymize_dots(c("maxit", "noise", "replicates"), 3, method = "mice")
  )
  expect_silent(check_anonymize_dots(c("trend", "maxit"), 2, method = "lme"))
  expect_silent(check_anonymize_dots(c("degree", "bw_mult"), 2, method = "copulas"))
  # the copula sampler does not forward to MICE
  expect_error(
    check_anonymize_dots("maxit", 1, method = "copulas"),
    "unused argument `maxit`"
  )
  expect_error(
    check_anonymize_dots(c("maxit", ""), 2, method = "mice"),
    "must be named"
  )
  expect_error(check_anonymize_dots(NULL, 1, method = "mice"), "must be named")
})

test_that("anonymize_dataset forwards accepted `...` arguments to the sampler", {
  dat <- data.frame(
    ID = c(1, 1, 2, 2), TIME = c(0, 1, 0, 1), EVID = c(1, 0, 1, 0),
    AMT = c(100, 0, 100, 0), DV = c(0, 5, 0, 4), WT = c(70, 70, 80, 80)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)
  received <- NULL
  # validation reads the real sampler's formals, so wrap rather than replace
  # the signature: the mock keeps the formals of the real MICE sampler
  mock <- sample_covariates_mice_timevarying
  body(mock) <- quote({
    received <<- list(...)
    data.frame(
      ID = c(1, 1, 2, 2), .design_id = c(2, 2, 1, 1),
      TIME = c(0, 1, 0, 1), WT = c(81, 81, 71, 71)
    )
  })
  environment(mock) <- environment()
  local_mocked_bindings(
    sample_covariates_mice_timevarying = mock,
    simulate_anonymized_concentrations = function(model_file, data, seed = NULL) {
      obs <- data[data$EVID == 0, ]
      data.frame(ID = obs$ID, TIME = obs$TIME, DV_SIM = c(3, 4))
    }
  )
  anonymize_dataset(dat, covariates = "WT", model_file = model, maxit = 7)
  expect_equal(received, list(maxit = 7))
})

test_that("a cat_covs entry with implausibly many levels is rejected before sampling", {
  # e.g. a clock column (time of previous reset) passed in `cat_covs`
  dat <- data.frame(
    ID = rep(1:30, each = 2), TIME = rep(c(0, 1), 30),
    EVID = rep(c(1, 0), 30), AMT = rep(c(100, 0), 30),
    DV = rep(c(0, 5), 30),
    PRT = seq(0.5, 30, by = 0.5),
    WT = rep(seq(50, 108, by = 2), each = 2)
  )
  model <- tempfile(fileext = ".ferx")
  file.create(model)
  local_mocked_bindings(
    simulate_anonymized_concentrations = function(...) {
      stop("simulation should not be reached")
    }
  )
  expect_error(
    anonymize_dataset(
      dat, covariates = c("PRT", "WT"), model_file = model, cat_covs = "PRT"
    ),
    "`PRT` \\(60 levels\\)",
    class = "irxforge_cat_levels_error"
  )
  # `max_cat_levels` is accepted through `...`
  expect_no_error(
    check_anonymize_dots("max_cat_levels", 1, method = "mice")
  )
})
