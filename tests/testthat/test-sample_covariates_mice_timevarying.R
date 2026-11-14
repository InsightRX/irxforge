test_that("returns long-format rows for each subject and time", {
  dat <- data.frame(
    ID = rep(1:4, each = 3),
    TIME = rep(c(0, 1, 2), times = 4),
    SEX = rep(c("M", "F", "F", "M"), each = 3),
    WT = c(70, 71, 72, 60, 61, 62, 65, 66, 67, 80, 81, 82)
  )

  local_mocked_bindings(
    sample_covariates_mice = function(
      data,
      cat_covs = NULL,
      n_subjects = nrow(data),
      ...
    ) {
      data[seq_len(n_subjects), , drop = FALSE]
    },
    impute_mice_transition_step = function(
      transition_data,
      current,
      time,
      previous_time,
      time_varying_covs,
      ...
    ) {
      out <- current[time_varying_covs]
      out$WT <- out$WT + time - previous_time
      out
    }
  )

  out <- sample_covariates_mice_timevarying(
    data = dat,
    static_covs = "SEX",
    time_varying_covs = "WT",
    n_subjects = 2,
    time_grid = c(0, 1, 2)
  )

  expect_equal(nrow(out), 6)
  expect_named(out, c("ID", "TIME", "SEX", "WT"))
  expect_equal(out$ID, c(1, 2, 1, 2, 1, 2))
  expect_equal(out$TIME, c(0, 0, 1, 1, 2, 2))
  expect_equal(out$WT, c(70, 60, 71, 61, 72, 62))
})

test_that("input class is preserved", {
  dat <- tibble::tibble(
    ID = rep(1:3, each = 2),
    TIME = rep(c(0, 1), times = 3),
    WT = c(70, 71, 60, 61, 80, 81)
  )

  local_mocked_bindings(
    sample_covariates_mice = function(
      data,
      cat_covs = NULL,
      n_subjects = nrow(data),
      ...
    ) {
      data[seq_len(n_subjects), , drop = FALSE]
    },
    impute_mice_transition_step = function(
      current,
      time_varying_covs,
      ...
    ) {
      current[time_varying_covs]
    }
  )

  out <- sample_covariates_mice_timevarying(
    data = dat,
    time_varying_covs = "WT",
    n_subjects = 2
  )

  expect_s3_class(out, "tbl_df")
})

test_that("replicates include replicate index", {
  dat <- data.frame(
    ID = rep(1:3, each = 2),
    TIME = rep(c(0, 1), times = 3),
    WT = c(70, 71, 60, 61, 80, 81)
  )

  local_mocked_bindings(
    sample_covariates_mice = function(
      data,
      cat_covs = NULL,
      n_subjects = nrow(data),
      ...
    ) {
      data[seq_len(n_subjects), , drop = FALSE]
    },
    impute_mice_transition_step = function(
      current,
      time_varying_covs,
      ...
    ) {
      current[time_varying_covs]
    }
  )

  out <- sample_covariates_mice_timevarying(
    data = dat,
    time_varying_covs = "WT",
    n_subjects = 2,
    replicates = 2
  )

  expect_equal(nrow(out), 8)
  expect_equal(out$.replicate, rep(1:2, each = 4))
})

test_that("default time profiles clone observed subject profiles deterministically", {
  dat <- data.frame(
    ID = c(1, 1, 1, 2, 2, 3, 3, 3),
    TIME = c(0, 1, 3, 0, 2, 0, 0.5, 4),
    WT = c(70, 71, 73, 60, 62, 80, 81, 84)
  )

  local_mocked_bindings(
    sample_covariates_mice = function(
      data,
      cat_covs = NULL,
      n_subjects = nrow(data),
      ...
    ) {
      data[seq_len(n_subjects), , drop = FALSE]
    },
    impute_mice_transition_step = function(
      current,
      time,
      previous_time,
      time_varying_covs,
      ...
    ) {
      out <- current[time_varying_covs]
      out$WT <- out$WT + time - previous_time
      out
    }
  )

  cloned_profiles <- split(dat$TIME, dat$ID)[c(1, 2, 3, 1, 2)]

  out <- sample_covariates_mice_timevarying(
    data = dat,
    time_varying_covs = "WT",
    n_subjects = 5
  )

  out_profiles <- split(out$TIME, out$ID)
  expect_equal(
    unname(lapply(out_profiles, as.numeric)),
    unname(lapply(cloned_profiles, as.numeric))
  )
})

test_that("sampled observed time profiles preserve repeated times", {
  dat <- data.frame(
    ID = c(1, 1, 1, 2, 2, 2),
    TIME = c(0, 1, 1, 0, 2, 2),
    WT = c(70, 71, 71, 60, 62, 62)
  )

  local_mocked_bindings(
    sample_covariates_mice = function(
      data,
      cat_covs = NULL,
      n_subjects = nrow(data),
      ...
    ) {
      data[seq_len(n_subjects), , drop = FALSE]
    },
    impute_mice_transition_step = function(
      current,
      time_varying_covs,
      ...
    ) {
      current[time_varying_covs]
    }
  )

  out <- sample_covariates_mice_timevarying(
    data = dat,
    time_varying_covs = "WT",
    n_subjects = 2
  )

  expect_true(any(duplicated(out[c("ID", "TIME")])))
  expect_true(
    all(
      vapply(
        split(out$TIME, out$ID),
        function(x) paste(x, collapse = ",") %in% c("0,1,1", "0,2,2"),
        logical(1)
      )
    )
  )
})

test_that("propensity design matching uses matched observed subject profiles", {
  dat <- data.frame(
    ID = c(1, 1, 2, 2, 2, 3, 3, 3, 3),
    TIME = c(0, 1, 0, 2, 4, 0, 3, 6, 9),
    AGE = c(20, 20, 50, 50, 50, 80, 80, 80, 80),
    WT = c(70, 71, 75, 76, 77, 80, 81, 82, 83)
  )

  local_mocked_bindings(
    sample_covariates_mice = function(
      data,
      cat_covs = NULL,
      n_subjects = nrow(data),
      ...
    ) {
      data.frame(
        AGE = c(79, 21),
        WT = c(100, 100)
      )[seq_len(n_subjects), , drop = FALSE]
    },
    impute_mice_transition_step = function(
      current,
      time_varying_covs,
      ...
    ) {
      current[time_varying_covs]
    }
  )

  out <- sample_covariates_mice_timevarying(
    data = dat,
    static_covs = "AGE",
    time_varying_covs = "WT",
    design_match = "propensity",
    design_match_covs = "AGE",
    n_subjects = 2
  )

  expect_equal(as.numeric(out$TIME[out$ID == 1]), c(0, 3, 6, 9))
  expect_equal(as.numeric(out$TIME[out$ID == 2]), c(0, 1))
})

test_that("propensity design matching is without replacement", {
  match_idx <- match_mice_profiles_by_propensity(
    baseline_data = data.frame(AGE = c(20, 50, 80)),
    baseline = data.frame(AGE = c(79, 78)),
    design_match_covs = "AGE",
    cat_covs = NULL
  )

  expect_length(match_idx, 2)
  expect_equal(length(unique(match_idx)), 2)
  expect_true(3 %in% match_idx)
})

test_that("propensity design matching errors when simulated subjects exceed observed designs", {
  dat <- data.frame(
    ID = rep(1:2, each = 2),
    TIME = rep(c(0, 1), times = 2),
    AGE = c(20, 20, 80, 80),
    WT = c(70, 71, 80, 81)
  )

  local_mocked_bindings(
    sample_covariates_mice = function(
      data,
      cat_covs = NULL,
      n_subjects = nrow(data),
      ...
    ) {
      data[rep(seq_len(nrow(data)), length.out = n_subjects), , drop = FALSE]
    }
  )

  expect_error(
    sample_covariates_mice_timevarying(
      data = dat,
      static_covs = "AGE",
      time_varying_covs = "WT",
      design_match = "propensity",
      n_subjects = 3
    ),
    "without replacement"
  )
})

test_that("propensity design matching requires observed design profiles", {
  dat <- data.frame(
    ID = rep(1:2, each = 2),
    TIME = rep(c(0, 1), times = 2),
    AGE = c(20, 20, 80, 80),
    WT = c(70, 71, 80, 81)
  )

  expect_error(
    sample_covariates_mice_timevarying(
      data = dat,
      static_covs = "AGE",
      time_varying_covs = "WT",
      design_match = "propensity",
      time_grid = c(0, 1)
    ),
    "time_grid = NULL"
  )
})

test_that("default measurement pattern carries forward unchanged covariates", {
  dat <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 1, 2),
    WT = c(70, 70, 72),
    SCR = c(1.0, 1.1, 1.1)
  )

  local_mocked_bindings(
    sample_covariates_mice = function(
      data,
      cat_covs = NULL,
      n_subjects = nrow(data),
      ...
    ) {
      data[seq_len(n_subjects), , drop = FALSE]
    },
    impute_mice_transition_step = function(
      current,
      time_varying_covs,
      ...
    ) {
      out <- current[time_varying_covs]
      out$WT <- out$WT + 10
      out$SCR <- out$SCR + 0.1
      out
    }
  )

  out <- sample_covariates_mice_timevarying(
    data = dat,
    time_varying_covs = c("WT", "SCR"),
    n_subjects = 1
  )

  expect_equal(out$WT, c(70, 70, 80))
  expect_equal(out$SCR, c(1.0, 1.1, 1.1))
})

test_that("measurement_pattern = 'all' updates every covariate at every row", {
  dat <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 1, 2),
    WT = c(70, 70, 72)
  )

  local_mocked_bindings(
    sample_covariates_mice = function(
      data,
      cat_covs = NULL,
      n_subjects = nrow(data),
      ...
    ) {
      data[seq_len(n_subjects), , drop = FALSE]
    },
    impute_mice_transition_step = function(
      current,
      time_varying_covs,
      ...
    ) {
      out <- current[time_varying_covs]
      out$WT <- out$WT + 10
      out
    }
  )

  out <- sample_covariates_mice_timevarying(
    data = dat,
    time_varying_covs = "WT",
    measurement_pattern = "all",
    n_subjects = 1
  )

  expect_equal(out$WT, c(70, 80, 90))
})

test_that("NA transition imputations do not overwrite carried values", {
  dat <- data.frame(
    ID = c(1, 1, 1),
    TIME = c(0, 1, 2),
    WT = c(70, 71, 72),
    SCR = c(1.0, 1.1, 1.2)
  )

  local_mocked_bindings(
    sample_covariates_mice = function(
      data,
      cat_covs = NULL,
      n_subjects = nrow(data),
      ...
    ) {
      data[seq_len(n_subjects), , drop = FALSE]
    },
    impute_mice_transition_step = function(
      current,
      time_varying_covs,
      ...
    ) {
      out <- current[time_varying_covs]
      out$WT <- out$WT + 1
      out$SCR <- NA_real_
      out
    }
  )

  out <- sample_covariates_mice_timevarying(
    data = dat,
    time_varying_covs = c("WT", "SCR"),
    measurement_pattern = "all",
    n_subjects = 1
  )

  expect_equal(out$WT, c(70, 71, 72))
  expect_equal(out$SCR, c(1, 1, 1))
})

test_that("transition data contains lagged covariates and elapsed time", {
  dat <- data.frame(
    ID = rep(1:2, each = 3),
    TIME = rep(c(0, 2, 5), times = 2),
    SEX = rep(c("M", "F"), each = 3),
    WT = c(70, 72, 75, 60, 63, 66)
  )

  out <- make_mice_transition_data(
    data = dat,
    id_var = "ID",
    time_var = "TIME",
    static_covs = "SEX",
    time_varying_covs = "WT"
  )

  expect_named(out, c("TIME", ".delta_time", "SEX", ".WT_lag", "WT"))
  expect_equal(out$.delta_time, c(2, 3, 2, 3))
  expect_equal(out$.WT_lag, c(70, 72, 60, 63))
})

test_that("errors when transition sampling has no observed transitions", {
  dat <- data.frame(ID = 1:3, TIME = 0, WT = c(70, 60, 80))

  local_mocked_bindings(
    sample_covariates_mice = function(...) stop("should not be called")
  )

  expect_error(
    sample_covariates_mice_timevarying(
      data = dat,
      time_grid = c(0, 1),
      time_varying_covs = "WT"
    ),
    "at least two timepoints"
  )
})

test_that("real mice path runs end-to-end and returns valid covariates", {
  skip_if_not_installed("mice")

  set.seed(1)
  ids <- 1:30
  dat <- do.call(rbind, lapply(ids, function(i) {
    base_wt <- stats::rnorm(1, 70, 10)
    base_scr <- stats::rnorm(1, 1, 0.2)
    times <- c(0, 1, 2)
    data.frame(
      ID = i,
      TIME = times,
      SEX = sample(c("M", "F"), 1),
      WT = base_wt + cumsum(c(0, stats::rnorm(2, 0, 1))),
      SCR = base_scr + cumsum(c(0, stats::rnorm(2, 0, 0.05)))
    )
  }))

  out <- sample_covariates_mice_timevarying(
    data = dat,
    static_covs = "SEX",
    time_varying_covs = c("WT", "SCR"),
    cat_covs = "SEX",
    time_grid = c(0, 1, 2),
    n_subjects = 10,
    seed = 42
  )

  expect_named(out, c("ID", "TIME", "SEX", "WT", "SCR"))
  expect_equal(nrow(out), 30)
  expect_equal(sort(unique(out$ID)), 1:10)
  expect_false(anyNA(out$WT))
  expect_false(anyNA(out$SCR))
  expect_true(all(out$SEX %in% c("M", "F")))
  # internal working columns must not leak into the output
  expect_false(any(c(".delta_time", ".Type", ".WT_lag", ".SCR_lag") %in% names(out)))
})

test_that("real mice path handles single-row subjects with default 'change' pattern", {
  skip_if_not_installed("mice")

  # mix of multi-row and single-row subjects with >=2 time-varying covs:
  # regression test for the dim-collapse crash in make_mice_update_matrix().
  dat <- data.frame(
    ID = c(1, 1, 1, 2, 2, 3),
    TIME = c(0, 1, 2, 0, 1, 0),
    WT = c(70, 71, 72, 60, 61, 80),
    SCR = c(1.0, 1.1, 1.2, 0.9, 0.95, 1.3)
  )

  expect_no_error(
    out <- sample_covariates_mice_timevarying(
      data = dat,
      time_varying_covs = c("WT", "SCR"),
      n_subjects = 3,
      seed = 7
    )
  )
  expect_setequal(names(out), c("ID", "TIME", "WT", "SCR"))
})

test_that("real mice path tolerates user covariates colliding with internal names", {
  skip_if_not_installed("mice")

  # user covariates named like the former internal working columns must not be
  # silently overwritten now that internals are `.`-prefixed.
  set.seed(3)
  n_id <- 12
  dat <- do.call(rbind, lapply(seq_len(n_id), function(i) {
    data.frame(
      ID = i,
      TIME = c(0, 1, 2),
      delta_time = stats::rnorm(3, 7, 2),
      WT_lag = stats::rnorm(3, 42, 5),
      WT = stats::rnorm(3, 70, 8)
    )
  }))

  expect_no_error(
    out <- sample_covariates_mice_timevarying(
      data = dat,
      time_varying_covs = c("delta_time", "WT_lag", "WT"),
      time_grid = c(0, 1, 2),
      n_subjects = 4,
      seed = 11
    )
  )
  expect_true(all(c("delta_time", "WT_lag", "WT") %in% names(out)))
  # columns retained and not silently wiped to all-NA by an internal collision
  expect_true(any(!is.na(out$delta_time)))
  expect_true(any(!is.na(out$WT_lag)))
})

test_that("errors when covariates are both static and time-varying", {
  dat <- data.frame(
    ID = rep(1:2, each = 2),
    TIME = rep(c(0, 1), times = 2),
    WT = c(70, 71, 60, 61)
  )

  expect_error(
    sample_covariates_mice_timevarying(
      data = dat,
      static_covs = "WT",
      time_varying_covs = "WT"
    ),
    "both static and time-varying"
  )
})

test_that("errors when design_id_var collides with an existing column", {
  dat <- data.frame(
    ID = rep(1:2, each = 2),
    TIME = rep(c(0, 1), times = 2),
    WT = c(70, 71, 60, 61)
  )

  expect_error(
    sample_covariates_mice_timevarying(
      data = dat,
      time_varying_covs = "WT",
      design_id_var = "WT"
    ),
    "must not match"
  )
})

test_that("observation profiles ignore unused factor ID levels", {
  dat <- data.frame(
    ID = factor(c("b", "b", "c", "c"), levels = c("a", "b", "c")),
    TIME = c(0, 1, 0, 1),
    WT = c(70, 71, 80, 81)
  )
  profiles <- make_mice_observation_profiles(
    data = dat, id_var = "ID", time_var = "TIME",
    time_varying_covs = "WT", measurement_pattern = "all"
  )
  expect_length(profiles, 2)
  expect_equal(vapply(profiles, function(p) p$id, character(1)), c("b", "c"), ignore_attr = TRUE)
})

test_that("categorical time-varying covariates may gain levels after baseline", {
  dat <- data.frame(
    ID = rep(1:8, each = 3),
    TIME = rep(c(0, 1, 2), times = 8),
    WT = 60 + rep(1:8, each = 3) + rep(c(0, 1, 2), times = 8),
    STATE = rep(c("A", "A", "B"), times = 8) # "B" never occurs at baseline
  )
  out <- sample_covariates_mice_timevarying(
    dat,
    time_varying_covs = c("WT", "STATE"),
    cat_covs = "STATE",
    n_subjects = 8,
    seed = 1
  )
  expect_equal(nrow(out), nrow(dat))
  expect_true(all(as.character(out$STATE) %in% c("A", "B")))
  expect_false(anyNA(out$STATE))
})

test_that("propensity inputs use factor labels, not integer codes", {
  observed <- data.frame(SEX = c("M", "F", "M"))
  sampled <- data.frame(SEX = factor(c("F", "M")))
  out <- prep_mice_propensity_covariates(observed, sampled, cat_covs = "SEX")
  expect_equal(as.character(out$SEX), c("M", "F", "M", "F", "M"))
})

test_that("propensity matching ignores single-level categorical covariates", {
  baseline_data <- data.frame(SEX = factor(c("M", "M", "M")), WT = c(60, 70, 80))
  baseline <- data.frame(SEX = factor(c("M", "M")), WT = c(79, 61))
  idx <- match_mice_profiles_by_propensity(
    baseline_data = baseline_data,
    baseline = baseline,
    design_match_covs = c("SEX", "WT"),
    cat_covs = "SEX"
  )
  expect_length(idx, 2)
  expect_true(all(idx %in% 1:3))
  expect_false(anyDuplicated(idx) > 0)
})

test_that("integer-coded categorical covariates survive multiple transition steps", {
  # regression test for #18: from the second transition step on, the simulated
  # rows carry factors while the observed transitions hold integer codes, which
  # previously made `bind_rows()` fail in `run_mice_simulation()`.
  set.seed(2)
  dat <- do.call(rbind, lapply(1:20, function(i) {
    data.frame(
      ID = i,
      TIME = 0:3,
      SEX = sample(0:1, 1),
      WT = 70 + i + cumsum(stats::rnorm(4)),
      STATE = sample(1:2, 4, replace = TRUE)
    )
  }))
  expect_no_error(
    out <- sample_covariates_mice_timevarying(
      dat,
      static_covs = "SEX",
      time_varying_covs = c("WT", "STATE"),
      cat_covs = c("SEX", "STATE"),
      n_subjects = 10,
      seed = 1
    )
  )
  expect_true(all(as.character(out$SEX) %in% c("0", "1")))
  expect_true(all(as.character(out$STATE) %in% c("1", "2")))
  expect_false(anyNA(out$STATE))
  expect_false(anyNA(out$WT))
})

test_that("run_mice_simulation combines integer and factor categorical columns", {
  original <- data.frame(
    SEX = c(0L, 1L, 0L, 1L, 0L, 1L),
    WT = c(60, 70, 65, 75, 62, 72)
  )
  simulated <- data.frame(
    SEX = factor(c("1", "0"), levels = c("0", "1")),
    WT = c(NA_real_, NA_real_)
  )
  out <- run_mice_simulation(
    original = original,
    simulated = simulated,
    cat_covs = "SEX",
    cont_covs = "WT",
    cont_method = "pmm",
    predicted_vars = "WT"
  )
  expect_equal(nrow(out), 2)
  expect_s3_class(out$SEX, "factor")
  expect_equal(as.character(out$SEX), c("1", "0"))
  expect_false(anyNA(out$WT))
})

test_that("integer-coded covariate warning is raised once per call", {
  set.seed(1)
  dat <- data.frame(
    ID = rep(1:20, each = 3),
    TIME = rep(c(0, 24, 48), 20),
    WT = stats::rnorm(60, 70, 10),
    SEX = rep(rep(c(0, 1), 10), each = 3)
  )
  warnings <- character()
  withCallingHandlers(
    sample_covariates_mice_timevarying(
      dat, time_varying_covs = "WT", static_covs = "SEX", seed = 1, maxit = 2
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(grep("`SEX`", warnings), 1)
})

test_that("categorical covariates with too many levels are rejected up front", {
  dat <- data.frame(
    ID = rep(1:10, each = 3),
    TIME = rep(c(0, 1, 2), times = 10),
    PRT = seq(0.1, 3, by = 0.1),
    WT = rep(seq(60, 78, by = 2), each = 3)
  )
  local_mocked_bindings(
    sample_tv_baseline = function(...) stop("sampling should not be reached")
  )
  expect_error(
    sample_covariates_mice_timevarying(
      dat, time_varying_covs = c("PRT", "WT"), cat_covs = "PRT"
    ),
    "`PRT` \\(30 levels\\)",
    class = "irxforge_cat_levels_error"
  )
  # the raised limit is forwarded to the baseline sampler
  local_mocked_bindings(
    sample_tv_baseline = function(..., max_cat_levels) {
      stop("baseline max_cat_levels = ", max_cat_levels)
    }
  )
  expect_error(
    sample_covariates_mice_timevarying(
      dat, time_varying_covs = c("PRT", "WT"), cat_covs = "PRT",
      max_cat_levels = 30
    ),
    "baseline max_cat_levels = 30"
  )
})
