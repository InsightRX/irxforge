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

  expect_named(out, c("TIME", "delta_time", "SEX", "WT_lag", "WT"))
  expect_equal(out$delta_time, c(2, 3, 2, 3))
  expect_equal(out$WT_lag, c(70, 72, 60, 63))
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
