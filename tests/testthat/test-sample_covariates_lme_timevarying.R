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
    fit_lme_transition_models = function(...) list(),
    simulate_lme_transition_step = function(
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

  out <- sample_covariates_lme_timevarying(
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
    fit_lme_transition_models = function(...) list(),
    simulate_lme_transition_step = function(current, time_varying_covs, ...) {
      current[time_varying_covs]
    }
  )

  out <- sample_covariates_lme_timevarying(
    data = dat,
    time_varying_covs = "WT",
    n_subjects = 2
  )

  expect_s3_class(out, "tbl_df")
})

test_that("only flagged covariates are updated each step", {
  dat <- data.frame(
    ID = rep(1:3, each = 3),
    TIME = rep(c(0, 1, 2), times = 3),
    WT = c(70, 70, 75, 60, 60, 66, 80, 80, 88),
    CRCL = c(90, 95, 95, 80, 85, 85, 100, 105, 105)
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
    # Deterministic step: every covariate would become 999 if updated.
    fit_lme_transition_models = function(...) list(),
    simulate_lme_transition_step = function(current, time_varying_covs, ...) {
      out <- current[time_varying_covs]
      out[] <- 999
      out
    }
  )

  out <- sample_covariates_lme_timevarying(
    data = dat,
    time_varying_covs = c("WT", "CRCL"),
    measurement_pattern = "change",
    n_subjects = 1,
    time_grid = NULL
  )

  # Subject 1 design: WT changes only at t = 2; CRCL changes only at t = 1.
  expect_equal(out$WT, c(70, 70, 999))
  expect_equal(out$CRCL, c(90, 999, 999))
})

test_that("categorical time-varying covariates are rejected", {
  dat <- data.frame(
    ID = rep(1:2, each = 2),
    TIME = rep(c(0, 1), times = 2),
    GRP = c("A", "B", "A", "A")
  )
  expect_error(
    sample_covariates_lme_timevarying(
      data = dat,
      time_varying_covs = "GRP",
      cat_covs = "GRP"
    ),
    "Categorical time-varying covariates are not supported"
  )
})

test_that("input validation errors fire", {
  dat <- data.frame(
    ID = rep(1:2, each = 2),
    TIME = rep(c(0, 1), times = 2),
    WT = c(70, 71, 60, 61)
  )

  expect_error(
    sample_covariates_lme_timevarying(dat, id_var = "MISSING"),
    "`id_var` was not found"
  )
  expect_error(
    sample_covariates_lme_timevarying(dat, time_var = "MISSING"),
    "`time_var` was not found"
  )
  expect_error(
    sample_covariates_lme_timevarying(
      dat,
      static_covs = "WT",
      time_varying_covs = "WT"
    ),
    "cannot be both static and time-varying"
  )
  expect_error(
    sample_covariates_lme_timevarying(dat, time_varying_covs = "NOPE"),
    "were not found in `data`"
  )
  expect_error(
    sample_covariates_lme_timevarying(
      dat,
      time_varying_covs = "WT",
      time_grid = c(0, 1),
      design_match = "propensity"
    ),
    "can only be used when `time_grid = NULL`"
  )
  expect_error(
    sample_covariates_lme_timevarying(dat, time_varying_covs = "WT", replicates = 0),
    "`replicates` must be at least 1"
  )
  expect_error(
    sample_covariates_lme_timevarying(dat, time_varying_covs = "WT", n_subjects = 0),
    "`n_subjects` must be at least 1"
  )
})

test_that("single-timepoint data returns baseline only without fitting models", {
  dat <- data.frame(
    ID = 1:4,
    TIME = 0,
    WT = c(70, 60, 80, 90)
  )

  # If a transition model were fitted this mock would not matter, but spy on it
  # by making the step explode -- it must never be called.
  local_mocked_bindings(
    fit_lme_transition_models = function(...) list(),
    simulate_lme_transition_step = function(...) stop("should not be reached")
  )

  out <- sample_covariates_lme_timevarying(
    data = dat,
    time_varying_covs = "WT",
    n_subjects = 4,
    seed = 1
  )
  expect_equal(nrow(out), 4)
  expect_equal(out$TIME, rep(0, 4), ignore_attr = TRUE)
})

# ---- End-to-end tests through the real nlme path -----------------------------

make_longitudinal_data <- function(n = 30, seed = 1) {
  withr::with_seed(seed, {
    do.call(rbind, lapply(seq_len(n), function(id) {
      k <- sample(4:6, 1)
      t <- cumsum(c(0, round(runif(k - 1, 1, 4))))
      age <- round(runif(1, 20, 80))
      sex <- sample(c("M", "F"), 1)
      wt <- rnorm(1, 75, 10) + cumsum(c(0, rnorm(k - 1, 0, 1.5))) + 0.2 * t
      crcl <- 100 - 0.4 * age + 0.1 * (wt - 75) + rnorm(k, 0, 4)
      data.frame(ID = id, TIME = t, AGE = age, SEX = sex, WT = wt, CRCL = crcl)
    }))
  })
}

test_that("end-to-end simulation produces well-formed output", {
  dat <- make_longitudinal_data()
  out <- sample_covariates_lme_timevarying(
    data = dat,
    static_covs = c("AGE", "SEX"),
    time_varying_covs = c("WT", "CRCL"),
    cat_covs = "SEX",
    n_subjects = 20,
    seed = 123
  )

  expect_named(out, c("ID", "TIME", "AGE", "SEX", "WT", "CRCL"))
  expect_equal(length(unique(out$ID)), 20)
  expect_false(anyNA(out$WT))
  expect_false(anyNA(out$CRCL))
  # Static covariates stay constant within subject.
  age_spread <- tapply(out$AGE, out$ID, function(x) length(unique(x)))
  expect_true(all(age_spread == 1))
})

test_that("seed makes the end-to-end simulation reproducible", {
  dat <- make_longitudinal_data()
  args <- list(
    data = dat,
    static_covs = "AGE",
    time_varying_covs = c("WT", "CRCL"),
    n_subjects = 15,
    seed = 99
  )
  out1 <- do.call(sample_covariates_lme_timevarying, args)
  out2 <- do.call(sample_covariates_lme_timevarying, args)
  expect_equal(out1, out2)
})

test_that("time_grid yields a shared grid for all subjects", {
  dat <- make_longitudinal_data()
  grid <- c(0, 2, 4, 6)
  out <- sample_covariates_lme_timevarying(
    data = dat,
    static_covs = "AGE",
    time_varying_covs = c("WT", "CRCL"),
    time_grid = grid,
    n_subjects = 10,
    seed = 5
  )
  expect_equal(sort(unique(out$TIME)), grid)
  expect_equal(nrow(out), 10 * length(grid))
})

test_that("trend = 'time' recovers an increasing population trajectory", {
  # WT has a clear positive time slope; the time-trend model should reproduce a
  # rising mean WT across the grid.
  dat <- make_longitudinal_data(n = 60, seed = 2)
  out <- sample_covariates_lme_timevarying(
    data = dat,
    static_covs = "AGE",
    time_varying_covs = c("WT", "CRCL"),
    trend = "time",
    time_grid = c(0, 4, 8, 12),
    n_subjects = 200,
    seed = 11
  )
  mean_wt <- tapply(out$WT, out$TIME, mean)
  expect_gt(mean_wt[["12"]], mean_wt[["0"]])
})

test_that("random_intercept = FALSE falls back to OLS and still runs", {
  dat <- make_longitudinal_data()
  out <- sample_covariates_lme_timevarying(
    data = dat,
    static_covs = "AGE",
    time_varying_covs = c("WT", "CRCL"),
    random_intercept = FALSE,
    n_subjects = 12,
    seed = 4
  )
  expect_equal(length(unique(out$ID)), 12)
  expect_false(anyNA(out$WT))
})

test_that("replicates add a .replicate column", {
  dat <- make_longitudinal_data()
  out <- sample_covariates_lme_timevarying(
    data = dat,
    static_covs = "AGE",
    time_varying_covs = c("WT", "CRCL"),
    n_subjects = 8,
    replicates = 3,
    seed = 8
  )
  expect_true(".replicate" %in% names(out))
  expect_equal(sort(unique(out$.replicate)), 1:3)
})

test_that("dispatcher routes method = 'lme_timevarying'", {
  dat <- make_longitudinal_data()
  out <- sample_covariates(
    method = "lme_timevarying",
    data = dat,
    static_covs = "AGE",
    time_varying_covs = c("WT", "CRCL"),
    n_subjects = 10,
    seed = 2
  )
  expect_equal(length(unique(out$ID)), 10)
})

test_that("propensity design matching runs end-to-end", {
  dat <- make_longitudinal_data(n = 40)
  out <- sample_covariates_lme_timevarying(
    data = dat,
    static_covs = c("AGE", "SEX"),
    time_varying_covs = c("WT", "CRCL"),
    cat_covs = "SEX",
    design_match = "propensity",
    n_subjects = 20,
    seed = 6
  )
  expect_equal(length(unique(out$ID)), 20)
  expect_false(anyNA(out$WT))
})

test_that("categorical level seen only at baseline does not crash prediction", {
  # Subjects with category "X" are all single-timepoint, so they contribute no
  # transition rows and "X" is never a fitting-time factor level. The baseline
  # sampler can still draw "X", which previously made predict() abort with
  # "factor has new levels". Such rows must instead carry forward unchanged.
  rows <- list()
  for (id in 1:30) {
    t <- 0:3
    sex <- if (id %% 2 == 0) "M" else "F"
    rows[[length(rows) + 1]] <- data.frame(
      ID = id, TIME = t, SEX = sex, WT = 75 + id %% 10 + 0.5 * t
    )
  }
  for (id in 90:95) {
    rows[[length(rows) + 1]] <- data.frame(ID = id, TIME = 0, SEX = "X", WT = 70)
  }
  dat <- do.call(rbind, rows)

  out <- suppressWarnings(sample_covariates_lme_timevarying(
    data = dat,
    static_covs = "SEX",
    time_varying_covs = "WT",
    cat_covs = "SEX",
    n_subjects = 36,
    seed = 5
  ))

  expect_false(anyNA(out$WT))
  # Subjects carrying the fitting-time-unseen level keep WT constant (LOCF).
  x_ids <- unique(out$ID[out$SEX == "X"])
  if (length(x_ids) > 0) {
    spread <- tapply(out$WT[out$ID %in% x_ids], out$ID[out$SEX == "X"], function(x) {
      length(unique(x))
    })
    expect_true(all(spread == 1))
  }
})

test_that("a missing lag predictor carries the covariate forward, no recycling", {
  # Directly exercise simulate_lme_transition_step with one subject whose lag
  # predictor is NA: predict() drops that row, so the fix must leave the
  # covariate unchanged rather than recycle another subject's prediction.
  dat <- make_longitudinal_data(n = 20)
  models <- fit_lme_transition_models(
    transition_data = make_lme_transition_data(
      dat[order(dat$ID, dat$TIME), ],
      id_var = "ID", time_var = "TIME",
      static_covs = "AGE", time_varying_covs = c("WT", "CRCL")
    ),
    id_var = "ID", time_var = "TIME", static_covs = "AGE",
    time_varying_covs = c("WT", "CRCL"), cat_covs = NULL,
    trend = "previous", random_intercept = TRUE
  )
  current <- data.frame(AGE = c(40, 50), WT = c(80, NA), CRCL = c(90, 95))
  ranef <- matrix(0, nrow = 2, ncol = 2, dimnames = list(NULL, c("WT", "CRCL")))

  out <- withr::with_seed(1, simulate_lme_transition_step(
    models = models, current = current,
    time = c(1, 1), previous_time = c(0, 0),
    time_var = "TIME", static_covs = "AGE",
    time_varying_covs = c("WT", "CRCL"), ranef = ranef
  ))

  expect_equal(nrow(out), 2)
  # Row 2 has an NA WT lag -> all its time-varying covs carried forward.
  expect_equal(out$WT[2], NA_real_)
  expect_equal(out$CRCL[2], 95)
})

test_that("design_id_var retains the matched observed subject id", {
  set.seed(1)
  dat <- data.frame(
    ID = rep(1:6, each = 3),
    TIME = rep(c(0, 1, 2), 6),
    WT = rep(c(50, 60, 70, 80, 90, 100), each = 3) + stats::rnorm(18, 0, 1)
  )
  out <- suppressWarnings(sample_covariates_lme_timevarying(
    dat, time_varying_covs = "WT", n_subjects = 6,
    design_match = "clone", design_id_var = ".design_id", seed = 1
  ))
  expect_true(".design_id" %in% names(out))
  expect_true(all(out$.design_id %in% as.character(dat$ID)))
})

test_that("design_id_var is rejected together with time_grid", {
  dat <- data.frame(ID = rep(1:4, each = 2), TIME = rep(c(0, 1), 4), WT = rnorm(8, 70, 5))
  expect_error(
    sample_covariates_lme_timevarying(
      dat, time_varying_covs = "WT", design_id_var = ".design_id",
      time_grid = c(0, 1)
    ),
    "time_grid"
  )
})
