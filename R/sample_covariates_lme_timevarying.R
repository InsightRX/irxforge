#' Sample time-varying covariates using linear mixed-effects transition models
#'
#' Samples baseline covariates with [sample_covariates_mice()], then samples
#' later timepoints sequentially from per-covariate linear mixed-effects models
#' fitted to the observed first-order transitions. Each continuous time-varying
#' covariate is modelled as a function of the previous values of all
#' time-varying covariates (capturing both autocorrelation and between-covariate
#' relationships), the elapsed time since the previous measurement, the static
#' covariates and, optionally, absolute time. A subject-level random intercept
#' captures within-subject correlation; new random effects are drawn for each
#' simulated subject.
#'
#' @param data long-format `data.frame` or `tibble` containing one row per
#'   subject and time.
#' @param id_var subject identifier column name.
#' @param time_var time column name.
#' @param time_grid optional numeric vector of simulated times. If supplied,
#'   every simulated subject uses this same time grid. If `NULL` (default), each
#'   simulated subject clones an observed subject's covariate observation-time
#'   design. Designs are assigned deterministically in observed subject order
#'   and recycled if `n_subjects` exceeds the number of observed subjects.
#' @param static_covs character vector of covariates that should be sampled at
#'   baseline and then carried forward unchanged. Categorical covariates are
#'   only supported as static covariates (see `cat_covs`).
#' @param time_varying_covs character vector of covariates to sample at each
#'   timepoint. Default is all columns except `id_var`, `time_var`, and
#'   `static_covs`. Time-varying covariates must be continuous.
#' @param cat_covs character vector containing categorical covariates. These may
#'   only appear among `static_covs`; categorical time-varying covariates are
#'   not supported by the mixed-effects engine.
#' @param trend predictors used to model each transition. `"previous"` (default)
#'   regresses each covariate on the previous covariate values and the elapsed
#'   time only (a Markov / autoregressive transition). `"time"` additionally
#'   includes absolute `time_var` as a fixed effect, capturing a population time
#'   trajectory on top of the dependence on the previous value.
#' @param random_intercept logical, whether to include a subject-level random
#'   intercept (default `TRUE`). When `FALSE`, or when the mixed-effects fit
#'   fails to converge, the transition is modelled with ordinary least squares.
#' @param measurement_pattern how to determine when a time-varying covariate
#'   receives a new measurement in the sampled observed profile. `"change"`
#'   (default) treats a covariate as newly measured when its value changes from
#'   the previous row, which is useful for datasets where covariates have been
#'   carried forward. `"nonmissing"` treats non-missing values as measurements.
#'   `"all"` updates every time-varying covariate at every sampled row.
#' @param design_match method used to assign observed covariate observation-time
#'   designs to simulated subjects when `time_grid = NULL`. `"clone"` (default)
#'   assigns designs deterministically in observed subject order. `"propensity"`
#'   fits a logistic propensity model comparing observed baseline covariates to
#'   sampled baseline covariates, then assigns each simulated subject the design
#'   from the unmatched observed subject with the nearest propensity score.
#' @param design_match_covs character vector of baseline covariates to use for
#'   propensity-score design matching. Default is all static and time-varying
#'   covariates.
#' @param n_subjects number of simulated subjects. Default is the number of
#'   unique observed subjects.
#' @param conditional list with conditional limits applied to the **baseline**
#'   covariate sample, passed through to [sample_covariates_mice()]. Sequential
#'   transitions are not constrained. See [sample_covariates_mice()] for the
#'   accepted format.
#' @param cont_method method used to predict continuous covariates within mice
#'   for the baseline sample, default is `pmm`.
#' @param replicates number of independent simulated datasets to sample.
#'   Default is 1. When greater than 1, a `.replicate` column is included.
#' @param seed integer random seed passed to [set.seed()] for reproducibility.
#'   Default `NULL` does not set a seed.
#' @param ... additional arguments passed to [sample_covariates_mice()] for the
#'   baseline sample.
#'
#' @details
#' This function implements a first-order linear mixed-effects transition
#' sampler. For each continuous time-varying covariate `X_j` it fits
#' `X_j(t) ~ X_1(t-1) + ... + X_p(t-1) + delta_time [+ time] + static + (1 | id)`
#' to the observed transitions, then generates simulated trajectories by drawing
#' a fresh random intercept per simulated subject and adding residual noise at
#' each timepoint.
#'
#' In contrast to [sample_covariates_mice_timevarying()], which resamples
#' observed values via chained equations, this sampler interpolates and
#' extrapolates through the fitted linear models and so can produce covariate
#' values not present in the observed data. It is intended as a lightweight
#' parametric alternative when smooth, model-based trajectories are preferred.
#'
#' Missing values in `data` must be coded as `NA`. Transitions with missing
#' predictors or response are dropped when fitting the models.
#'
#' @returns long-format data.frame with simulated subject IDs, time, static
#'   covariates, and time-varying covariates.
#'
#' @seealso [sample_covariates_mice_timevarying()] for the chained-equations
#'   resampling alternative.
#'
#' @export
sample_covariates_lme_timevarying <- function(
  data,
  id_var = "ID",
  time_var = "TIME",
  time_grid = NULL,
  static_covs = NULL,
  time_varying_covs = NULL,
  cat_covs = NULL,
  trend = c("previous", "time"),
  random_intercept = TRUE,
  measurement_pattern = c("change", "nonmissing", "all"),
  design_match = c("clone", "propensity"),
  design_match_covs = NULL,
  n_subjects = length(unique(data[[id_var]])),
  conditional = NULL,
  cont_method = "pmm",
  replicates = 1,
  seed = NULL,
  ...
) {
  if (!is.null(seed)) set.seed(seed)
  trend <- match.arg(trend)
  measurement_pattern <- match.arg(measurement_pattern)
  design_match <- match.arg(design_match)

  if (!id_var %in% names(data)) {
    stop("`id_var` was not found in `data`.", call. = FALSE)
  }
  if (!time_var %in% names(data)) {
    stop("`time_var` was not found in `data`.", call. = FALSE)
  }
  if (!is.null(time_grid)) {
    time_grid <- sort(unique(time_grid))
    if (length(time_grid) == 0) {
      stop("`time_grid` must contain at least one timepoint.", call. = FALSE)
    }
  }
  if (!is.null(time_grid) && design_match == "propensity") {
    stop(
      "`design_match = \"propensity\"` can only be used when `time_grid = NULL`.",
      call. = FALSE
    )
  }
  if (replicates < 1) {
    stop("`replicates` must be at least 1.", call. = FALSE)
  }
  if (n_subjects < 1) {
    stop("`n_subjects` must be at least 1.", call. = FALSE)
  }

  if (is.null(static_covs)) {
    static_covs <- character()
  }
  if (is.null(time_varying_covs)) {
    time_varying_covs <- setdiff(names(data), c(id_var, time_var, static_covs))
  }
  overlap_covs <- intersect(static_covs, time_varying_covs)
  if (length(overlap_covs) > 0) {
    stop(
      "Covariates cannot be both static and time-varying: ",
      paste(overlap_covs, collapse = ", "),
      call. = FALSE
    )
  }
  covs <- c(static_covs, time_varying_covs)
  unknown_covs <- setdiff(covs, names(data))
  if (length(unknown_covs) > 0) {
    stop(
      "These covariates were not found in `data`: ",
      paste(unknown_covs, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(time_varying_covs) == 0) {
    stop("`time_varying_covs` must contain at least one covariate.", call. = FALSE)
  }
  cat_time_varying <- intersect(cat_covs, time_varying_covs)
  if (length(cat_time_varying) > 0) {
    stop(
      "Categorical time-varying covariates are not supported by the ",
      "mixed-effects sampler; treat them as static or use ",
      "`sample_covariates_mice_timevarying()`: ",
      paste(cat_time_varying, collapse = ", "),
      call. = FALSE
    )
  }
  if (is.null(design_match_covs)) {
    design_match_covs <- covs
  }
  unknown_match_covs <- setdiff(design_match_covs, covs)
  if (length(unknown_match_covs) > 0) {
    stop(
      "`design_match_covs` must be among static and time-varying covariates: ",
      paste(unknown_match_covs, collapse = ", "),
      call. = FALSE
    )
  }
  if (design_match == "propensity" && length(design_match_covs) == 0) {
    stop(
      "`design_match_covs` must contain at least one covariate for propensity matching.",
      call. = FALSE
    )
  }

  data <- data |>
    dplyr::arrange(.data[[id_var]], .data[[time_var]])

  observed_profiles <- make_mice_observation_profiles(
    data = data,
    id_var = id_var,
    time_var = time_var,
    time_varying_covs = time_varying_covs,
    measurement_pattern = measurement_pattern
  )

  baseline_data <- data |>
    dplyr::group_by(.data[[id_var]]) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(covs))

  transition_data <- make_lme_transition_data(
    data = data,
    id_var = id_var,
    time_var = time_var,
    static_covs = static_covs,
    time_varying_covs = time_varying_covs
  )
  max_timepoints <- if (is.null(time_grid)) {
    max(vapply(observed_profiles, function(x) length(x$time), integer(1)))
  } else {
    length(time_grid)
  }
  if (max_timepoints > 1 && nrow(transition_data) == 0) {
    stop(
      "`data` must contain at least two timepoints per subject to fit transitions.",
      call. = FALSE
    )
  }

  # Models are fitted once on the observed transitions and reused across
  # replicates; only the baseline sample and the drawn random effects/residuals
  # differ between replicates.
  models <- if (max_timepoints > 1) {
    fit_lme_transition_models(
      transition_data = transition_data,
      id_var = id_var,
      time_var = time_var,
      static_covs = static_covs,
      time_varying_covs = time_varying_covs,
      cat_covs = cat_covs,
      trend = trend,
      random_intercept = random_intercept
    )
  } else {
    list()
  }

  out <- vector("list", replicates)
  for (replicate_idx in seq_len(replicates)) {
    baseline <- sample_covariates_mice(
      data = baseline_data,
      cat_covs = intersect(cat_covs, covs),
      conditional = conditional,
      n_subjects = n_subjects,
      cont_method = cont_method,
      replicates = 1,
      seed = NULL,
      ...
    )

    subject_profiles <- if (is.null(time_grid)) {
      assign_mice_observation_profiles(
        observed_profiles = observed_profiles,
        baseline_data = baseline_data,
        baseline = baseline,
        design_match = design_match,
        design_match_covs = design_match_covs,
        cat_covs = cat_covs,
        n_subjects = n_subjects
      )
    } else {
      rep(
        list(list(
          time = time_grid,
          update = matrix(
            TRUE,
            nrow = length(time_grid),
            ncol = length(time_varying_covs),
            dimnames = list(NULL, time_varying_covs)
          )
        )),
        n_subjects
      )
    }

    # Draw a fresh random intercept per simulated subject for each covariate.
    ranef <- draw_lme_random_effects(
      models = models,
      time_varying_covs = time_varying_covs,
      n_subjects = n_subjects
    )

    current <- baseline
    current[[id_var]] <- seq_len(n_subjects)
    current[[time_var]] <- vapply(
      subject_profiles,
      function(x) x$time[[1]],
      numeric(1)
    )
    current <- current[, c(id_var, time_var, covs), drop = FALSE]

    profile_lengths <- vapply(subject_profiles, function(x) length(x$time), integer(1))
    replicate_rows <- vector("list", max(profile_lengths))
    replicate_rows[[1]] <- current

    if (length(replicate_rows) > 1) {
      for (time_idx in 2:length(replicate_rows)) {
        active_subjects <- which(profile_lengths >= time_idx)
        active_current <- current[active_subjects, , drop = FALSE]
        active_time <- vapply(
          subject_profiles[active_subjects],
          function(x) x$time[[time_idx]],
          numeric(1)
        )
        active_previous_time <- vapply(
          subject_profiles[active_subjects],
          function(x) x$time[[time_idx - 1]],
          numeric(1)
        )
        active_update <- do.call(
          rbind,
          lapply(
            subject_profiles[active_subjects],
            function(x) x$update[time_idx, , drop = FALSE]
          )
        )

        if (any(active_update)) {
          next_covs <- simulate_lme_transition_step(
            models = models,
            current = active_current,
            time = active_time,
            previous_time = active_previous_time,
            time_var = time_var,
            static_covs = static_covs,
            time_varying_covs = time_varying_covs,
            ranef = ranef[active_subjects, , drop = FALSE]
          )

          for (cov in time_varying_covs) {
            update_rows <- active_update[, cov] & !is.na(next_covs[[cov]])
            active_current[update_rows, cov] <- next_covs[update_rows, cov]
          }
        }
        active_current[[time_var]] <- active_time
        current[active_subjects, ] <- active_current
        replicate_rows[[time_idx]] <- active_current
      }
    }

    out[[replicate_idx]] <- dplyr::bind_rows(replicate_rows)
    if (replicates > 1) {
      out[[replicate_idx]][[".replicate"]] <- replicate_idx
    }
  }

  out <- dplyr::bind_rows(out)
  if (tibble::is_tibble(data)) out <- tibble::as_tibble(out)
  out
}

#' Build first-order transition rows for the mixed-effects sampler
#'
#' Like `make_mice_transition_data()` but retains `id_var` so that a
#' subject-level random effect can be fitted. Each row is one observed
#' transition: the current time-varying covariate values together with the
#' previous values (`.{cov}_lag`), the elapsed time (`.delta_time`), absolute
#' time and the static covariates.
#'
#' @noRd
make_lme_transition_data <- function(
  data,
  id_var,
  time_var,
  static_covs,
  time_varying_covs
) {
  lag_covs <- paste0(".", time_varying_covs, "_lag")

  data |>
    dplyr::group_by(.data[[id_var]]) |>
    dplyr::arrange(.data[[time_var]], .by_group = TRUE) |>
    dplyr::mutate(
      .previous_time = dplyr::lag(.data[[time_var]]),
      .delta_time = .data[[time_var]] - .data$.previous_time
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(time_varying_covs),
        dplyr::lag,
        .names = ".{.col}_lag"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$.previous_time)) |>
    dplyr::select(
      dplyr::all_of(
        c(id_var, time_var, ".delta_time", static_covs, lag_covs, time_varying_covs)
      )
    )
}

#' Predictor names shared by every transition model
#'
#' @noRd
lme_transition_predictors <- function(
  time_var,
  static_covs,
  time_varying_covs,
  trend
) {
  lag_covs <- paste0(".", time_varying_covs, "_lag")
  c(
    if (trend == "time") time_var,
    ".delta_time",
    static_covs,
    lag_covs
  )
}

#' Fit one linear mixed-effects transition model per time-varying covariate
#'
#' Returns a named list of fitted models, each carrying the residual SD
#' (`sigma`), the random-intercept SD (`tau`), the fit object and its type
#' (`"lme"` or `"lm"`). Covariates whose models cannot be fitted at all are
#' stored as `NULL` and are carried forward unchanged during simulation.
#'
#' @noRd
fit_lme_transition_models <- function(
  transition_data,
  id_var,
  time_var,
  static_covs,
  time_varying_covs,
  cat_covs,
  trend,
  random_intercept
) {
  predictors <- lme_transition_predictors(
    time_var = time_var,
    static_covs = static_covs,
    time_varying_covs = time_varying_covs,
    trend = trend
  )

  cat_static <- intersect(cat_covs, static_covs)
  td <- transition_data
  td[cat_static] <- lapply(td[cat_static], as.factor)

  random <- stats::as.formula(paste0("~ 1 | ", id_var))

  fell_back <- character()
  unfit <- character()
  models <- stats::setNames(vector("list", length(time_varying_covs)), time_varying_covs)
  for (cov in time_varying_covs) {
    fml <- stats::reformulate(predictors, response = cov)

    fit <- NULL
    if (random_intercept) {
      fit <- tryCatch(
        suppressWarnings(nlme::lme(
          fixed = fml,
          random = random,
          data = td,
          na.action = stats::na.omit,
          control = nlme::lmeControl(opt = "optim", returnObject = TRUE)
        )),
        error = function(e) NULL
      )
    }

    if (!is.null(fit)) {
      tau <- sqrt(as.numeric(nlme::getVarCov(fit)))
      models[[cov]] <- list(
        fit = fit,
        type = "lme",
        sigma = fit$sigma,
        tau = if (length(tau) == 1 && is.finite(tau)) tau else 0
      )
      next
    }

    if (random_intercept) fell_back <- c(fell_back, cov)
    lm_fit <- tryCatch(
      stats::lm(fml, data = td, na.action = stats::na.omit),
      error = function(e) NULL
    )
    if (is.null(lm_fit)) {
      unfit <- c(unfit, cov)
      next
    }
    models[[cov]] <- list(
      fit = lm_fit,
      type = "lm",
      sigma = stats::sigma(lm_fit),
      tau = 0
    )
  }

  if (length(fell_back) > 0) {
    warning(
      "Mixed-effects fit failed; used ordinary least squares for: ",
      paste(fell_back, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(unfit) > 0) {
    warning(
      "Could not fit a transition model; carrying forward baseline values for: ",
      paste(unfit, collapse = ", "),
      call. = FALSE
    )
  }
  models
}

#' Draw subject-level random intercepts for each covariate model
#'
#' @noRd
draw_lme_random_effects <- function(models, time_varying_covs, n_subjects) {
  ranef <- matrix(
    0,
    nrow = n_subjects,
    ncol = length(time_varying_covs),
    dimnames = list(NULL, time_varying_covs)
  )
  for (cov in time_varying_covs) {
    tau <- models[[cov]]$tau
    if (!is.null(tau) && tau > 0) {
      ranef[, cov] <- stats::rnorm(n_subjects, mean = 0, sd = tau)
    }
  }
  ranef
}

#' Simulate one transition step for all active subjects
#'
#' For each time-varying covariate, predicts the population (fixed-effects) mean
#' from the previous covariate values, adds the subject's drawn random intercept
#' and a fresh residual draw. Covariates without a fitted model are carried
#' forward unchanged.
#'
#' @noRd
simulate_lme_transition_step <- function(
  models,
  current,
  time,
  previous_time,
  time_var,
  static_covs,
  time_varying_covs,
  ranef
) {
  lag_covs <- paste0(".", time_varying_covs, "_lag")
  newdata <- current[, static_covs, drop = FALSE]
  newdata[[time_var]] <- time
  newdata[[".delta_time"]] <- time - previous_time
  newdata[lag_covs] <- current[time_varying_covs]

  out <- current[, time_varying_covs, drop = FALSE]
  for (cov in time_varying_covs) {
    model <- models[[cov]]
    if (is.null(model)) {
      # No fitted model: carry the previous value forward.
      out[[cov]] <- current[[cov]]
      next
    }
    # `na.exclude` pads predictions for rows with missing predictors back to
    # `NA` instead of erroring; those covariates are then left unchanged by the
    # caller's `!is.na()` update filter.
    mu <- if (model$type == "lme") {
      as.numeric(stats::predict(
        model$fit,
        newdata = newdata,
        level = 0,
        na.action = stats::na.exclude
      ))
    } else {
      as.numeric(stats::predict(
        model$fit,
        newdata = newdata,
        na.action = stats::na.exclude
      ))
    }
    residual <- stats::rnorm(nrow(newdata), mean = 0, sd = model$sigma)
    out[[cov]] <- mu + ranef[, cov] + residual
  }
  out
}
