#' Sample time-varying covariates using sequential MICE
#'
#' Samples baseline covariates with [sample_covariates_mice()], then samples
#' later timepoints sequentially from a transition model. Each transition is
#' imputed conditional on the previous simulated covariate values, time, elapsed
#' time, and any static covariates.
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
#'   baseline and then carried forward unchanged.
#' @param time_varying_covs character vector of covariates to sample at each
#'   timepoint. Default is all columns except `id_var`, `time_var`, and
#'   `static_covs`.
#' @param cat_covs character vector containing categorical covariates among
#'   `static_covs` and `time_varying_covs`.
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
#' @param cont_method method used to predict continuous covariates within mice,
#'   default is `pmm`.
#' @param replicates number of independent simulated datasets to sample.
#'   Default is 1. When greater than 1, a `.replicate` column is included.
#' @param seed integer random seed passed to [set.seed()] for reproducibility.
#'   Default `NULL` does not set a seed.
#' @param ... additional arguments passed to `mice::mice()`.
#'
#' @details
#' This function implements a first-order empirical transition sampler:
#' `X(t) ~ X(t - 1), time, delta_time, static covariates`. It is intended for
#' simulation from observed longitudinal covariate data, not for replacing a
#' mechanistic or mixed-effects longitudinal model.
#'
#' Missing values in `data` must be coded as `NA`.
#'
#' @returns long-format data.frame with simulated subject IDs, time, static
#'   covariates, and time-varying covariates.
#'
#' @export
sample_covariates_mice_timevarying <- function(
  data,
  id_var = "ID",
  time_var = "TIME",
  time_grid = NULL,
  static_covs = NULL,
  time_varying_covs = NULL,
  cat_covs = NULL,
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

  transition_data <- make_mice_transition_data(
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
      "`data` must contain at least two timepoints per subject to sample transitions.",
      call. = FALSE
    )
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
          next_covs <- impute_mice_transition_step(
            transition_data = transition_data,
            current = active_current,
            time = active_time,
            previous_time = active_previous_time,
            time_var = time_var,
            static_covs = static_covs,
            time_varying_covs = time_varying_covs,
            cat_covs = cat_covs,
            cont_method = cont_method,
            ...
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

clone_mice_observation_profiles <- function(observed_profiles, n_subjects) {
  observed_profiles[
    rep(seq_along(observed_profiles), length.out = n_subjects)
  ]
}

assign_mice_observation_profiles <- function(
  observed_profiles,
  baseline_data,
  baseline,
  design_match,
  design_match_covs,
  cat_covs,
  n_subjects
) {
  if (design_match == "clone") {
    return(clone_mice_observation_profiles(observed_profiles, n_subjects))
  }
  if (n_subjects > length(observed_profiles)) {
    stop(
      "`design_match = \"propensity\"` matches without replacement, so ",
      "`n_subjects` cannot exceed the number of observed subjects.",
      call. = FALSE
    )
  }

  match_idx <- match_mice_profiles_by_propensity(
    baseline_data = baseline_data,
    baseline = baseline,
    design_match_covs = design_match_covs,
    cat_covs = cat_covs
  )
  observed_profiles[match_idx]
}

match_mice_profiles_by_propensity <- function(
  baseline_data,
  baseline,
  design_match_covs,
  cat_covs
) {
  observed <- baseline_data[, design_match_covs, drop = FALSE]
  sampled <- baseline[, design_match_covs, drop = FALSE]
  combined <- prep_mice_propensity_covariates(
    observed = observed,
    sampled = sampled,
    cat_covs = intersect(cat_covs, design_match_covs)
  )
  combined[[".sampled"]] <- c(rep(0, nrow(observed)), rep(1, nrow(sampled)))

  pred <- stats::model.matrix(
    stats::reformulate(design_match_covs),
    data = combined
  )
  pred <- pred[, colnames(pred) != "(Intercept)", drop = FALSE]
  keep <- apply(pred, 2, function(x) length(unique(x)) > 1)
  pred <- pred[, keep, drop = FALSE]
  if (ncol(pred) == 0) {
    return(seq_len(nrow(sampled)))
  }

  model_data <- data.frame(.sampled = combined$.sampled, pred, check.names = FALSE)
  fit <- suppressWarnings(stats::glm(
    .sampled ~ .,
    data = model_data,
    family = stats::binomial()
  ))
  scores <- stats::predict(fit, type = "response")
  observed_scores <- scores[seq_len(nrow(observed))]
  sampled_scores <- scores[seq(from = nrow(observed) + 1, length.out = nrow(sampled))]
  observed_pred <- pred[seq_len(nrow(observed)), , drop = FALSE]
  sampled_pred <- pred[seq(from = nrow(observed) + 1, length.out = nrow(sampled)), , drop = FALSE]

  used <- logical(length(observed_scores))
  match_idx <- integer(length(sampled_scores))
  for (i in seq_along(sampled_scores)) {
    observed_candidates <- which(!used)
    score_diff <- abs(observed_scores[observed_candidates] - sampled_scores[[i]])
    best_score <- min(score_diff)
    candidates <- observed_candidates[
      score_diff <= best_score + sqrt(.Machine$double.eps)
    ]
    if (length(candidates) > 1) {
      cov_dist <- rowSums(
        sweep(observed_pred[candidates, , drop = FALSE], 2, sampled_pred[i, ], "-")^2
      )
      candidates <- candidates[[which.min(cov_dist)]]
    }
    match_idx[[i]] <- candidates
    used[candidates] <- TRUE
  }
  match_idx
}

prep_mice_propensity_covariates <- function(observed, sampled, cat_covs) {
  data <- data.frame(row.names = seq_len(nrow(observed) + nrow(sampled)))
  for (col in names(observed)) {
    x <- c(observed[[col]], sampled[[col]])
    if (col %in% cat_covs || is.factor(x) || is.character(x)) {
      x <- as.character(x)
      x[is.na(x)] <- "<missing>"
      data[[col]] <- factor(x)
    } else {
      if (!is.numeric(x)) {
        x <- as.numeric(x)
      }
      if (anyNA(x)) {
        fill <- stats::median(x, na.rm = TRUE)
        if (is.na(fill)) fill <- 0
        x[is.na(x)] <- fill
      }
      data[[col]] <- x
    }
  }
  data
}

make_mice_observation_profiles <- function(
  data,
  id_var,
  time_var,
  time_varying_covs,
  measurement_pattern
) {
  profiles <- split(data, data[[id_var]])
  lapply(profiles, function(profile) {
    profile <- profile |>
      dplyr::arrange(.data[[time_var]])
    list(
      time = profile[[time_var]],
      update = make_mice_update_matrix(
        profile = profile,
        time_varying_covs = time_varying_covs,
        measurement_pattern = measurement_pattern
      )
    )
  })
}

make_mice_update_matrix <- function(
  profile,
  time_varying_covs,
  measurement_pattern
) {
  update <- switch(
    measurement_pattern,
    all = matrix(
      TRUE,
      nrow = nrow(profile),
      ncol = length(time_varying_covs),
      dimnames = list(NULL, time_varying_covs)
    ),
    nonmissing = as.matrix(!is.na(profile[time_varying_covs])),
    change = sapply(profile[time_varying_covs], function(x) {
      changed <- c(TRUE, x[-1] != x[-length(x)])
      changed[is.na(changed)] <- TRUE
      changed
    })
  )

  if (is.null(dim(update))) {
    # `sapply()` over a single-row profile drops dims to a length-k vector; force
    # the matrix shape explicitly so 1-row subjects with multiple covariates do
    # not collapse to a k x 1 matrix that breaks the `colnames<-` below.
    update <- matrix(
      update,
      nrow = nrow(profile),
      ncol = length(time_varying_covs)
    )
  }
  colnames(update) <- time_varying_covs
  update[1, ] <- TRUE
  update
}

make_mice_transition_data <- function(
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
      dplyr::all_of(c(time_var, ".delta_time", static_covs, lag_covs, time_varying_covs))
    )
}

impute_mice_transition_step <- function(
  transition_data,
  current,
  time,
  previous_time,
  time_var,
  static_covs,
  time_varying_covs,
  cat_covs,
  cont_method,
  ...
) {
  lag_covs <- paste0(".", time_varying_covs, "_lag")
  synthetic <- current[, static_covs, drop = FALSE]
  synthetic[[time_var]] <- time
  synthetic[[".delta_time"]] <- time - previous_time
  synthetic[lag_covs] <- current[time_varying_covs]
  synthetic[time_varying_covs] <- NA
  synthetic <- synthetic[
    c(time_var, ".delta_time", static_covs, lag_covs, time_varying_covs)
  ]

  cat_lag_covs <- paste0(".", intersect(cat_covs, time_varying_covs), "_lag")
  factor_covs <- intersect(c(cat_covs, cat_lag_covs), names(transition_data))

  run_mice_simulation(
    original = transition_data,
    simulated = synthetic,
    cat_covs = factor_covs,
    cont_covs = setdiff(time_varying_covs, cat_covs),
    cont_method = cont_method,
    m = 1,
    predicted_vars = time_varying_covs,
    ...
  ) |>
    dplyr::select(dplyr::all_of(time_varying_covs))
}

#' Run a single MICE simulation pass shared by the (time-varying) mice samplers
#'
#' Binds observed (`original`) and to-be-imputed (`simulated`) rows via an
#' internal `.Type` flag, runs one set of chained equations, and returns the
#' completed simulated rows in long format. Centralising this here keeps the
#' `Type` bookkeeping, predictor-matrix wiring, method selection and warning
#' handling identical across `sample_covariates_mice()` and the time-varying
#' transition sampler so the two paths cannot drift.
#'
#' @param original observed data used as the imputation donors.
#' @param simulated rows whose target columns are `NA` and will be imputed.
#' @param cat_covs categorical covariates to coerce to factors before fitting.
#' @param cont_covs continuous covariates to impute with `cont_method`.
#' @param cont_method mice method for continuous covariates (e.g. `"pmm"`).
#' @param m number of imputations.
#' @param predicted_vars optional character vector restricting which columns are
#'   imputed (others are excluded as both targets and predictors). `NULL`
#'   imputes every column with missing values.
#' @param ... additional arguments passed to `mice::mice()`.
#' @returns long-format completed data filtered to the simulated rows.
#' @noRd
run_mice_simulation <- function(
  original,
  simulated,
  cat_covs,
  cont_covs,
  cont_method,
  m = 1,
  predicted_vars = NULL,
  ...
) {
  comb <- original |>
    dplyr::mutate(.Type = "Original") |>
    dplyr::bind_rows(
      simulated |>
        dplyr::mutate(.Type = "Simulated")
    ) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(cat_covs), as.factor))

  pred <- mice::make.predictorMatrix(comb)
  pred[, ".Type"] <- 0
  if (!is.null(predicted_vars)) {
    pred[setdiff(rownames(pred), predicted_vars), ] <- 0
  }

  method <- mice::make.method(comb)
  method[".Type"] <- ""
  if (!is.null(predicted_vars)) {
    method[setdiff(names(method), predicted_vars)] <- ""
  }
  method[intersect(cont_covs, names(method))] <- cont_method

  suppressWarnings(
    imp_data <- mice::mice(
      comb,
      m = m,
      printFlag = FALSE,
      predictorMatrix = pred,
      method = method,
      ...
    )
  )

  imp_data |>
    tidyr::complete(action = "long") |>
    dplyr::filter(.data$.Type == "Simulated")
}
