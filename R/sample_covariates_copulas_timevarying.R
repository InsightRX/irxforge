#' Sample time-varying covariates using copula modeling
#'
#' Implements the copula-based virtual-patient simulation of Zwep et al. (2024)
#' for time-varying covariates. Each continuous time-varying covariate trajectory
#' is summarised by the per-subject coefficients of a random-effects polynomial
#' regression on time; a vine copula is then fitted to the joint distribution of
#' those coefficients together with the (continuous) static covariates. New
#' virtual subjects are drawn from the copula and their trajectories are
#' reconstructed by evaluating the sampled polynomial coefficients over a time
#' grid. Modelling dependence in coefficient space, with nonparametric (kernel)
#' marginals, reproduces the realistic covariate domain and the full correlation
#' structure without resampling or extrapolating individual observed values.
#'
#' @param data long-format `data.frame` or `tibble` containing one row per
#'   subject and time.
#' @param id_var subject identifier column name.
#' @param time_var time column name. Must be numeric.
#' @param time_grid optional numeric vector of simulated times. If supplied,
#'   every simulated subject's trajectory is reconstructed on this grid. If
#'   `NULL` (default), each simulated subject clones an observed subject's
#'   observation-time vector, assigned deterministically in observed subject
#'   order and recycled if `n_subjects` exceeds the number of observed subjects.
#' @param static_covs character vector of continuous covariates that are
#'   constant within a subject. Sampled jointly with the trajectory coefficients
#'   and carried forward unchanged.
#' @param time_varying_covs character vector of continuous covariates to model
#'   over time. Default is all columns except `id_var`, `time_var`, and
#'   `static_covs`.
#' @param degree degree of the per-subject polynomial in `time_var` used to
#'   summarise each time-varying covariate trajectory. Default `1` (a
#'   subject-specific intercept and slope). Higher degrees capture curvature but
#'   require more timepoints per subject.
#' @param family_set copula families considered by `rvinecopulib::vine()`.
#'   Default `"all"`.
#' @param selcrit model-selection criterion for the vine copula, default
#'   `"aic"`. Use `"loglik"` for the tightest in-sample reproduction.
#' @param bw_mult bandwidth multiplier for the kernel-density marginals passed to
#'   `rvinecopulib::vine()` (`margins_controls$mult`). Default `1`. Values below
#'   `1` reduce marginal smoothing, tightening the simulated marginals toward the
#'   observed (empirical) distribution; this is the main lever for matching the
#'   original covariate distributions most closely. Very small values risk
#'   overfitting the observed sample.
#' @param noise optional single non-negative number giving the log-scale SD of
#'   multiplicative log-normal noise (e.g. `0.05` for ~5%) applied to the
#'   reconstructed output values to obscure the generated data. `NULL` (default)
#'   applies no noise. Time-varying covariates are jittered independently at each
#'   timepoint (adding measurement-like scatter around the trajectory); static
#'   covariates are jittered once per subject so they stay constant over time.
#'   Applied before `truncate`.
#' @param truncate logical; if `TRUE` (default) reconstructed time-varying
#'   covariate values are clamped to the observed `[min, max]` range of each
#'   covariate, preventing nonphysical extrapolated values.
#' @param design_id_var optional column name for retaining the observed subject
#'   ID whose observation-time design was cloned for each simulated subject.
#'   Only usable when `time_grid = NULL`. Default `NULL` omits this column.
#' @param n_subjects number of simulated subjects. Default is the number of
#'   unique observed subjects.
#' @param replicates number of independent simulated datasets to sample.
#'   Default is 1. When greater than 1, a `.replicate` column is included.
#' @param seed integer random seed passed to [set.seed()] for reproducibility.
#'   Default `NULL` does not set a seed.
#' @param ... reserved for future use.
#'
#' @details
#' The algorithm follows Zwep et al. (2024):
#' 1. For each time-varying covariate `X_j`, fit a random-effects polynomial
#'    regression `X_j ~ poly(time, degree) + (poly(time, degree) | id)` and
#'    extract the per-subject coefficient vector. If the full random-slope model
#'    fails to converge, a random-intercept model is used and the
#'    population-level slope coefficients are shared across subjects.
#' 2. Assemble a subject-level table of all trajectory coefficients and the
#'    static covariates, and fit a vine copula with kernel-density marginals via
#'    `rvinecopulib::vine()`.
#' 3. Draw `n_subjects` coefficient vectors from the copula and reconstruct each
#'    trajectory by evaluating its polynomial over the requested times.
#'
#' Only continuous covariates are supported; categorical covariates should be
#' handled with [sample_covariates_mice_timevarying()].
#'
#' Requires the `rvinecopulib` package.
#'
#' @returns long-format data.frame with simulated subject IDs, time, static
#'   covariates, and time-varying covariates.
#'
#' @references
#' Zwep LB, Guo T, Nagler T, Knibbe CAJ, Meulman JJ, van Hasselt JGC. Virtual
#' Patient Simulation Using Copula Modeling. Clin Pharmacol Ther.
#' 2024;115(4):795-804. \doi{10.1002/cpt.3099}
#'
#' @seealso [sample_covariates_mice_timevarying()] and
#'   [sample_covariates_lme_timevarying()] for the resampling and mixed-effects
#'   alternatives.
#'
#' @export
sample_covariates_copulas_timevarying <- function(
  data,
  id_var = "ID",
  time_var = "TIME",
  time_grid = NULL,
  static_covs = NULL,
  time_varying_covs = NULL,
  degree = 1,
  family_set = "all",
  selcrit = "aic",
  bw_mult = 1,
  noise = NULL,
  truncate = TRUE,
  design_id_var = NULL,
  n_subjects = length(unique(data[[id_var]])),
  replicates = 1,
  seed = NULL,
  ...
) {
  if (!requireNamespace("rvinecopulib", quietly = TRUE)) {
    stop(
      "Package `rvinecopulib` is required for ",
      "`sample_covariates_copulas_timevarying()`.",
      call. = FALSE
    )
  }
  if (!is.null(seed)) set.seed(seed)

  if (!id_var %in% names(data)) {
    stop("`id_var` was not found in `data`.", call. = FALSE)
  }
  if (!time_var %in% names(data)) {
    stop("`time_var` was not found in `data`.", call. = FALSE)
  }
  if (!is.numeric(data[[time_var]])) {
    stop("`time_var` must be numeric.", call. = FALSE)
  }
  if (length(degree) != 1 || degree < 1 || degree != round(degree)) {
    stop("`degree` must be a positive integer.", call. = FALSE)
  }
  if (length(bw_mult) != 1 || !is.numeric(bw_mult) || is.na(bw_mult) || bw_mult <= 0) {
    stop("`bw_mult` must be a single positive number.", call. = FALSE)
  }
  if (!is.null(noise) &&
      (!is.numeric(noise) || length(noise) != 1 || is.na(noise) || noise < 0)) {
    stop("`noise` must be a single non-negative number.", call. = FALSE)
  }
  if (replicates < 1) {
    stop("`replicates` must be at least 1.", call. = FALSE)
  }
  if (n_subjects < 1) {
    stop("`n_subjects` must be at least 1.", call. = FALSE)
  }
  if (!is.null(time_grid)) {
    time_grid <- sort(unique(time_grid))
    if (length(time_grid) == 0) {
      stop("`time_grid` must contain at least one timepoint.", call. = FALSE)
    }
  }
  if (!is.null(time_grid) && !is.null(design_id_var)) {
    stop(
      "`design_id_var` can only be used when `time_grid = NULL`.",
      call. = FALSE
    )
  }

  if (is.null(static_covs)) static_covs <- character()
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
  non_numeric <- covs[!vapply(data[covs], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop(
      "Only continuous covariates are supported; these are not numeric: ",
      paste(non_numeric, collapse = ", "),
      ". Use `sample_covariates_mice_timevarying()` for categorical covariates.",
      call. = FALSE
    )
  }
  if (!is.null(design_id_var) && design_id_var %in% c(id_var, time_var, covs)) {
    stop(
      "`design_id_var` must not match `id_var`, `time_var`, or a covariate name.",
      call. = FALSE
    )
  }

  data <- data |>
    dplyr::arrange(.data[[id_var]], .data[[time_var]])

  # Observed designs (time vectors per subject) for the clone path, and the
  # observed range of each time-varying covariate for optional truncation.
  observed_times <- lapply(
    split(data[[time_var]], data[[id_var]]),
    function(x) sort(x)
  )
  observed_ranges <- lapply(
    time_varying_covs,
    function(cov) range(data[[cov]], na.rm = TRUE)
  )
  names(observed_ranges) <- time_varying_covs

  subject_table <- make_copula_subject_table(
    data = data,
    id_var = id_var,
    time_var = time_var,
    static_covs = static_covs,
    time_varying_covs = time_varying_covs,
    degree = degree
  )

  if (length(subject_table$varying_cols) == 0) {
    stop(
      "No between-subject variation in the trajectory coefficients to model.",
      call. = FALSE
    )
  }
  copula <- fit_copula_model(
    varying = subject_table$table[, subject_table$varying_cols, drop = FALSE],
    family_set = family_set,
    selcrit = selcrit,
    bw_mult = bw_mult
  )

  out <- vector("list", replicates)
  for (replicate_idx in seq_len(replicates)) {
    sampled <- simulate_copula_subjects(
      copula = copula,
      varying_cols = subject_table$varying_cols,
      fixed_values = subject_table$fixed_values,
      n_subjects = n_subjects
    )

    design_assignment <- if (is.null(time_grid)) {
      rep(seq_along(observed_times), length.out = n_subjects)
    } else {
      NULL
    }
    subject_times <- if (is.null(time_grid)) {
      observed_times[design_assignment]
    } else {
      rep(list(time_grid), n_subjects)
    }
    design_ids <- if (!is.null(design_id_var) && !is.null(design_assignment)) {
      names(observed_times)[design_assignment]
    } else {
      NULL
    }

    out[[replicate_idx]] <- reconstruct_copula_trajectories(
      sampled = sampled,
      subject_times = subject_times,
      static_covs = static_covs,
      time_varying_covs = time_varying_covs,
      coef_cols = subject_table$coef_cols,
      degree = degree,
      id_var = id_var,
      time_var = time_var,
      design_id_var = design_id_var,
      design_ids = design_ids,
      noise = noise,
      truncate = truncate,
      observed_ranges = observed_ranges
    )
    if (replicates > 1) {
      out[[replicate_idx]][[".replicate"]] <- replicate_idx
    }
  }

  out <- dplyr::bind_rows(out)
  if (tibble::is_tibble(data)) out <- tibble::as_tibble(out)
  out
}

#' Build the subject-level coefficient table for the copula sampler
#'
#' Returns the per-subject design matrix combining static covariates and the
#' fitted polynomial coefficients of every time-varying covariate, together with
#' the names of the columns that actually vary between subjects (and so enter the
#' copula) and the constant value of any columns that do not.
#'
#' @noRd
make_copula_subject_table <- function(
  data,
  id_var,
  time_var,
  static_covs,
  time_varying_covs,
  degree
) {
  ids <- sort(unique(data[[id_var]]))

  static_table <- NULL
  if (length(static_covs) > 0) {
    splits <- split(data, data[[id_var]])
    static_table <- do.call(
      rbind,
      lapply(splits, function(profile) profile[1, static_covs, drop = FALSE])
    )
    rownames(static_table) <- names(splits)
    # `split()` orders groups by the character form of the ID, which can differ
    # from the numeric order of `ids`; realign by name.
    static_table <- static_table[match(as.character(ids), rownames(static_table)), , drop = FALSE]
  }

  coef_cols <- stats::setNames(vector("list", length(time_varying_covs)), time_varying_covs)
  coef_blocks <- vector("list", length(time_varying_covs))
  for (j in seq_along(time_varying_covs)) {
    cov <- time_varying_covs[[j]]
    block <- fit_copula_poly_coefficients(
      data = data,
      cov = cov,
      id_var = id_var,
      time_var = time_var,
      degree = degree,
      ids = ids
    )
    coef_cols[[cov]] <- colnames(block)
    coef_blocks[[j]] <- block
  }

  table <- do.call(cbind, coef_blocks)
  if (!is.null(static_table)) {
    table <- cbind(as.data.frame(static_table), table)
  }
  table <- as.data.frame(table)
  rownames(table) <- ids

  # Columns with effectively no between-subject variation (e.g. shared
  # population slopes when a random-intercept fallback was used, or a static
  # covariate that is constant across the cohort) cannot be modelled by the
  # copula's kernel marginals. Hold them fixed and re-inject at simulation time.
  spread <- vapply(table, function(x) stats::sd(x, na.rm = TRUE), numeric(1))
  scale <- pmax(abs(vapply(table, function(x) mean(x, na.rm = TRUE), numeric(1))), 1)
  varying_cols <- names(table)[spread > 1e-8 * scale]
  fixed_cols <- setdiff(names(table), varying_cols)
  fixed_values <- vapply(table[fixed_cols], function(x) mean(x, na.rm = TRUE), numeric(1))

  list(
    table = table,
    varying_cols = varying_cols,
    fixed_values = fixed_values,
    coef_cols = coef_cols
  )
}

#' Fit per-subject polynomial coefficients for one time-varying covariate
#'
#' Tries a random-slope polynomial mixed model first, falling back to a
#' random-intercept model (shared population slopes) if it fails to converge.
#' Returns a subjects x (degree + 1) matrix of coefficients ordered by `ids`.
#'
#' @noRd
fit_copula_poly_coefficients <- function(
  data,
  cov,
  id_var,
  time_var,
  degree,
  ids
) {
  fixed <- stats::as.formula(
    sprintf("%s ~ poly(%s, %d, raw = TRUE)", cov, time_var, degree)
  )
  random_slope <- stats::as.formula(
    sprintf("~ poly(%s, %d, raw = TRUE) | %s", time_var, degree, id_var)
  )
  random_intercept <- stats::as.formula(sprintf("~ 1 | %s", id_var))

  ctrl <- nlme::lmeControl(opt = "optim", returnObject = TRUE)
  fit <- tryCatch(
    suppressWarnings(nlme::lme(
      fixed = fixed, random = random_slope, data = data,
      na.action = stats::na.omit, control = ctrl
    )),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    fit <- suppressWarnings(nlme::lme(
      fixed = fixed, random = random_intercept, data = data,
      na.action = stats::na.omit, control = ctrl
    ))
  }

  cf <- stats::coef(fit)
  cf <- cf[match(as.character(ids), rownames(cf)), , drop = FALSE]
  colnames(cf) <- paste0(".", cov, "_b", seq_len(ncol(cf)) - 1L)
  as.matrix(cf)
}

#' Fit a vine copula to the (already-selected) varying subject-level columns
#'
#' @noRd
fit_copula_model <- function(varying, family_set, selcrit, bw_mult = 1) {
  rvinecopulib::vine(
    varying,
    margins_controls = list(mult = bw_mult),
    copula_controls = list(family_set = family_set, selcrit = selcrit)
  )
}

#' Draw new subjects from the copula and re-attach the fixed columns
#'
#' @noRd
simulate_copula_subjects <- function(copula, varying_cols, fixed_values, n_subjects) {
  sampled <- as.data.frame(rvinecopulib::rvine(n_subjects, copula))
  names(sampled) <- varying_cols
  for (col in names(fixed_values)) {
    sampled[[col]] <- fixed_values[[col]]
  }
  sampled
}

#' Reconstruct long-format trajectories from sampled polynomial coefficients
#'
#' @noRd
reconstruct_copula_trajectories <- function(
  sampled,
  subject_times,
  static_covs,
  time_varying_covs,
  coef_cols,
  degree,
  id_var,
  time_var,
  design_id_var = NULL,
  design_ids = NULL,
  noise,
  truncate,
  observed_ranges
) {
  powers <- 0:degree
  jitter <- !is.null(noise) && noise > 0
  rows <- vector("list", nrow(sampled))
  for (i in seq_len(nrow(sampled))) {
    times <- subject_times[[i]]
    n_t <- length(times)
    block <- data.frame(.id = i, .time = times)
    names(block) <- c(id_var, time_var)
    if (!is.null(design_id_var) && !is.null(design_ids)) {
      block[[design_id_var]] <- as.character(design_ids[[i]])
    }

    for (cov in static_covs) {
      value <- sampled[[cov]][[i]]
      # Static covariate: one jitter draw reused across the subject's rows so it
      # stays constant over time.
      if (jitter) value <- value * exp(stats::rnorm(1, 0, noise))
      block[[cov]] <- rep(value, n_t)
    }
    for (cov in time_varying_covs) {
      b <- as.numeric(sampled[i, coef_cols[[cov]]])
      design <- outer(times, powers, `^`)
      values <- as.numeric(design %*% b)
      # Time-varying covariate: independent jitter at each timepoint.
      if (jitter) values <- values * exp(stats::rnorm(n_t, 0, noise))
      if (truncate) {
        rng <- observed_ranges[[cov]]
        values <- pmin(pmax(values, rng[[1]]), rng[[2]])
      }
      block[[cov]] <- values
    }
    rows[[i]] <- block
  }
  dplyr::bind_rows(rows)
}
