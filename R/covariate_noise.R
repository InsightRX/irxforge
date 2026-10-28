#' Apply multiplicative noise to continuous covariates
#'
#' Obscures sampled continuous covariate values by multiplying each by an
#' independent log-normal factor `exp(rnorm(0, noise))`, i.e. roughly `noise`
#' fractional (CV) jitter. Multiplicative noise preserves the sign and keeps
#' positive covariates (weight, creatinine, ...) positive. Categorical
#' covariates and non-numeric columns are left untouched.
#'
#' @param data data.frame of sampled covariates.
#' @param noise single non-negative number giving the log-scale SD of the
#'   multiplicative noise (e.g. `0.05` for ~5%). `NULL` or `0` returns `data`
#'   unchanged.
#' @param cat_covs character vector of categorical covariates to exclude from
#'   the jitter.
#' @returns `data` with continuous covariates jittered.
#' @noRd
apply_covariate_noise <- function(data, noise, cat_covs = NULL) {
  if (is.null(noise)) {
    return(data)
  }
  if (!is.numeric(noise) || length(noise) != 1 || is.na(noise) || noise < 0) {
    stop("`noise` must be a single non-negative number.", call. = FALSE)
  }
  if (noise == 0) {
    return(data)
  }
  cont_covs <- setdiff(names(data), cat_covs)
  cont_covs <- cont_covs[vapply(data[cont_covs], is.numeric, logical(1))]
  for (cov in cont_covs) {
    x <- data[[cov]]
    data[[cov]] <- x * exp(stats::rnorm(length(x), mean = 0, sd = noise))
  }
  data
}

#' Sample baseline covariates for the time-varying samplers
#'
#' Shared baseline draw used by both [sample_covariates_mice_timevarying()] and
#' [sample_covariates_lme_timevarying()] so the two cannot drift. `"mice"`
#' generates baselines via chained equations (the historical default).
#' `"bootstrap"` resamples observed baseline rows with replacement, which
#' preserves the observed joint distribution of the baseline covariates instead
#' of shrinking it toward the multivariate mean as full-row MICE imputation
#' does. Output-level `noise` is applied separately to the assembled
#' trajectories by [apply_timevarying_noise()], not here.
#'
#' @noRd
sample_tv_baseline <- function(
  baseline_data,
  baseline_method,
  cat_covs,
  conditional,
  n_subjects,
  cont_method,
  ...
) {
  if (baseline_method == "bootstrap") {
    out <- sample_covariates_bootstrap(
      data = baseline_data,
      n_subjects = n_subjects,
      conditional = conditional,
      cat_covs = cat_covs,
      noise = NULL,
      seed = NULL,
      na.rm = TRUE
    )
    out <- as.data.frame(out)
    rownames(out) <- NULL
    for (cov in cat_covs) {
      out[[cov]] <- as.factor(out[[cov]])
    }
    return(out)
  }
  sample_covariates_mice(
    data = baseline_data,
    cat_covs = cat_covs,
    conditional = conditional,
    n_subjects = n_subjects,
    cont_method = cont_method,
    replicates = 1,
    seed = NULL,
    ...
  )
}

#' Apply multiplicative noise to a long-format simulated trajectory dataset
#'
#' Obscures the simulated output values. Each continuous time-varying covariate
#' is jittered independently at every timepoint (measurement-like scatter), while
#' each continuous static covariate is jittered once per subject so it stays
#' constant over time. Categorical and non-numeric columns are left untouched.
#'
#' @noRd
apply_timevarying_noise <- function(
  data,
  id_var,
  static_covs,
  time_varying_covs,
  noise
) {
  if (is.null(noise)) {
    return(data)
  }
  if (!is.numeric(noise) || length(noise) != 1 || is.na(noise) || noise < 0) {
    stop("`noise` must be a single non-negative number.", call. = FALSE)
  }
  if (noise == 0) {
    return(data)
  }

  ids <- as.character(data[[id_var]])
  for (cov in intersect(static_covs, names(data))) {
    if (!is.numeric(data[[cov]])) next
    uid <- unique(ids)
    factor_by_id <- stats::setNames(
      exp(stats::rnorm(length(uid), mean = 0, sd = noise)),
      uid
    )
    data[[cov]] <- data[[cov]] * factor_by_id[ids]
  }
  for (cov in intersect(time_varying_covs, names(data))) {
    if (!is.numeric(data[[cov]])) next
    data[[cov]] <- data[[cov]] * exp(stats::rnorm(nrow(data), mean = 0, sd = noise))
  }
  data
}
