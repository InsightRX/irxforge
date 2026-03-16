#' Sample covariates from multivariate normal distributions
#'
#' Samples from a multivariate normal distribution either derived from observed
#' data or specified directly via `means` plus a covariance matrix (`sigma`) or
#' standard deviations (`sd`).
#'
#' @param data data.frame (n x p) containing the original, observed,
#' time-invariant covariates (ID should not be included) that will be used to
#' inform the imputation. Can be `NULL` when `means` and either `sigma` or
#' `sd` are supplied directly. Ignored with a warning when `means` is also
#' provided.
#' @param means named numeric vector of means for each covariate. When supplied,
#' the distribution is specified directly and `data` is ignored. Must be
#' supplied together with either `sigma` or `sd` when `data` is `NULL`.
#' @param sigma named numeric matrix (p x p) giving the full covariance matrix.
#' Takes precedence over `sd` when both are provided. Column and row names
#' must match the names in `means`.
#' @param sd named numeric vector of standard deviations. Used to construct a
#' diagonal covariance matrix (`diag(sd^2)`) when `sigma` is not provided.
#' Names must match the names in `means`.
#' @param cat_covs character vector containing the names of the categorical
#' covariates in orgCovs.
#' @param n_subjects number of simulated subjects. Defaults to `nrow(data)`
#' when `data` is provided; required (no default) when `data` is `NULL`.
#' @param exponential sample from exponential distribution? Default `FALSE`.
#' Only applies when means/covariance are derived from `data`.
#' @param conditional description...
#' @param seed integer random seed passed to [set.seed()] for reproducibility.
#' Default `NULL` does not set a seed.
#' @param ... additional arguments passed to `mvrnorm()` function
#'
#' @returns a data.frame with the simulated covariates, with `n_subjects`
#' rows and `p` columns
#'
#' @note missing values in `data` must be coded as NA
#'
#' @export
sample_covariates_mvtnorm <- function(
  data = NULL,
  means = NULL,
  sigma = NULL,
  sd = NULL,
  cat_covs = NULL,
  n_subjects = if (!is.null(data)) nrow(data) else stop("`n_subjects` must be specified when `data` is NULL."),
  exponential = FALSE,
  conditional = NULL,
  seed = NULL,
  ...
) {
  if (!is.null(seed)) set.seed(seed)

  ## Branch: distribution specified directly via means + sigma/sd
  if (!is.null(means)) {
    if (!is.null(data)) {
      warning("`data` is ignored when `means` is provided.")
    }
    if (is.null(sigma) && is.null(sd)) {
      stop("When `means` is supplied, either `sigma` or `sd` must also be provided.")
    }
    if (!is.null(sigma)) {
      cov_mat <- sigma
    } else {
      if (length(sd) != length(means)) {
        stop("`sd` must have the same length as `means`.")
      }
      cov_mat <- diag(sd^2)
      if (!is.null(names(sd))) {
        rownames(cov_mat) <- names(sd)
        colnames(cov_mat) <- names(sd)
      } else if (!is.null(names(means))) {
        rownames(cov_mat) <- names(means)
        colnames(cov_mat) <- names(means)
      }
    }
    out <- mvtnorm::rmvnorm(
      n_subjects,
      mean = means,
      sigma = cov_mat,
      ...
    ) |>
      as.data.frame()
    if (!is.null(names(means))) names(out) <- names(means)
    return(out)
  }

  ## Branch: derive distribution from data
  if (is.null(data)) {
    stop("Either `data` or `means` with `sigma`/`sd` must be provided.")
  }

  if(!is.null(conditional)) {
    for(key in names(conditional)) {
      data <- dplyr::filter(
        data,
        .data[[key]] >= min(conditional[[key]]) &
        .data[[key]] <= max(conditional[[key]])
      )
    }
  }

  # names of continuous covariates
  # FIXME: This code does nothing currently... is this function intended to
  # work with categorical covariates? or only continuous? If the latter, how
  # do we handle categorical?
  cont_covs <- setdiff(names(data), cat_covs)
  miss_vars <- names(data)[colSums(is.na(data)) > 0]

  ## Get distribution and sample
  if(exponential) {
    # FIXME: This fails if there are zeroes or negative numbers. Should add some
    # safety rails.
    data_means <- apply(data, 2, function(x) mean(log(x)))
    cov_mat <- stats::cov(log(data))
    out <- mvtnorm::rmvnorm(
      n_subjects,
      mean = data_means,
      sigma = cov_mat
    ) |>
      exp() |>
      as.data.frame()
  } else {
    data_means <- apply(data, 2, mean)
    cov_mat <- stats::cov(data)
    out <- mvtnorm::rmvnorm(
      n_subjects,
      mean = data_means,
      sigma = cov_mat
    ) |>
      as.data.frame()
  }

  if (tibble::is_tibble(data)) out <- tibble::as_tibble(out)
  out
}
