#' Sample covariates using bootstrap
#'
#' @inheritParams sample_covariates_mice
#' @param cat_covs character vector of categorical covariates. Used to filter
#'   categorical `conditional` limits by value and to exclude these columns from
#'   the multiplicative `noise` jitter.
#' @param noise optional single non-negative number giving the log-scale SD of
#'   multiplicative log-normal noise applied to the sampled continuous
#'   covariates (e.g. `0.05` for ~5%), to obscure the original values. `NULL`
#'   (default) applies no noise.
#' @param na.rm logical. If `TRUE` (default), rows with `NA` in any column are
#'   dropped before sampling.
#'
#' @returns a data.frame with the simulated covariates, with `n_subjects`
#' rows and `p` columns
#'
#' @export
sample_covariates_bootstrap <- function(
  data,
  n_subjects = nrow(data),
  conditional = NULL,
  cat_covs = NULL,
  noise = NULL,
  seed = NULL,
  na.rm = TRUE,
  ...
) {
  if (!is.null(seed)) set.seed(seed)
  if (na.rm) {
    data <- data[stats::complete.cases(data), , drop = FALSE]
  }
  if(!is.null(conditional)) {
    for(key in names(conditional)) {
      if (key %in% cat_covs) {
        data <- dplyr::filter(data, .data[[key]] %in% conditional[[key]])
      } else {
        data <- dplyr::filter(
          data,
          .data[[key]] >= min(conditional[[key]]) &
          .data[[key]] <= max(conditional[[key]])
        )
      }
    }
  }
  if (nrow(data) == 0) {
    stop("No observations present within the conditional limits for the sampled population.")
  }
  out <- dplyr::slice_sample(data, n = n_subjects, replace = TRUE)
  apply_covariate_noise(out, noise = noise, cat_covs = cat_covs)
}
