#' Sample covariates from multivariate normal distributions
#'
#' @param data data.frame (n x p) containing the original, observed,
#' time-invariant covariates (ID should not be included) that will be used to
#' inform the imputation.
#' @param cat_covs character vector containing the names of the categorical
#' covariates in orgCovs.
#' @param n_subjects number of simulated subjects, default is the number of
#' subjects in the data.
#' @param exponential sample from exponential distribution? Default `FALSE`.
#' @param conditional description...
#' @param ... additional arguments passed to `mvrnorm()` function
#'
#' @returns a data.frame with the simulated covariates, with `n_subjects`
#' rows and `p` columns
#'
#' @note missing values in `data` must be coded as NA
#'
#' @export
sample_covariates_mvtnorm <- function(
  data,
  cat_covs = NULL,
  n_subjects = nrow(data),
  exponential = FALSE,
  conditional = NULL,
  ...
) {
  
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
    means <- apply(data, 2, function(x) mean(log(x)))
    cov_mat <- stats::cov(log(data)) 
    out <- mvtnorm::rmvnorm(
      n_subjects, 
      mean = means,
      sigma = cov_mat
    ) |>
      exp() |>
      as.data.frame()
  } else {
    means <- apply(data, 2, mean)
    cov_mat <- stats::cov(data)
    out <- mvtnorm::rmvnorm(
      n_subjects, 
      mean = means,
      sigma = cov_mat
    ) |>
      as.data.frame()
  }
  
  if (tibble::is_tibble(data)) out <- tibble::as_tibble(out)
  out  
}
