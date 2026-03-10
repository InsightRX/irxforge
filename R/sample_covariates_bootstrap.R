#' Sample covariates using bootstrap
#'
#' @inheritParams sample_covariates_mice
#' @param rm.na logical. If `TRUE` (default), rows with `NA` in any column are
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
  seed = NULL,
  rm.na = TRUE,
  ...
) {
  if (!is.null(seed)) set.seed(seed)
  if (rm.na) {
    data <- data[stats::complete.cases(data), , drop = FALSE]
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
  if (nrow(data) == 0) {
    stop("No observations present within the conditional limits for the sampled population.")
  }
  dplyr::slice_sample(data, n = n_subjects, replace = TRUE)
}
