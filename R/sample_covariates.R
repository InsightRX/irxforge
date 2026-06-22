#' Sample covariates using a variety of methods
#' 
#' Categorical covariates: all covariate sampling methods (except multi-variate
#' normal) support sampling of categorical covariates as well as continuous. 
#' In the `mice` sampling method, the categorical covariates have to be
#' provided specifically as a vector of `character` indicating the column names.
#' If not provided, they will otherwise be treated as continuous variables and
#' non-integer values may be sampled.
#'
#' @param method sampling method, one of `mvtnorm`, `bootstrap`, `mice`,
#' `mice_timevarying`, or `nhanes`. E.g.
#' `list(AGE = c(60, 80), WT = c(70, 100))`.
#' @param seed integer random seed passed to [set.seed()] for reproducibility.
#' Default `NULL` does not set a seed.
#' @param ... arguments passed to lower-level function(s).
#'
#' @returns data.frame with covariates in each column
#'
#' @export
sample_covariates <- function(
  method = c("mvtnorm", "mice", "mice_timevarying", "bootstrap", "nhanes"),
  seed = NULL,
  ...
) {
  method <- rlang::arg_match(method)
  do.call(paste0("sample_covariates_", method), args = list(seed = seed, ...))
}
