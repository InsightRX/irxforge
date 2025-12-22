#' Sample covariates using a variety of methods
#' 
#' @param method sampling method, one of `mvtnorm`, `bootstrap`, or `mice`.
#' E.g. `list(AGE = c(60, 80), WT = c(70, 100))`.
#' 
#' @param ... arguments passed to lower-level function(s).
#' 
#' @returns data.frame with covariates in each column
#'
#' @export
sample_covariates <- function(
  method = c("mvtnorm", "mice", "bootstrap"),
  ...
) {
  method <- match.arg(method)
  do.call(paste0("sample_covariates_", method), args = list(...))
}
