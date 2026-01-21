#' Add nominal timepoints to a dataset
#' 
#' @param data `data.frame` or `tibble` with at least a time column
#' @param time_var variable name for time vector in dataset
#' @param nominal_time_var variable name for new nominal time vector
#' @param verbose Logical.
#' @param ... passed as argument to [get_nominal_timepoints()] and to
#'   `stats::density()`.
#' 
#' @returns data.frame or tibble
#' 
#' @seealso [get_nominal_timepoints()]
#'
#' @export
add_nominal_timepoints <- function(
  data,
  time_var = "time",
  nominal_time_var = "NOMINAL_TIME",
  verbose = FALSE,
  ...
) {
  rlang::check_dots_used()
  t_nom <- get_nominal_timepoints(data[[time_var]], ...)
  if(verbose) {
    message("Nominal times identified: ", paste0(t_nom, collapse=", "))
  }
  data[[nominal_time_var]] <- t_nom[match.closest(data[[time_var]], t_nom)]
  data
}
