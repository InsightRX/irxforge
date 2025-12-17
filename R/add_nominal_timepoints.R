#' Add nominal timepoints to a dataset
#' 
#' @param data `data.frame` or `tibble` with at least a time column
#' @param time_var variable name for time vector in dataset
#' @param nominal_time_var variable name for new nominal time vector
#' @param ... passed as argument to `get_nominal_timepoints()` and
#' to `stats::density()`.
#' 
#' @returns data.frame or tibble
#'
#' @export
add_nominal_timepoints <- function(
    data,
    time_var = "time",
    nominal_time_var = "NOMINAL_TIME",
    verbose = FALSE,
    ...
) {
  t_nom <- get_nominal_timepoints(
    data[[time_var]], 
    ...
  )
  if(verbose) {
    message("Nominal times identified: ", paste0(t_nom, collapse=", "))
  }
  data[[nominal_time_var]] <- t_nom[match.closest(data[[time_var]], t_nom)]
  data
}

match.closest <- function(x, y) {
  cuts <- c(-Inf, y[-1] - diff(y)/2, Inf)
  idx <- findInterval(x, cuts)
}