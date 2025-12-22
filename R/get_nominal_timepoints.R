#' Get nominal timepoints based on vector of timepoints
#' 
#' @param t vector of timepoints
#' @param adjust sensitivity for peaks in the density curve to detect nominal
#' timepoints. A factor of 0.5 often works well for PK data.
#' @param ... optional arguments passed to `stats::density` function in addition
#' to `adjust`, e.g `bw`. The easiest way to adjust the sensitivity to peaks in 
#' the data is to use `adjust`. 
#' 
#' @returns a vector of approximate nominal timepoints estimated from the data
#' 
#' @export
#' 
get_nominal_timepoints <- function(
  t, 
  adjust = 0.5,
  ...
) {
  kernel <- stats::density(t, adjust = adjust, ...)
  peaks <- data.frame(time = kernel$x, y = kernel$y) |>
    dplyr::arrange(.data$time) |>
    dplyr::mutate( # find peaks (where dy/dt changes negative)
      delta = c(diff(.data$y), 0),
      delta_prv = c(0, .data$delta[-length(.data$delta)]),
      peak = .data$delta < 0 & .data$delta_prv >= 0
    ) |>
    dplyr::filter(.data$peak)

  t_nom <- c(floor(peaks$time[1:(nrow(peaks)-1)]), ceiling(utils::tail(peaks$time,1)))

  t_nom
}
