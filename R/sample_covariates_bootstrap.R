#' Sample covariates using bootstrap
#'
#' @inheritParams sample_covariates_mice
#' 
#' @returns a data.frame with the simulated covariates, with `n_subjects`
#' rows and `p` columns
#'
#' @export
#'
sample_covariates_bootstrap <- function(
    data,
    n_subjects = nrow(data),
    conditional = NULL,
    ...
) {
  
  if(!is.null(conditional)) {
    for(key in names(conditional)) {
      data <- data |>
        dplyr::filter(.data[[key]] >= min(conditional[[key]]) & .data[[key]] <= max(conditional[[key]])) 
    }
  }

  row_idx <- sample(
    x = 1:nrow(data), 
    size = n_subjects, 
    replace = TRUE
  )
  data[row_idx,]
}
