#' Get the data needed for model fitting and parse into format for easy parsing
#' with NCA tool.
#' 
#' @inheritParams get_data_for_modelfit
#' 
#' @returns data.frame
#'
#' @export
#' 
get_data_for_nca <- function(
    db = NULL
) {
  
  data <- read_data(
    tables = c("pc", "dm", "ex"), 
    db = db
  )
  
  # Parse tables for NCA:
  parse_data_for_nca(data)
  
}

