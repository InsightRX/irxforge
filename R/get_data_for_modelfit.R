#' Get the data needed for model fitting and parse into NONMEM style
#' format
#' 
#' @param db list with db connection information. If NULL, will read
#' data from example admiral dataset.
#' 
#' @returns NONMEM style dataset as data.frame
#'
#' @export
#' 
get_data_for_modelfit <- function(
  db = NULL
) {
  
  data <- read_data(
    tables = c("pc", "adsl", "ex", "lb", "vs"), 
    db = db
  )

  # parse data into NONMEM-style and return
  parse_data_for_modelfit(data)
  
}

