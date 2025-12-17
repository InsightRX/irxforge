#' Get the route of administration from a vector of routes for doses
#' 
#' It is assumed that all routes are the same for all patients and drug 
#' administrations. 
#' 
#' @param x vector of routes, e.g. from EXROUTE columns in EX dataset,
#' or ROUTE column in output dataset from `get_data_for_modeling()`. 
#' 
#' @returns route (character), either "iv", "oral", "sc", or "im"
#' 
#' @export
#' 
get_route_from_data_column <- function(
    x
  ) {
  x <- x[!is.na(x)]
  if(length(unique(x)) > 1) {
    warning("Multiple routes in clinical trial database, this is not yet supported. Analysis may be incorrect.")
  }
  route <- x[1]
  switch(
    tolower(route),
    oral = "oral",
    subcutaneous = "sc",
    sc = "sc",
    intramuscular = "im",
    im = "im",
    intravenous = "iv",
    iv = "iv",
    "oral" 
  )
}