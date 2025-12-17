#' Reformat NCA-type analysis-ready dataset (ARD) into NONMEM-style modeling 
#' dataset
#'
#' @returns data.frame with population PK input data in NONMEM-style
#' format.
#' 
#' @param data dataset formatted as modeling-ready dataset
#' @param dictionary a data dictionary that maps expected variable names to 
#' variables in the data.
#' 
#' @export
#' 
reformat_data_modeling_to_nca <- function(
  data, 
  dictionary = NULL
) {
  ## TODO:
  # strip out EVID=2.
  # match EVID=1 events to concentrations and add as DOSE column
}