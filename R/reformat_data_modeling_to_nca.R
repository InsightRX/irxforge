#' Reformat NCA-type analysis-ready dataset (ARD) into NONMEM-style modeling 
#' dataset
#' 
#' @param data dataset formatted as modeling-ready dataset
#' @param dictionary a data dictionary that maps expected variable names to 
#' variables in the data.
#' @param na what to set NA values to. E.g. ".", or NA (keep NA, default),
#' or NULL (do nothing).
#' 
#' @returns data.frame with population PK input data in NONMEM-style
#' format.
#' 
#' @export
reformat_data_modeling_to_nca <- function(
  data, 
  dictionary = NULL,
  na = NA
) {
  ## TODO:
  # strip out EVID=2.
  # match EVID=1 events to concentrations and add as DOSE column
}
