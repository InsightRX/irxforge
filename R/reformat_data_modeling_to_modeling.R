#' Reformat modeling dataset into a properly checked and validated modeling 
#' dataset.
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
reformat_data_modeling_to_modeling <- function(
    data, 
    dictionary = NULL
) {
  
  data <- data %>%
    setNames(toupper(names(data))) %>%     # names should be upper-case by default in NONMEM
    mutate(DV = ifelse(EVID == 0, DV, 0))  # make sure there are no DV=x when there shouldn't be

  ## Create MDV column if it doesn't exist
  if(is.null(data$MDV)) {
    data <- data %>%
      dplyr::mutate(MDV = ifelse(EVID == 0, 0, 1))
  }
  
  ## Make sure the GROUP variable exists
  if(is.null(data$group)) {
    if(!is.null(dictionary$group)) {
      data$GROUP <- data[[dictionary$group]]
    } else {
      data$GROUP <- 1 # dummy grouper
    }
  }

  data
  
} 