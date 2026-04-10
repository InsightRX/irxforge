#' Reformat modeling dataset into a properly checked and validated modeling 
#' dataset.
#' 
#' @param data dataset formatted as modeling-ready dataset
#' @param dictionary a data dictionary that maps expected variable names to 
#' variables in the data.
#' @param na what to set NA values to. E.g. ".", (default) or NA (keep NA),
#' or NULL (do nothing).
#' 
#' @returns data.frame with population PK input data in NONMEM-style
#' format.
#' 
#' @export
reformat_data_modeling_to_modeling <- function(
  data,
  dictionary = NULL,
  na = "."
) {
  
  data <- data |>
    dplyr::rename_with(toupper) |> # names should be upper-case by default in NONMEM
    dplyr::mutate(DV = ifelse(.data$EVID == 0, .data$DV, 0))  # make sure there are no DV=x when there shouldn't be

  ## Create MDV column if it doesn't exist
  if(is.null(data$MDV)) {
    data <- dplyr::mutate(data, MDV = ifelse(.data$EVID == 0, 0, 1))
  }
  
  ## Make sure the GROUP variable exists
  if(is.null(data$GROUP)) {
    # TODO: Since dictionary is only used for the group variable, it might be
    # better to simplify the argument to just take a string indicating the
    # grouping column.
    if(!is.null(dictionary$group)) {
      data$GROUP <- data[[dictionary$group]]
    } else {
      data$GROUP <- 1 # dummy grouper
    }
  }
  
  ## Convert NA's to dots (or something else)
  if(!is.null(na)) {
    data <- data |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(is.na(.) | . == "NA", na, .)))
  }
  
  data
} 
