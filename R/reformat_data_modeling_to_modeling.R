#' Reformat modeling dataset into a properly checked and validated modeling 
#' dataset.
#' 
#' @param data dataset formatted as modeling-ready dataset
#' @param dictionary a data dictionary that maps expected variable names to
#' variables in the data.
#' @param categorical_mapping Either a character vector of column names to
#' auto-encode (most common value gets 0, next gets 1, etc.), or a data.frame
#' with columns `column`, `original_value`, `encoded_value` for explicit
#' mappings. NA values are encoded as -99. The final mapping is attached as a
#' `"categorical_mapping"` attribute on the returned data.frame. Default `NULL`
#' skips encoding.
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
  categorical_mapping = NULL,
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
  
  ## Apply categorical encoding
  data <- apply_categorical_mapping(data, categorical_mapping)
  cat_map <- attr(data, "categorical_mapping")

  ## Convert NA's to dots (or something else)
  if(!is.null(na)) {
    data <- data |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(is.na(.) | . == "NA", na, .)))
  }

  ## Preserve categorical mapping attribute (dplyr may strip it)
  if (!is.null(cat_map)) attr(data, "categorical_mapping") <- cat_map

  data
} 
