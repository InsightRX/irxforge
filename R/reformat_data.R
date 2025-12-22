#' Reformat data between various dataset layouts
#' 
#' @param data dataset or list of datasets
#' @param dictionary a data dictionary that maps expected variable names to 
#' variables in the data.
#' @param input_type type of input dataset. Can be one of `nca`, `modeling`, or
#' `sdtm`. The types are defined as follows:
#' - `nca`: a dataset with one row for every observed concentration measurement. 
#' Time, concentration value, and administered dose are required columns.
#' The dataset may contain any other variables, covariates, groupings etc in 
#' columns.
#' - `modeling`: a dataset with dose events (evid = 1), concentrations (evid = 
#' 0), and potentially other events (events = 2) in separate rows. Dataset 
#' requires columns for event type, time, dose amount, measured concentration.
#' The dataset may contain any other variables, covariates, groupings etc in 
#' columns.
#' - `sdtm`: a list of various datasets or "domains", such as `ADSL` `DM`, `EX`, and 
#' `DS`,  following the SDTM structure and nomenclature.
#' @param output_type type of output dataset. Can be either `nca` or `modeling`.
#' @param ... passed onto specific reformatting functions:
#' `reformat_data_nca_to_modeling`, `` 
#' 
#' @returns a data.frame in either `nca` or `modeling` format
#' 
#' @export
#' 
reformat_data <- function(
  data,
  dictionary = NULL,
  input_type = c("auto", "nca", "modeling", "sdtm"),
  output_type = c("nca", "modeling"),
  ...
) {
  input_type <- rlang::arg_match(input_type)
  output_type <- rlang::arg_match(output_type)

  ## If needed, try to detect type for input data
  if (is.null(input_type) || input_type == "auto") {
    input_type <- detect_dataset_type(data)
  }
  
  ## Reformat SDTM datasets
  if (input_type == "sdtm") {
    if (output_type != "modeling") {
      not_supported_reformat(input_type, output_type)
    }
    new_data <- reformat_data_sdtm_to_modeling(data, dictionary, ...)
  } else if(input_type == "nca") {
    if (output_type != "modeling") {
      not_supported_reformat(input_type, output_type)
    }
    new_data <- reformat_data_nca_to_modeling(data, dictionary, ...)
  } else if(input_type == "modeling") {
    if (output_type == "modeling") {
      new_data <- reformat_data_modeling_to_modeling(data, dictionary, ...)
    } else if (output_type == "nca") {
      new_data <- reformat_data_modeling_to_nca(data, dictionary, ...)
    } else {
      not_supported_reformat(input_type, output_type, ...)
    }
  } else {
    stop("`input_type` not recognized.")
  }
  
  new_data
}

#' Function that throws an informative error if reformatting is not supported.
#' 
#' @param input_type type of input
#' @param output_type type of output
#' 
not_supported_reformat <- function(input_type, output_type) {
  stop("Sorry, conversion from", input_type, "to", output_type, "is not supported yet.")
}

#' Auto-detect dataset type 
#' 
#' @param data a data.frame
#' 
detect_dataset_type <- function(data) {
  ## TODO: Fairly crude auto-detection, probably OK for 90% of dataset, but 
  ## should add more advanced checks.
  if (inherits(data, "list")) {
    return("sdtm")
  }
  if (!inherits(data, "data.frame")) {
    stop("Unsupported data type, expecting either a list or a data.frame.")    
  }
  if ("evid" %in% tolower(names(data))) {
    return("modeling")
  } else {
    return("nca")
  }
}

