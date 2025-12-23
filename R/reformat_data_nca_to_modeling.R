#' Reformat NCA-type analysis-ready dataset (ARD) into a NONMEM-style modeling 
#' dataset
#'
#' @returns data.frame with population PK input data in NONMEM-style
#' format.
#' 
#' @param data dataset formatted as NCA analysis-ready dataset
#' @param dictionary a data dictionary that maps expected variable names to 
#' variables in the data.
#' @param dose_compartment the compartment in which doses are entered
#' @param obs_compartment the observation compartment number
#' @param covariates a vector of covariate names that are to be extracted
#' and added to the modeling dataset.
#'
#' @export
#' 
reformat_data_nca_to_modeling <- function(
  data, 
  dictionary = list(
    subject_id = "ID",
    group = "GROUP",
    time = "TIME",
    dose = "AMT",
    conc = "DV"
  ),
  dose_compartment = 1,
  obs_compartment = 1,
  covariates = NULL
) {
  
  groups <- c(dictionary$subject_id, dictionary$group)
  
  ## Check if GROUP is available
  ## If not available, create it for consistency
  if(is.null(dictionary$group)) {
    dictionary$group <- "GROUP"
  }
  if(is.null(data[[dictionary$group]])) {
    data[[dictionary$group]] <- 1
  }
  
  ## IDs
  ids <- data |>
    dplyr::select(ORIGID = dictionary$subject_id) |>
    dplyr::slice(1, .by = "ORIGID") |>
    dplyr::mutate(ID = 1:dplyr::n())
  
  ## Doses
  doses <- data |>
    dplyr::select(
      TIME = !!dictionary$time,
      ORIGID = !!dictionary$subject_id,
      GROUP = !!dictionary$group, 
      AMT = !!dictionary$dose,
      !!covariates
    ) |>
    dplyr::mutate(EVID = 1, MDV = 1, DV = 0, CMT = dose_compartment) |>
    dplyr::left_join(ids, by = dplyr::join_by("ORIGID"))
  if(nrow(doses) == nrow(data)) { # Dose is given as a column, and not row-wise using EVID
    doses <- doses |>
      dplyr::group_by("ORIGID", "GROUP") |>
      dplyr::slice(1) |>
      dplyr::mutate(TIME = 0) |>
      dplyr::ungroup()
  }
  
  ## Observations
  samples <- data |>
    dplyr::select(
      ORIGID = dictionary$subject_id, 
      GROUP = dictionary$group, 
      TIME = dictionary$time, 
      DV = dictionary$conc,
      !!covariates
    ) |>
    dplyr::mutate(AMT = 0, EVID = 0, MDV = 0, CMT = obs_compartment) |>
    dplyr::mutate(DV = as.numeric(ifelse(
      stringr::str_detect(tolower(.data$DV), "[<a-z]"), -99, .data$DV
    ))) |>
    dplyr::left_join(ids, by = dplyr::join_by("ORIGID"))
  
  ## Combine
  comb <- dplyr::bind_rows(
    doses,
    samples
  ) |>
    dplyr::mutate(ifelse(is.null(.data$GROUP), 1, .data$GROUP)) |>
    dplyr::arrange(!!dictionary$subject_id, !!dictionary$group, !!dictionary$time, .data$EVID) |>
    dplyr::select("ID", "TIME", "CMT", "EVID", "MDV", "DV", "AMT", "GROUP", "ORIGID", !!covariates) |>
    dplyr::arrange(.data$GROUP, .data$ID, .data$TIME, -.data$EVID)
  
  ## Convert all character columns to categorical (but numeric)
  for(key in names(comb)) {
    if(! inherits(comb[[key]], "numeric")) {
      suppressWarnings(
        comb[[key]] <- match(comb[[key]], unique(comb[[key]]))
      )
    }
  }
  
  ## Remove any observations with DV = -99
  comb <- dplyr::filter(comb, .data$DV != -99)
  
  ## Return
  comb
  
}
