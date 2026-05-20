#' Reformat NCA-type analysis-ready dataset (ARD) into a NONMEM-style modeling 
#' dataset
#' 
#' @param data dataset formatted as NCA analysis-ready dataset
#' @param dictionary a data dictionary that maps expected variable names to 
#' variables in the data.
#' @param dose_compartment the compartment in which doses are entered
#' @param obs_compartment the observation compartment number
#' @param covariates a vector of covariate names that are to be extracted
#' and added to the modeling dataset.
#' @param na what to set NA values to. E.g. ".", (default) or NA (keep NA),
#' or NULL (do nothing).
#' @param repeat_doses Optional list for repeated dosing (MAD studies). Must
#' contain `interval` (dosing interval in TIME units). Optionally contains `n`
#' (total number of doses). If `n` is omitted, it is inferred per subject/group
#' as `ceiling(max(observation_time) / interval)`. Only applies to column-wise
#' dose data. Default `NULL` preserves existing behavior (no ADDL/II columns).
#' Examples: `list(interval = 12)` or `list(n = 5, interval = 12)`.
#' @param categorical_mapping Either a character vector of column names to
#' auto-encode (most common value gets 0, next gets 1, etc.), or a data.frame
#' with columns `column`, `original_value`, `encoded_value` for explicit
#' mappings. NA values are encoded as -99. The final mapping is attached as a
#' `"categorical_mapping"` attribute on the returned data.frame. Default `NULL`
#' skips explicit encoding (existing blanket conversion still applies).
#'
#' @returns data.frame with population PK input data in NONMEM-style
#' format.
#'
#' @export
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
  covariates = NULL,
  repeat_doses = NULL,
  categorical_mapping = NULL,
  na = "."
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
    dplyr::filter(!is.na(AMT)) |>
    dplyr::mutate(EVID = 1, MDV = 1, DV = 0, CMT = dose_compartment) |>
    dplyr::left_join(ids, by = dplyr::join_by("ORIGID"))
  
  if(nrow(doses) == nrow(data)) { # Dose is given as a column, and not row-wise using EVID
    doses <- doses |>
      dplyr::group_by(.data$ORIGID, .data$GROUP) |>
      dplyr::slice(1) |>
      dplyr::mutate(TIME = 0) |>
      dplyr::ungroup()

    if (!is.null(repeat_doses)) {
      if (is.null(repeat_doses$interval)) {
        stop("`repeat_doses` must contain an `interval` element.")
      }
      interval <- repeat_doses$interval
      if (!is.null(repeat_doses$n)) {
        doses <- doses |>
          dplyr::mutate(ADDL = as.numeric(repeat_doses$n) - 1, II = interval)
      } else {
        max_obs_times <- data |>
          dplyr::select(
            ORIGID = !!dictionary$subject_id,
            GROUP  = !!dictionary$group,
            TIME   = !!dictionary$time
          ) |>
          dplyr::group_by(.data$ORIGID, .data$GROUP) |>
          dplyr::summarise(max_obs_time = max(.data$TIME, na.rm = TRUE), .groups = "drop")
        doses <- doses |>
          dplyr::left_join(max_obs_times, by = c("ORIGID", "GROUP")) |>
          dplyr::mutate(
            ADDL = pmax(0, ceiling(.data$max_obs_time / interval) - 1),
            II   = interval
          ) |>
          dplyr::select(-"max_obs_time")
      }
    }
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

  if (!is.null(repeat_doses)) {
    samples <- samples |> dplyr::mutate(ADDL = 0, II = 0)
  }

  ## Combine
  comb <- dplyr::bind_rows(
    doses,
    samples
  ) |>
    dplyr::mutate(ifelse(is.null(.data$GROUP), 1, .data$GROUP)) |>
    dplyr::arrange(!!dictionary$subject_id, !!dictionary$group, !!dictionary$time, .data$EVID) |>
    dplyr::select("ID", "TIME", "CMT", "EVID", "MDV", "DV", "AMT", dplyr::any_of(c("ADDL", "II")), "GROUP", "ORIGID", !!covariates) |>
    dplyr::arrange(.data$GROUP, .data$ID, .data$TIME, -.data$EVID)
  
  ## Apply user-specified categorical encoding
  comb <- apply_categorical_mapping(comb, categorical_mapping)
  cat_map <- attr(comb, "categorical_mapping")
  already_encoded <- if (!is.null(cat_map)) unique(cat_map$column) else character(0)

  ## Convert remaining character columns to categorical (but numeric)
  for(key in names(comb)) {
    if (key %in% already_encoded) next
    if(! rlang::inherits_any(comb[[key]], c("integer", "numeric"))) {
      suppressWarnings(
        comb[[key]] <- match(comb[[key]], unique(comb[[key]]))
      )
    }
  }

  ## Remove any observations with DV = -99
  comb <- comb |>
    dplyr::filter(.data$DV != -99)

  ## Convert NA's to dots or something else
  if(!is.null(na)) {
    comb <- comb |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(is.na(.) | . == "NA", na, .)))
  }

  ## Preserve categorical mapping attribute (dplyr may strip it)
  if (!is.null(cat_map)) attr(comb, "categorical_mapping") <- cat_map

  ## Return
  comb
  
}
