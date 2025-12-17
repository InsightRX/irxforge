#' Create a standardized table for demographics information
#'
#' @param data list of data.frames with SDTM dataset, including at least DM
#' table. 
#' @param demographics character vector specifying which demographics should
#' be pulled and summarized. The function will check availability of the 
#' demographics and try to figure out whether the data is continuous or 
#' categorical.
#' @param group name of variable in dataset to group statistics by, e.g. 
#' `"ACTARM"`
#' @param path optional, path to filename to save output table to.
#' 
#' @export
create_demographics_table <- function(
    data,
    demographics = c("weight", "height", "sex", "age", "race", "ethnic", "arm"),
    group = NULL,
    path = NULL
) {

  demographics <- tolower(demographics)
  
  ## figure out what is in demographics and what is in vitals
  if (is.null(data$vs)) {
    vs_demographics <- NULL
  } else {
    vs_available <- data$vs %>%
      stats::setNames(tolower(names(.))) %>%
      .$vstest %>% unique() %>% tolower()
    vs_demographics <- intersect(demographics, vs_available)
  }

  dm_possible <- c(
    "age", "sex", "race", "ethnic", "arm", "actarm", "country", "siteid",
    "studyid", "domain", "armcd"
  )
  dm_available <- intersect( # limit to what are actually demographics, not all columns are
    tolower(names(data$dm)), dm_possible
  )
  dm_demographics <- intersect(demographics, dm_available)
  
  ## get data not in DM table
  if (!is.null(data$vs)) {
    vitals <- data$vs %>%
      stats::setNames(tolower(names(.))) %>%
      dplyr::mutate(vstest = tolower(.data$vstest)) %>%
      dplyr::filter(.data$vstest %in% tolower(vs_demographics)) %>%
      dplyr::group_by(.data$usubjid) %>%
      dplyr::filter(!duplicated(.data$vstest)) %>%
      dplyr::select("usubjid", name = "vstest", value = "vsstresc") %>%
      tidyr::pivot_wider() %>%
      stats::setNames(tolower(names(.)))
  }
  ## merge into with DM table  
  demographics_found <- c(vs_demographics, dm_demographics)
  if(length(demographics_found) == 0) {
    stop("Couldn't find any requested demographic")
  }
  if(length(vs_demographics) == 0) {
    demo <- stats::setNames(data$dm, tolower(names(data$dm)))
  } else {
    if(length(dm_demographics) == 0) {
      demo <- vitals
    } else {
      demo <- data$dm %>%
        stats::setNames(tolower(names(.))) %>%
        dplyr::full_join(vitals, by = "usubjid")
    }
  }
  demo <- demo %>%
    dplyr::group_by(.data$usubjid) %>%
    dplyr::slice(1) %>% # make sure only 1 row per patient
    dplyr::select("usubjid", !! demographics_found) %>%
    dplyr::ungroup()

  ## figure out which is categorical and which is continuous
  types <- lapply(demographics_found, function(d) { is_continuous(demo[[d]]) } )
  names(types) <- demographics_found
  continuous <- names(types)[types == TRUE]
  categorical <- names(types)[types == FALSE]
  
  ## summary statistics
  cont_data <- NULL
  if(length(continuous) > 0) {
    cont_data <- demo %>%
      dplyr::select(tidyselect::all_of(continuous)) %>%
      dplyr::mutate(
        dplyr::across(!!continuous, ~ as.numeric(as.character(.x)))
      ) %>%
      tidyr::pivot_longer(cols = tidyselect::all_of(continuous)) %>%
      dplyr::group_by(c(.data$name, !!group)) %>%
      dplyr::summarise(
        mean = mean(.data$value, na.rm = TRUE),
        sd = stats::sd(.data$value, na.rm = TRUE),
        median = stats::median(.data$value, na.rm = TRUE),
        min = min(.data$value, na.rm = TRUE),
        max = max(.data$value, na.rm = TRUE)
      ) %>%
      dplyr::mutate(
        dplyr::across(c("mean", "sd", "median", "min", "max"), ~ as.character(round(.x, 1)))
      ) %>%
      dplyr::mutate(min_max = paste0(.data$min, " - ", .data$max)) %>%
      dplyr::select(-"min", -"max") %>%
      tidyr::pivot_longer(cols = c("mean", "sd", "median", "min_max")) %>%
      stats::setNames(c("Demographic", "Statistic", "Value")) 
  }
  
  cat_data <- NULL
  if(length(categorical) > 0) {
    cat_data <- lapply(categorical, function(x) {
      demo %>%
        dplyr::select(!!x) %>%
        table() %>%
        as.data.frame() %>%
        dplyr::mutate(Demographic = !!x) %>%
        stats::setNames(c("Statistic", "Value", "Demographic"))
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::select("Demographic", "Statistic", "Value") %>%
      dplyr::mutate(
        dplyr::across(!!names(.), ~ as.character(.x))
      ) 
  }
  
  comb_data <- dplyr::bind_rows(
    cont_data,
    cat_data
  ) %>%
    dplyr::mutate(
      Demographic = ifelse(duplicated(.data$Demographic), "", .data$Demographic)
    ) %>%
    dplyr::rename("Value/count" = "Value")
  
  if(!is.null(path)) {
    utils::write.csv(comb_data, file = path, row.names=F, quote=F)
  }
  
  comb_data
  
}
