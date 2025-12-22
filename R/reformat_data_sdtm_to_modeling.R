#' Reformat SDTM datasets into NONMEM-style modeling dataset
#'
#' @returns data.frame with population PK input data in NONMEM-style
#' format. It will also add the non-standard columns ROUTE ("oral", "iv") and 
#' FORM (formulation: "tablet", "suspension", "patch", "infusion", etc.) with 
#' values for each dose and NA for observations.
#' 
#' @param data list containing data.frames with SDTM domains
#' @param dictionary a data dictionary that maps expected variable names to 
#' variables in the data.
#'
#' @export
#' 
reformat_data_sdtm_to_modeling <- function(
  data, 
  dictionary
) {
  
  ## Parse into modeling dataset (NONMEM-style format)
  ## For the modeling analysis we need the dosing history (in this example
  ## very simple, since just a single dose), and the concentration data.
  ## For covariate analyses we also need the covariates (e.g. weight, etc).
  ## We'll pull those together in a single data file.
  
  ## This code was adapted from an example on the admiral package website.
  ## It was made more generic, e.g. avoid hardcoded covariates as much as
  ## possible. Still needs some work to make fully generic.
  
  for(key in names(data)) { # admiral package is written assuming uppercase column names
    names(data[[key]]) <- toupper(names(data[[key]]))
  }
  
  param_lookup <- data.frame(
    PCTESTCD = c("DRUGX", "DOSE"),
    PARAMCD = c("DRUGX", "DOSE"),
    PARAM = c("concentration of DrugX", "DrugX Dose"),
    PARAMN = c(1, 2)
  )
  
  # Get list of ADSL vars required for derivations
  adsl_vars <- syms_to_exprs(c("TRTSDT", "TRTSDTM", "TRT01P", "TRT01A"))
  
  ## Concentrations
  pc_dates <- data$pc %>%
    # Join ADSL with PC (need TRTSDT for ADY derivation)
    admiral::derive_vars_merged(
      dataset_add = data$adsl,
      new_vars = adsl_vars,
      by_vars = syms_to_exprs(c("STUDYID", "USUBJID"))
    ) %>%
    # Derive analysis date/time
    # Impute missing time to 00:00:00
    admiral::derive_vars_dtm(
      new_vars_prefix = "A",
      dtc = !!rlang::sym("PCDTC"),
      time_imputation = "00:00:00"
    ) %>%
    # Derive dates and times from date/times
    admiral::derive_vars_dtm_to_dt(syms_to_exprs(c("ADTM"))) %>%
    admiral::derive_vars_dtm_to_tm(syms_to_exprs(c("ADTM"))) %>%
    # Derive event ID and nominal relative time from first dose (NFRLT)
    dplyr::mutate(
      EVID = 0,
      DRUG = .data$PCTEST,
      NFRLT = dplyr::if_else(
        .data$PCTPTNUM < 0, 0, .data$PCTPTNUM
      ), 
      .after = "USUBJID"
    )
  
  ## Doses
  ex_dates <- data$ex %>%
    admiral::derive_vars_merged(
      dataset_add = data$adsl,
      new_vars = adsl_vars,
      by_vars = syms_to_exprs(c("STUDYID", "USUBJID"))
    ) %>%
    # Keep records with nonzero dose
    dplyr::filter(.data$EXDOSE > 0) %>%
    # Add time and set missing end date to start date
    # Impute missing time to 00:00:00
    # Note all times are missing for dosing records in this example data
    # Derive Analysis Start and End Dates
    admiral::derive_vars_dtm(
      new_vars_prefix = "AST",
      dtc = !!rlang::sym("EXSTDTC"),
      time_imputation = "00:00:00"
    ) %>%
    admiral::derive_vars_dtm(
      new_vars_prefix = "AEN",
      dtc = !!rlang::sym("EXENDTC"),
      time_imputation = "00:00:00"
    ) %>%
    # Derive event ID and nominal relative time from first dose (NFRLT)
    dplyr::mutate(
      EVID = 1,
      NFRLT = 24 * (.data$VISITDY - 1),
      .after = "USUBJID"
    ) %>%
    # Set missing end dates to start date
    dplyr::mutate(
      AENDTM = dplyr::case_when(
        is.na(.data$AENDTM) ~ .data$ASTDTM,
        TRUE ~ .data$AENDTM
      )
    ) %>%
    # Derive dates from date/times
    admiral::derive_vars_dtm_to_dt(syms_to_exprs(c("ASTDTM"))) %>%
    admiral::derive_vars_dtm_to_dt(syms_to_exprs(c("AENDTM")))
  
  ex_exp <- ex_dates %>%
    admiral::create_single_dose_dataset(
      dose_freq = !!rlang::sym("EXDOSFRQ"),
      start_date = !!rlang::sym("ASTDT"),
      start_datetime = !!rlang::sym("ASTDTM"),
      end_date = !!rlang::sym("AENDT"),
      end_datetime = !!rlang::sym("AENDTM"),
      nominal_time = !!rlang::sym("NFRLT"),
      lookup_table = admiral::dose_freq_lookup,
      lookup_column = !!rlang::sym("CDISC_VALUE"),
      keep_source_vars = syms_to_exprs(c(
        "STUDYID", "USUBJID", "EVID", "EXDOSFRQ", "EXDOSFRM",
        "NFRLT", "EXDOSE", "EXDOSU", "EXTRT", "ASTDT", "ASTDTM", "AENDT", "AENDTM",
        "VISIT", "VISITNUM", "VISITDY", "EXROUTE", "EXDOSFRM",
        "TRT01A", "TRT01P", "DOMAIN", "EXSEQ", !!!adsl_vars
      ))
    ) %>%
    # Derive AVISIT based on nominal relative time
    # Derive AVISITN to nominal time in whole days using integer division
    # Define AVISIT based on nominal day
    dplyr::mutate(
      AVISITN = .data$NFRLT %/% 24 + 1,
      AVISIT = paste("Day", .data$AVISITN),
      ADTM = .data$ASTDTM,
      DRUG = .data$EXTRT
    ) %>%
    # Derive dates and times from datetimes
    admiral::derive_vars_dtm_to_dt(syms_to_exprs(c("ADTM"))) %>%
    admiral::derive_vars_dtm_to_tm(syms_to_exprs(c("ADTM"))) %>%
    admiral::derive_vars_dtm_to_tm(syms_to_exprs(c("ASTDTM"))) %>%
    admiral::derive_vars_dtm_to_tm(syms_to_exprs(c("AENDTM"))) %>%
    # TODO: line below is necessary, because create_single_dose_dataset() messes 
    #       up the actual nominal times. Should replace with custom function?
    dplyr::filter(.data$AVISITN == 1)
  
  # ---- Find first dose per treatment per subject ----
  # ---- Join with ADPPK data and keep only subjects with dosing ----
  
  adppk_first_dose <- pc_dates %>%
    admiral::derive_vars_merged(
      dataset_add = ex_exp,
      filter_add = (!is.na(.data$ADTM)),
      new_vars = syms_to_exprs(c(
        FANLDTM = "ADTM", 
        EXDOSE_first = "EXDOSE"
      )),
      order = syms_to_exprs(c("ADTM", "EXSEQ")),
      mode = "first",
      by_vars = syms_to_exprs(c("STUDYID", "USUBJID", "DRUG"))
    ) %>%
    dplyr::filter(!is.na(.data$FANLDTM)) %>%
    # Derive AVISIT based on nominal relative time
    # Derive AVISITN to nominal time in whole days using integer division
    # Define AVISIT based on nominal day
    dplyr::mutate(
      AVISITN = .data$NFRLT %/% 24 + 1,
      AVISIT = paste("Day", .data$AVISITN),
    )
  
  # ---- Find previous dose  ----
  adppk_prev <- adppk_first_dose %>%
    admiral::derive_vars_joined(
      dataset_add = ex_exp,
      by_vars = syms_to_exprs(c("USUBJID")),
      order = syms_to_exprs(c("ADTM")),
      new_vars = syms_to_exprs(c(
        ADTM_prev = "ADTM", 
        EXDOSE_prev = "EXDOSE", 
        AVISIT_prev = "AVISIT",
        AENDTM_prev = "AENDTM"
      )),
      join_type = "all",
      join_vars = syms_to_exprs(c("ADTM")),
      filter_add = NULL,
      filter_join = .data$ADTM > .data$ADTM.join,
      mode = "last",
      check_type = "none"
    )
  
  # ---- Find previous nominal dose ----
  adppk_nom_prev <- adppk_prev %>%
    admiral::derive_vars_joined(
      dataset_add = ex_exp,
      by_vars = syms_to_exprs(c("USUBJID")),
      order = syms_to_exprs(c("NFRLT")),
      new_vars = syms_to_exprs(c(NFRLT_prev = "NFRLT")),
      join_type = "all",
      join_vars = syms_to_exprs(c("NFRLT")),
      filter_add = NULL,
      filter_join = .data$NFRLT > .data$NFRLT.join,
      mode = "last",
      check_type = "none"
    )
  
  # ---- Combine ADPPK and EX data ----
  # Derive Relative Time Variables
  adppk_aprlt <- dplyr::bind_rows(adppk_nom_prev, ex_exp) %>%
    dplyr::group_by("USUBJID", "DRUG") %>%
    dplyr::mutate(
      FANLDTM = min(.data$FANLDTM, na.rm = TRUE),
      min_NFRLT = min(.data$NFRLT, na.rm = TRUE),
      maxdate = max(.data$ADT[.data$EVID == 0], na.rm = TRUE),
      .after = "USUBJID"
    ) %>%
    dplyr::arrange(.data$USUBJID, .data$ADTM) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$ADT <= .data$maxdate) %>%
    # Derive Actual Relative Time from First Dose (AFRLT)
    admiral::derive_vars_duration(
      new_var = !!rlang::sym("AFRLT"),
      start_date = !!rlang::sym("FANLDTM"),
      end_date = !!rlang::sym("ADTM"),
      out_unit = "hours",
      floor_in = FALSE,
      add_one = FALSE
    ) %>%
    # Derive Actual Relative Time from Reference Dose (APRLT)
    admiral::derive_vars_duration(
      new_var = !!rlang::sym("APRLT"),
      start_date = !!rlang::sym("ADTM_prev"),
      end_date = !!rlang::sym("ADTM"),
      out_unit = "hours",
      floor_in = FALSE,
      add_one = FALSE
    ) %>%
    # Derive APRLT
    dplyr::mutate(
      APRLT = dplyr::case_when(
        .data$EVID == 1 ~ 0,
        is.na(.data$APRLT) ~ .data$AFRLT,
        TRUE ~ .data$APRLT
      ),
      NPRLT = dplyr::case_when(
        .data$EVID == 1 ~ 0,
        is.na(.data$NFRLT_prev) ~ .data$NFRLT - .data$min_NFRLT,
        TRUE ~ .data$NFRLT - .data$NFRLT_prev
      )
    ) %>%
    dplyr::mutate(
      ROUTE = .data$EXROUTE, 
      FORM = .data$EXDOSFRM
    )
  
  # ---- Derive Analysis Variables ----
  # Derive actual dose DOSEA and planned dose DOSEP,
  # Derive AVAL and DV
  
  adppk_aval <- adppk_aprlt %>%
    dplyr::mutate(
      # Derive Actual Dose
      DOSEA = dplyr::case_when(
        .data$EVID == 1 ~ .data$EXDOSE,
        is.na(.data$EXDOSE_prev) ~ .data$EXDOSE_first,
        TRUE ~ .data$EXDOSE_prev
      ),
      # Derive PARAMCD
      PARAMCD = dplyr::case_when(
        .data$EVID == 1 ~ "DOSE",
        TRUE ~ .data$PCTESTCD
      ),
      ALLOQ = .data$PCLLOQ,
      # Derive CMT
      CMT = dplyr::case_when(
        .data$EVID == 1 ~ 1,
        TRUE ~ 2
      ),
      # Derive BLQFL/BLQFN
      BLQFL = dplyr::case_when(
        .data$PCSTRESC == "<BLQ" ~ "Y",
        TRUE ~ "N"
      ),
      BLQFN = dplyr::case_when(
        .data$PCSTRESC == "<BLQ" ~ 1,
        TRUE ~ 0
      ),
      AMT = dplyr::case_when(
        .data$EVID == 1 ~ .data$EXDOSE,
        TRUE ~ NA_real_
      ),
      # Derive DV and AVAL
      DV = .data$PCSTRESN,
      AVAL = .data$DV,
      DVL = dplyr::case_when(
        .data$DV != 0 ~ log(.data$DV),
        TRUE ~ NA_real_
      ),
      # Derive MDV
      MDV = dplyr::case_when(
        .data$EVID == 1 ~ 1,
        is.na(.data$DV) ~ 1,
        TRUE ~ 0
      ),
      AVALU = dplyr::case_when(
        .data$EVID == 1 ~ NA_character_,
        TRUE ~ .data$PCSTRESU
      ),
      UDTC = lubridate::format_ISO8601(.data$ADTM),
      II = 0, # TODO: original implementation was wrong
      SS = 0, # TODO: original implementation was wrong
    )
  
  # ---- Add ASEQ ----
  
  adppk_aseq <- adppk_aval %>%
    # Calculate ASEQ
    admiral::derive_var_obs_number(
      new_var = !!rlang::sym("ASEQ"),
      by_vars = syms_to_exprs(c("STUDYID", "USUBJID")),
      order = syms_to_exprs(c("AFRLT", "EVID")),
      check_type = "error"
    ) %>%
    # Derive PARAM and PARAMN
    admiral::derive_vars_merged(dataset_add = dplyr::select(param_lookup, -"PCTESTCD"), by_vars = syms_to_exprs(c("PARAMCD"))) %>%
    dplyr::mutate(
      PROJID = .data$DRUG,
      PROJIDN = 1
    ) %>%
    # Remove temporary variables
    dplyr::select(
      -"DOMAIN",
      -tidyselect::starts_with("min"),
      -tidyselect::starts_with("max"),
      -tidyselect::starts_with("EX"),
      -tidyselect::starts_with("PC"),
      -tidyselect::ends_with("first"),
      -tidyselect::ends_with("prev"),
      -tidyselect::ends_with("DTM"),
      -tidyselect::ends_with("DT"),
      -tidyselect::ends_with("TM"),
      -tidyselect::starts_with("VISIT"),
      -tidyselect::starts_with("AVISIT"),
      -tidyselect::starts_with("PARAM"),
      -tidyselect::ends_with("TMF"),
      -tidyselect::starts_with("TRT"),
      -tidyselect::starts_with("ATPT"),
      -"DRUG"
    )
  
  #---- Derive Covariates ----
  # Include numeric values for STUDYIDN, USUBJIDN, SEXN, RACEN etc.
  covar <- data$adsl %>%
    dplyr::mutate(
      STUDYIDN = as.numeric(stringr::word(.data$USUBJID, 1, sep = stringr::fixed("-"))),
      SITEIDN = as.numeric(stringr::word(.data$USUBJID, 2, sep = stringr::fixed("-"))),
      USUBJIDN = as.numeric(stringr::word(.data$USUBJID, 3, sep = stringr::fixed("-"))),
      SUBJIDN = as.numeric(.data$SUBJID),
      SEXN = dplyr::case_when(
        .data$SEX == "M" ~ 1,
        .data$SEX == "F" ~ 2,
        TRUE ~ 3
      )
    )
  categorical_vars <- c("RACE", "ETHNIC", "ARM", "ACTARM", "COUNTRY")
  covar[paste0(categorical_vars, "N")] <- data.matrix(covar[, categorical_vars])
  covar <- covar %>%
    dplyr::mutate(
      COHORT = .data$ARMN,
      COHORTC = .data$ARM
    ) %>%
    dplyr::select(
      "STUDYID", "STUDYIDN", "SITEID", "SITEIDN", "USUBJID", "USUBJIDN",
      "SUBJID", "SUBJIDN", "AGE", "SEX", "SEXN", "COHORT", "COHORTC",
      "RACE", "RACEN", "ETHNIC", "ETHNICN", "COUNTRY", "COUNTRYN"
    )
  
  #---- Derive additional baselines from VS and LB ----
  numeric_vars <- c("CREAT", "ALT", "AST", "BILI") ## TODO: fairly generic, but might not always be available or might include others
  labsbl <- data$lb %>%
    dplyr::filter(.data$LBBLFL == "Y" & .data$LBTESTCD %in% numeric_vars) %>%
    dplyr::mutate(LBTESTCDB = paste0(.data$LBTESTCD, "BL")) %>%
    dplyr::select("STUDYID", "USUBJID", "LBTESTCDB", "LBSTRESN")
  
  covar_vslb <- covar %>%
    admiral::derive_vars_merged(
      dataset_add = data$vs,
      filter_add = !!rlang::sym("VSTESTCD") == "HEIGHT",
      by_vars = syms_to_exprs(c("STUDYID", "USUBJID")),
      new_vars = syms_to_exprs(c("HTBL" = "VSSTRESN"))
    ) %>%
    admiral::derive_vars_merged(
      dataset_add = data$vs,
      filter_add = !!rlang::sym("VSTESTCD") == "WEIGHT" & !!rlang::sym("VSBLFL") == "Y",
      by_vars = syms_to_exprs(c("STUDYID", "USUBJID")),
      new_vars = syms_to_exprs(c(WTBL = "VSSTRESN"))
    ) %>%
    admiral::derive_vars_transposed(
      dataset_merge = labsbl,
      by_vars = syms_to_exprs(c("STUDYID", "USUBJID")),
      key_var = !!rlang::sym("LBTESTCDB"),
      value_var = !!rlang::sym("LBSTRESN")
    ) %>%
    dplyr::mutate(
      BMIBL = admiral::compute_bmi(height = .data$HTBL, weight = .data$WTBL),
      BSABL = admiral::compute_bsa(
        height = .data$HTBL,
        weight = .data$WTBL,
        method = "Mosteller"
      ),
      CRCLBL = admiral::compute_egfr(
        creat = .data$CREATBL,
        creatu = "SI",
        age = .data$AGE,
        weight = .data$WTBL,
        sex = .data$SEX,
        method = "CRCL"
      ), 
      EGFRBL = admiral::compute_egfr(
        creat = .data$CREATBL,
        creatu = "SI",
        age = .data$AGE,
        weight = .data$WTBL,
        sex = .data$SEX,
        method = "CKD-EPI"
      )
    ) %>%
    dplyr::rename(TBILBL = "BILIBL")
  
  # Combine covariates with APPPK data
  adppk <- adppk_aseq %>%
    admiral::derive_vars_merged(
      dataset_add = covar_vslb,
      by_vars = syms_to_exprs(c("STUDYID", "USUBJID"))
    ) %>%
    dplyr::arrange(.data$STUDYIDN, .data$USUBJIDN, .data$AFRLT, .data$EVID) %>%
    dplyr::mutate(RECSEQ = dplyr::row_number()) %>%
    dplyr::mutate(ROUTE = tolower(.data$ROUTE), FORM = tolower(.data$FORM))
  
  poppk_data <- adppk %>% # select the variables we need from the data
    dplyr::select(
      ID = "SUBJID",
      TIME = "NFRLT",
      "DV",
      "MDV",
      "EVID",
      "SS",
      "II",
      "AMT",
      "SEXN",
      "AGE",
      WT = "WTBL",
      "ROUTE",
      "FORM",
      COHORT = "COHORTC",
      "SITEID",
      "RACE",
      "ETHNIC",
      "COUNTRY"
    ) %>% 
    dplyr::filter(!(is.na(.data$DV) & .data$EVID == 0)) # filter out DV=0 at time==0
  
  poppk_data
}

