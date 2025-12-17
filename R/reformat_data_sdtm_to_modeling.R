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
  adsl_vars <- admiral::exprs(TRTSDT, TRTSDTM, TRT01P, TRT01A)
  
  ## Concentrations
  pc_dates <- data$pc %>%
    # Join ADSL with PC (need TRTSDT for ADY derivation)
    admiral::derive_vars_merged(
      dataset_add = data$adsl,
      new_vars = adsl_vars,
      by_vars = admiral::exprs(STUDYID, USUBJID)
    ) %>%
    # Derive analysis date/time
    # Impute missing time to 00:00:00
    admiral::derive_vars_dtm(
      new_vars_prefix = "A",
      dtc = PCDTC,
      time_imputation = "00:00:00"
    ) %>%
    # Derive dates and times from date/times
    admiral::derive_vars_dtm_to_dt(admiral::exprs(ADTM)) %>%
    admiral::derive_vars_dtm_to_tm(admiral::exprs(ADTM)) %>%
    # Derive event ID and nominal relative time from first dose (NFRLT)
    dplyr::mutate(
      EVID = 0,
      DRUG = PCTEST,
      NFRLT = dplyr::if_else(
        PCTPTNUM < 0, 0, PCTPTNUM
      ), 
      .after = USUBJID
    )
  
  ## Doses
  ex_dates <- data$ex %>%
    admiral::derive_vars_merged(
      dataset_add = data$adsl,
      new_vars = adsl_vars,
      by_vars = admiral::exprs(STUDYID, USUBJID)
    ) %>%
    # Keep records with nonzero dose
    dplyr::filter(EXDOSE > 0) %>%
    # Add time and set missing end date to start date
    # Impute missing time to 00:00:00
    # Note all times are missing for dosing records in this example data
    # Derive Analysis Start and End Dates
    admiral::derive_vars_dtm(
      new_vars_prefix = "AST",
      dtc = EXSTDTC,
      time_imputation = "00:00:00"
    ) %>%
    admiral::derive_vars_dtm(
      new_vars_prefix = "AEN",
      dtc = EXENDTC,
      time_imputation = "00:00:00"
    ) %>%
    # Derive event ID and nominal relative time from first dose (NFRLT)
    dplyr::mutate(
      EVID = 1,
      NFRLT = 24 * (VISITDY - 1), .after = USUBJID
    ) %>%
    # Set missing end dates to start date
    dplyr::mutate(AENDTM = dplyr::case_when(
      is.na(AENDTM) ~ ASTDTM,
      TRUE ~ AENDTM
    )) %>%
    # Derive dates from date/times
    admiral::derive_vars_dtm_to_dt(admiral::exprs(ASTDTM)) %>%
    admiral::derive_vars_dtm_to_dt(admiral::exprs(AENDTM))
  
  ex_exp <- ex_dates %>%
    admiral::create_single_dose_dataset(
      dose_freq = EXDOSFRQ,
      start_date = ASTDT,
      start_datetime = ASTDTM,
      end_date = AENDT,
      end_datetime = AENDTM,
      nominal_time = NFRLT,
      lookup_table = admiral::dose_freq_lookup,
      lookup_column = CDISC_VALUE,
      keep_source_vars = admiral::exprs(
        STUDYID, USUBJID, EVID, EXDOSFRQ, EXDOSFRM,
        NFRLT, EXDOSE, EXDOSU, EXTRT, ASTDT, ASTDTM, AENDT, AENDTM,
        VISIT, VISITNUM, VISITDY, EXROUTE, EXDOSFRM,
        TRT01A, TRT01P, DOMAIN, EXSEQ, !!!adsl_vars
      )
    ) %>%
    # Derive AVISIT based on nominal relative time
    # Derive AVISITN to nominal time in whole days using integer division
    # Define AVISIT based on nominal day
    dplyr::mutate(
      AVISITN = NFRLT %/% 24 + 1,
      AVISIT = paste("Day", AVISITN),
      ADTM = ASTDTM,
      DRUG = EXTRT
    ) %>%
    # Derive dates and times from datetimes
    admiral::derive_vars_dtm_to_dt(admiral::exprs(ADTM)) %>%
    admiral::derive_vars_dtm_to_tm(admiral::exprs(ADTM)) %>%
    admiral::derive_vars_dtm_to_tm(admiral::exprs(ASTDTM)) %>%
    admiral::derive_vars_dtm_to_tm(admiral::exprs(AENDTM)) %>%
    # TODO: line below is necessary, because create_single_dose_dataset() messes 
    #       up the actual nominal times. Should replace with custom function?
    dplyr::filter(AVISITN == 1)
  
  # ---- Find first dose per treatment per subject ----
  # ---- Join with ADPPK data and keep only subjects with dosing ----
  
  adppk_first_dose <- pc_dates %>%
    admiral::derive_vars_merged(
      dataset_add = ex_exp,
      filter_add = (!is.na(ADTM)),
      new_vars = admiral::exprs(
        FANLDTM = ADTM, 
        EXDOSE_first = EXDOSE
      ),
      order = admiral::exprs(ADTM, EXSEQ),
      mode = "first",
      by_vars = admiral::exprs(STUDYID, USUBJID, DRUG)
    ) %>%
    dplyr::filter(!is.na(FANLDTM)) %>%
    # Derive AVISIT based on nominal relative time
    # Derive AVISITN to nominal time in whole days using integer division
    # Define AVISIT based on nominal day
    dplyr::mutate(
      AVISITN = NFRLT %/% 24 + 1,
      AVISIT = paste("Day", AVISITN),
    )
  
  # ---- Find previous dose  ----
  adppk_prev <- adppk_first_dose %>%
    admiral::derive_vars_joined(
      dataset_add = ex_exp,
      by_vars = admiral::exprs(USUBJID),
      order = admiral::exprs(ADTM),
      new_vars = admiral::exprs(
        ADTM_prev = ADTM, 
        EXDOSE_prev = EXDOSE, 
        AVISIT_prev = AVISIT,
        AENDTM_prev = AENDTM
      ),
      join_type = "all",
      join_vars = admiral::exprs(ADTM),
      filter_add = NULL,
      filter_join = ADTM > ADTM.join,
      mode = "last",
      check_type = "none"
    )
  
  # ---- Find previous nominal dose ----
  adppk_nom_prev <- adppk_prev %>%
    admiral::derive_vars_joined(
      dataset_add = ex_exp,
      by_vars = admiral::exprs(USUBJID),
      order = admiral::exprs(NFRLT),
      new_vars = admiral::exprs(NFRLT_prev = NFRLT),
      join_type = "all",
      join_vars = admiral::exprs(NFRLT),
      filter_add = NULL,
      filter_join = NFRLT > NFRLT.join,
      mode = "last",
      check_type = "none"
    )
  
  # ---- Combine ADPPK and EX data ----
  # Derive Relative Time Variables
  adppk_aprlt <- dplyr::bind_rows(adppk_nom_prev, ex_exp) %>%
    dplyr::group_by(USUBJID, DRUG) %>%
    dplyr::mutate(
      FANLDTM = min(FANLDTM, na.rm = TRUE),
      min_NFRLT = min(NFRLT, na.rm = TRUE),
      maxdate = max(ADT[EVID == 0], na.rm = TRUE), .after = USUBJID
    ) %>%
    dplyr::arrange(USUBJID, ADTM) %>%
    dplyr::ungroup() %>%
    dplyr::filter(ADT <= maxdate) %>%
    # Derive Actual Relative Time from First Dose (AFRLT)
    admiral::derive_vars_duration(
      new_var = AFRLT,
      start_date = FANLDTM,
      end_date = ADTM,
      out_unit = "hours",
      floor_in = FALSE,
      add_one = FALSE
    ) %>%
    # Derive Actual Relative Time from Reference Dose (APRLT)
    admiral::derive_vars_duration(
      new_var = APRLT,
      start_date = ADTM_prev,
      end_date = ADTM,
      out_unit = "hours",
      floor_in = FALSE,
      add_one = FALSE
    ) %>%
    # Derive APRLT
    dplyr::mutate(
      APRLT = dplyr::case_when(
        EVID == 1 ~ 0,
        is.na(APRLT) ~ AFRLT,
        TRUE ~ APRLT
      ),
      NPRLT = dplyr::case_when(
        EVID == 1 ~ 0,
        is.na(NFRLT_prev) ~ NFRLT - min_NFRLT,
        TRUE ~ NFRLT - NFRLT_prev
      )
    ) %>%
    dplyr::mutate(
      ROUTE = EXROUTE, 
      FORM = EXDOSFRM
    )
  
  # ---- Derive Analysis Variables ----
  # Derive actual dose DOSEA and planned dose DOSEP,
  # Derive AVAL and DV
  
  adppk_aval <- adppk_aprlt %>%
    dplyr::mutate(
      # Derive Actual Dose
      DOSEA = dplyr::case_when(
        EVID == 1 ~ EXDOSE,
        is.na(EXDOSE_prev) ~ EXDOSE_first,
        TRUE ~ EXDOSE_prev
      ),
      # Derive PARAMCD
      PARAMCD = dplyr::case_when(
        EVID == 1 ~ "DOSE",
        TRUE ~ PCTESTCD
      ),
      ALLOQ = PCLLOQ,
      # Derive CMT
      CMT = dplyr::case_when(
        EVID == 1 ~ 1,
        TRUE ~ 2
      ),
      # Derive BLQFL/BLQFN
      BLQFL = dplyr::case_when(
        PCSTRESC == "<BLQ" ~ "Y",
        TRUE ~ "N"
      ),
      BLQFN = dplyr::case_when(
        PCSTRESC == "<BLQ" ~ 1,
        TRUE ~ 0
      ),
      AMT = dplyr::case_when(
        EVID == 1 ~ EXDOSE,
        TRUE ~ NA_real_
      ),
      # Derive DV and AVAL
      DV = PCSTRESN,
      AVAL = DV,
      DVL = dplyr::case_when(
        DV != 0 ~ log(DV),
        TRUE ~ NA_real_
      ),
      # Derive MDV
      MDV = dplyr::case_when(
        EVID == 1 ~ 1,
        is.na(DV) ~ 1,
        TRUE ~ 0
      ),
      AVALU = dplyr::case_when(
        EVID == 1 ~ NA_character_,
        TRUE ~ PCSTRESU
      ),
      UDTC = lubridate::format_ISO8601(ADTM),
      II = 0, # TODO: original implementation was wrong
      SS = 0, # TODO: original implementation was wrong
    )
  
  # ---- Add ASEQ ----
  
  adppk_aseq <- adppk_aval %>%
    # Calculate ASEQ
    admiral::derive_var_obs_number(
      new_var = ASEQ,
      by_vars = admiral::exprs(STUDYID, USUBJID),
      order = admiral::exprs(AFRLT, EVID),
      check_type = "error"
    ) %>%
    # Derive PARAM and PARAMN
    admiral::derive_vars_merged(dataset_add = dplyr::select(param_lookup, -PCTESTCD), by_vars = admiral::exprs(PARAMCD)) %>%
    dplyr::mutate(
      PROJID = DRUG,
      PROJIDN = 1
    ) %>%
    # Remove temporary variables
    dplyr::select(
      -DOMAIN, -tidyselect::starts_with("min"), -tidyselect::starts_with("max"), -tidyselect::starts_with("EX"),
      -tidyselect::starts_with("PC"), -tidyselect::ends_with("first"), -tidyselect::ends_with("prev"),
      -tidyselect::ends_with("DTM"), -tidyselect::ends_with("DT"), -tidyselect::ends_with("TM"), -tidyselect::starts_with("VISIT"),
      -tidyselect::starts_with("AVISIT"), -tidyselect::starts_with("PARAM"),
      -tidyselect::ends_with("TMF"), -tidyselect::starts_with("TRT"), -tidyselect::starts_with("ATPT"), -DRUG
    )
  
  #---- Derive Covariates ----
  # Include numeric values for STUDYIDN, USUBJIDN, SEXN, RACEN etc.
  covar <- data$adsl %>%
    dplyr::mutate(
      STUDYIDN = as.numeric(stringr::word(USUBJID, 1, sep = stringr::fixed("-"))),
      SITEIDN = as.numeric(stringr::word(USUBJID, 2, sep = stringr::fixed("-"))),
      USUBJIDN = as.numeric(stringr::word(USUBJID, 3, sep = stringr::fixed("-"))),
      SUBJIDN = as.numeric(SUBJID),
      SEXN = dplyr::case_when(
        SEX == "M" ~ 1,
        SEX == "F" ~ 2,
        TRUE ~ 3
      )
    )
  categorical_vars <- c("RACE", "ETHNIC", "ARM", "ACTARM", "COUNTRY")
  covar[paste0(categorical_vars, "N")] <- data.matrix(covar[, categorical_vars])
  covar <- covar %>%
    dplyr::mutate(
      COHORT = ARMN,
      COHORTC = ARM
    ) %>%
    dplyr::select(
      STUDYID, STUDYIDN, SITEID, SITEIDN, USUBJID, USUBJIDN,
      SUBJID, SUBJIDN, AGE, SEX, SEXN, COHORT, COHORTC,
      RACE, RACEN, ETHNIC, ETHNICN, COUNTRY, COUNTRYN
    )
  
  #---- Derive additional baselines from VS and LB ----
  numeric_vars <- c("CREAT", "ALT", "AST", "BILI") ## TODO: fairly generic, but might not always be available or might include others
  labsbl <- data$lb %>%
    dplyr::filter(LBBLFL == "Y" & LBTESTCD %in% numeric_vars) %>%
    dplyr::mutate(LBTESTCDB = paste0(LBTESTCD, "BL")) %>%
    dplyr::select(STUDYID, USUBJID, LBTESTCDB, LBSTRESN)
  
  covar_vslb <- covar %>%
    admiral::derive_vars_merged(
      dataset_add = data$vs,
      filter_add = VSTESTCD == "HEIGHT",
      by_vars = admiral::exprs(STUDYID, USUBJID),
      new_vars = admiral::exprs(HTBL = VSSTRESN)
    ) %>%
    admiral::derive_vars_merged(
      dataset_add = data$vs,
      filter_add = VSTESTCD == "WEIGHT" & VSBLFL == "Y",
      by_vars = admiral::exprs(STUDYID, USUBJID),
      new_vars = admiral::exprs(WTBL = VSSTRESN)
    ) %>%
    admiral::derive_vars_transposed(
      dataset_merge = labsbl,
      by_vars = admiral::exprs(STUDYID, USUBJID),
      key_var = LBTESTCDB,
      value_var = LBSTRESN
    ) %>%
    dplyr::mutate(
      BMIBL = admiral::compute_bmi(height = HTBL, weight = WTBL),
      BSABL = admiral::compute_bsa(
        height = HTBL,
        weight = WTBL,
        method = "Mosteller"
      ),
      CRCLBL = admiral::compute_egfr(
        creat = CREATBL, creatu = "SI", age = AGE, weight = WTBL, sex = SEX,
        method = "CRCL"
      ),
      EGFRBL = admiral::compute_egfr(
        creat = CREATBL, creatu = "SI", age = AGE, weight = WTBL, sex = SEX,
        method = "CKD-EPI"
      )
    ) %>%
    dplyr::rename(TBILBL = BILIBL)
  
  # Combine covariates with APPPK data
  adppk <- adppk_aseq %>%
    admiral::derive_vars_merged(
      dataset_add = covar_vslb,
      by_vars = admiral::exprs(STUDYID, USUBJID)
    ) %>%
    dplyr::arrange(STUDYIDN, USUBJIDN, AFRLT, EVID) %>%
    dplyr::mutate(RECSEQ = dplyr::row_number()) %>%
    dplyr::mutate(ROUTE = tolower(ROUTE), FORM = tolower(FORM))
  
  poppk_data <- adppk %>% # select the variables we need from the data
    dplyr::select(
      ID = SUBJID, TIME = NFRLT, 
      DV, MDV, EVID, SS, II,
      AMT, SEXN, AGE, WT = WTBL, 
      ROUTE, FORM, 
      COHORT = COHORTC, SITEID,
      RACE, ETHNIC, COUNTRY
    ) %>% 
    dplyr::filter(!(is.na(DV) & EVID == 0)) # filter out DV=0 at time==0
  
  poppk_data
  
}

