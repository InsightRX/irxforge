#' Sample covariates from the NHANES database
#'
#' @param covariates character vector of NHANES variable names to include in
#'   the output, e.g. `c("RIDAGEYR", "BMXBMI", "WTMEC2YR")`. If `NULL`
#'   (default), all variables in the cached data are returned (SEQN is always
#'   dropped).
#' @param year NHANES survey cycle, e.g. `"2017-2018"`. Supported values:
#'   `"1999-2000"`, `"2001-2002"`, `"2003-2004"`, `"2005-2006"`,
#'   `"2007-2008"`, `"2009-2010"`, `"2011-2012"`, `"2013-2014"`,
#'   `"2015-2016"`, `"2017-2018"`, `"2019-2020"`.
#' @param n_subjects number of subjects to sample. Default is 100.
#' @param conditional list with conditional limits for sampled population,
#'   e.g. `list("RIDAGEYR" = c(18, 65), "BMXBMI" = c(18, 35))`. Filters are
#'   applied before sampling.
#' @param use_weights logical. If `TRUE`, use NHANES 2-year MEC examination
#'   weights (`WTMEC2YR`) for probability-proportional sampling, which
#'   produces a sample more representative of the U.S. civilian
#'   non-institutionalized population. Requires `WTMEC2YR` to be present in
#'   the cached data (included when `"DEMO"` was downloaded). Default `FALSE`.
#' @param seed integer random seed passed to [set.seed()] for reproducibility.
#'   Default `NULL` does not set a seed.
#' @param dictionary named list mapping user-defined covariate names to their
#'   NHANES variable names, e.g.
#'   `list("WT" = "BMXWT", "HT" = "BMXHT", "AGE" = "RIDAGEYR")`.
#'   Names in `covariates` and `conditional` that appear as keys in
#'   `dictionary` are translated to the corresponding NHANES names before
#'   lookup and translated back in the output. Names not present in
#'   `dictionary` are treated as direct NHANES variable names.
#' @param na.rm logical. If `TRUE` (default), rows with `NA` in any of the
#'   requested covariates are dropped before sampling.
#' @param cache_dir path to a directory containing a merged NHANES RDS file
#'   created by [download_nhanes_cache()]. Defaults to the package-level cache
#'   populated automatically on first load. Set to `NULL` to always download
#'   on demand via `nhanesA` (requires internet).
#' @param ... additional arguments (currently unused)
#'
#' @details
#' On first load, `irxforge` automatically downloads NHANES Demographics,
#' Laboratory, and Examination tables (cycle 2017-2018) and saves a single
#' merged RDS file in the package installation directory. Subsequent calls
#' read from this cache with no internet access required.
#'
#' Call [download_nhanes_cache()] to pre-download additional years or groups.
#'
#' If the cache file for the requested year is absent, an error is raised with
#' instructions to run [download_nhanes_cache()].
#'
#' NHANES uses a complex multi-stage sampling design. Survey weights reflect
#' the probability of selection and non-response. Use `use_weights = TRUE` to
#' account for this when sampling.
#'
#' @section Key covariates in the default cache (NHANES 2017-2018):
#' The default cache merges all Demographics, Laboratory, and Examination
#' tables. The full merged dataset contains all variables from every table in
#' those groups; the most commonly used ones are listed below. Measurement
#' variables only — administrative comment-code fields and SI-unit duplicates
#' are omitted for brevity.
#'
#' **Demographics (DEMO_J)**
#'
#' | Variable | Description |
#' |----------|-------------|
#' | RIDAGEYR | Age in years at screening (top-coded at 80) |
#' | RIDAGEMN | Age in months at screening (ages ≤ 24 months) |
#' | RIAGENDR | Gender (1 = Male, 2 = Female) |
#' | RIDRETH1 | Race/Hispanic origin |
#' | RIDRETH3 | Race/Hispanic origin (includes Non-Hispanic Asian) |
#' | RIDEXPRG | Pregnancy status (females 20–44 at exam) |
#' | DMDBORN4 | Country of birth |
#' | DMDCITZN | Citizenship status |
#' | DMDEDUC2 | Education level – adults 20+ |
#' | DMDEDUC3 | Education level – youth 6–19 |
#' | DMDMARTL | Marital status |
#' | DMDYRSUS | Years in the US |
#' | DMDFMSIZ | Total number of people in the family |
#' | DMDHHSIZ | Total number of people in the household |
#' | INDFMIN2 | Total family income (range value, USD) |
#' | INDFMPIR | Ratio of family income to poverty guidelines |
#' | INDHHIN2 | Total household income (range value, USD) |
#' | WTINT2YR | Full sample 2-year interview weight |
#' | WTMEC2YR | Full sample 2-year MEC exam weight |
#'
#' **Body Measures (BMX_J)**
#'
#' | Variable | Description |
#' |----------|-------------|
#' | BMXWT    | Weight (kg) |
#' | BMXHT    | Standing height (cm) |
#' | BMXBMI   | Body Mass Index (kg/m²) |
#' | BMXWAIST | Waist circumference (cm) |
#' | BMXHIP   | Hip circumference (cm) |
#' | BMXARMC  | Arm circumference (cm) |
#' | BMXARML  | Upper arm length (cm) |
#' | BMXLEG   | Upper leg length (cm) |
#'
#' **Blood Pressure & Pulse (BPX_J)**
#'
#' | Variable | Description |
#' |----------|-------------|
#' | BPXSY1   | Systolic blood pressure, 1st reading (mm Hg) |
#' | BPXSY2   | Systolic blood pressure, 2nd reading (mm Hg) |
#' | BPXSY3   | Systolic blood pressure, 3rd reading (mm Hg) |
#' | BPXDI1   | Diastolic blood pressure, 1st reading (mm Hg) |
#' | BPXDI2   | Diastolic blood pressure, 2nd reading (mm Hg) |
#' | BPXDI3   | Diastolic blood pressure, 3rd reading (mm Hg) |
#' | BPXPLS   | 60-second pulse (beats/min) |
#'
#' **Glycohemoglobin (GHB_J)**
#'
#' | Variable | Description |
#' |----------|-------------|
#' | LBXGH    | Glycohemoglobin / HbA1c (%) |
#'
#' **Standard Biochemistry Profile (BIOPRO_J)**
#'
#' | Variable  | Description |
#' |-----------|-------------|
#' | LBXSAL    | Albumin (g/dL) |
#' | LBXSBU    | Blood urea nitrogen / BUN (mg/dL) |
#' | LBXSCA    | Total calcium (mg/dL) |
#' | LBXSCR    | Creatinine, serum (mg/dL) |
#' | LBXSGL    | Glucose, serum (mg/dL) |
#' | LBXSGB    | Globulin (g/dL) |
#' | LBXSIR    | Iron, serum (ug/dL) |
#' | LBXSPH    | Phosphorus (mg/dL) |
#' | LBXSTB    | Total bilirubin (mg/dL) |
#' | LBXSTP    | Total protein (g/dL) |
#' | LBXSTR    | Triglycerides, serum (mg/dL) |
#' | LBXSUA    | Uric acid (mg/dL) |
#' | LBXSATSI  | Alanine aminotransferase / ALT (U/L) |
#' | LBXSASSI  | Aspartate aminotransferase / AST (U/L) |
#' | LBXSGTSI  | Gamma-glutamyl transferase / GGT (IU/L) |
#' | LBXSAPSI  | Alkaline phosphatase / ALP (IU/L) |
#' | LBXSCK    | Creatine phosphokinase / CPK (IU/L) |
#' | LBXSCH    | Total cholesterol, serum (mg/dL) |
#' | LBXSC3SI  | Bicarbonate (mmol/L) |
#' | LBXSCLSI  | Chloride (mmol/L) |
#' | LBXSKSI   | Potassium (mmol/L) |
#' | LBXSNASI  | Sodium (mmol/L) |
#' | LBXSOSSI  | Osmolality (mmol/kg) |
#' | LBXSLDSI  | Lactate dehydrogenase / LDH (IU/L) |
#'
#' **Complete Blood Count (CBC_J)**
#'
#' | Variable  | Description |
#' |-----------|-------------|
#' | LBXWBCSI  | White blood cell count (1000 cells/µL) |
#' | LBXRBCSI  | Red blood cell count (million cells/µL) |
#' | LBXHGB    | Hemoglobin (g/dL) |
#' | LBXHCT    | Hematocrit (%) |
#' | LBXMCVSI  | Mean cell volume (fL) |
#' | LBXMCHSI  | Mean cell hemoglobin (pg) |
#' | LBXMC     | Mean cell hemoglobin concentration (g/dL) |
#' | LBXRDW    | Red cell distribution width (%) |
#' | LBXPLTSI  | Platelet count (1000 cells/µL) |
#' | LBXMPSI   | Mean platelet volume (fL) |
#' | LBXLYPCT  | Lymphocyte percent (%) |
#' | LBDLYMNO  | Lymphocyte number (1000 cells/µL) |
#' | LBXNEPCT  | Segmented neutrophils percent (%) |
#' | LBDNENO   | Segmented neutrophils number (1000 cells/µL) |
#' | LBXMOPCT  | Monocyte percent (%) |
#' | LBDMONO   | Monocyte number (1000 cells/µL) |
#' | LBXEOPCT  | Eosinophils percent (%) |
#' | LBDEONO   | Eosinophils number (1000 cells/µL) |
#' | LBXBAPCT  | Basophils percent (%) |
#' | LBDBANO   | Basophils number (1000 cells/µL) |
#' | LBXNRBC   | Nucleated red blood cells (/100 WBC) |
#'
#' **Lipids**
#'
#' | Variable  | Table    | Description |
#' |-----------|----------|-------------|
#' | LBXTC     | TCHOL_J  | Total cholesterol (mg/dL) |
#' | LBDHDD    | HDL_J    | Direct HDL-cholesterol (mg/dL) |
#' | LBXTR     | TRIGLY_J | Triglycerides (mg/dL) |
#' | LBDLDL    | TRIGLY_J | LDL-cholesterol, Friedewald equation (mg/dL) |
#' | LBDLDLM   | TRIGLY_J | LDL-cholesterol, Martin-Hopkins equation (mg/dL) |
#' | LBDLDLN   | TRIGLY_J | LDL-cholesterol, NIH equation 2 (mg/dL) |
#'
#' **Urine Albumin & Creatinine (ALB_CR_J)**
#'
#' | Variable | Description |
#' |----------|-------------|
#' | URXUCR   | Creatinine, urine (mg/dL) |
#' | URXCRS   | Creatinine, urine (µmol/L) |
#' | URXUMA   | Albumin, urine (µg/mL) |
#' | URXUMS   | Albumin, urine (mg/L) |
#' | URDACT   | Albumin-creatinine ratio (mg/g) |
#'
#' The full merged dataset contains additional variables from all other
#' Laboratory and Examination tables downloaded for the requested year.
#' Use `names(readRDS(file.path(cache_dir, "nhanes_<year>.rds")))` to
#' inspect all available columns.
#'
#' @returns a data.frame with `n_subjects` rows and the requested covariates
#'   as columns.
#'
#' @export
sample_covariates_nhanes <- function(
  covariates  = NULL,
  year        = "2017-2018",
  n_subjects  = 100,
  conditional = NULL,
  use_weights = FALSE,
  seed        = NULL,
  dictionary  = NULL,
  na.rm       = TRUE,
  cache_dir   = nhanes_default_cache_dir(),
  ...
) {
  if (!is.null(seed)) set.seed(seed)

  data <- nhanes_load_merged(year, cache_dir)

  # Translate user names → NHANES names via dictionary
  covariates_nhanes  <- nhanes_translate(covariates,          dictionary)
  conditional_nhanes <- nhanes_translate_names(conditional,   dictionary)

  if (!is.null(covariates_nhanes)) {
    missing_covs <- setdiff(covariates_nhanes, names(data))
    if (length(missing_covs) > 0) {
      stop(
        "Covariates not found in NHANES data: ",
        paste(missing_covs, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # Drop rows with NAs in the requested covariates (or all non-SEQN columns)
  if (na.rm) {
    cols_to_check <- if (!is.null(covariates_nhanes)) {
      covariates_nhanes
    } else {
      setdiff(names(data), "SEQN")
    }
    data <- data[stats::complete.cases(data[, cols_to_check, drop = FALSE]), , drop = FALSE]
  }

  # Apply conditional filters before sampling
  if (!is.null(conditional_nhanes)) {
    for (key in names(conditional_nhanes)) {
      data <- dplyr::filter(
        data,
        .data[[key]] >= min(conditional_nhanes[[key]]) &
          .data[[key]] <= max(conditional_nhanes[[key]])
      )
    }
  }

  if (nrow(data) == 0) {
    stop(
      "No observations present within the conditional limits for the sampled population.",
      call. = FALSE
    )
  }

  # Sample with or without survey weights
  if (use_weights) {
    if (!"WTMEC2YR" %in% names(data)) {
      stop(
        "Survey weights (WTMEC2YR) not found in the cached data. ",
        "Re-run download_nhanes_cache() with groups including \"DEMO\".",
        call. = FALSE
      )
    }
    weights <- data$WTMEC2YR
    weights[is.na(weights)] <- 0
    idx  <- sample(seq_len(nrow(data)), size = n_subjects, replace = TRUE, prob = weights)
    data <- data[idx, ]
  } else {
    data <- dplyr::slice_sample(data, n = n_subjects, replace = TRUE)
  }

  # Select output columns (always drop SEQN)
  if (!is.null(covariates_nhanes)) {
    data <- dplyr::select(data, dplyr::all_of(covariates_nhanes))
  } else {
    data <- dplyr::select(data, -"SEQN")
  }

  # Rename NHANES names back to user-defined names
  if (!is.null(dictionary)) {
    nhanes_to_user <- setNames(names(dictionary), as.character(dictionary))
    idx <- match(names(data), names(nhanes_to_user))
    names(data)[!is.na(idx)] <- nhanes_to_user[na.omit(idx)]
  }

  data
}

#' Translate a character vector of names through a dictionary
#' @noRd
nhanes_translate <- function(names_vec, dictionary) {
  if (is.null(names_vec) || is.null(dictionary)) return(names_vec)
  vapply(names_vec, function(x) {
    if (x %in% names(dictionary)) dictionary[[x]] else x
  }, character(1), USE.NAMES = FALSE)
}

#' Translate the names of a named list (e.g. conditional) through a dictionary
#' @noRd
nhanes_translate_names <- function(named_list, dictionary) {
  if (is.null(named_list) || is.null(dictionary)) return(named_list)
  names(named_list) <- nhanes_translate(names(named_list), dictionary)
  named_list
}

#' Default cache directory inside the package installation folder
#' @noRd
nhanes_default_cache_dir <- function() {
  file.path(system.file(package = "irxforge"), "nhanes_cache")
}

#' Load a merged NHANES RDS for a given year
#' @noRd
nhanes_load_merged <- function(year, cache_dir) {
  nhanes_year_suffix(year)  # validate year
  if (!is.null(cache_dir)) {
    rds_path <- file.path(cache_dir, paste0("nhanes_", year, ".rds"))
    if (file.exists(rds_path)) {
      return(readRDS(rds_path))
    }
    stop(
      "NHANES cache not found for year ", year, ": ", rds_path, ".\n",
      "Run download_nhanes_cache(years = \"", year, "\") to create it.",
      call. = FALSE
    )
  }
  # cache_dir = NULL: download on demand (slow; use only for one-off calls)
  if (!requireNamespace("nhanesA", quietly = TRUE)) {
    stop(
      "No cached NHANES data found and 'nhanesA' is not installed. ",
      "Run download_nhanes_cache() or install nhanesA.",
      call. = FALSE
    )
  }
  message("No cache_dir supplied; downloading NHANES data on demand (this may be slow).")
  tmp <- tempfile()
  download_nhanes_cache(years = year, path = tmp)
  readRDS(file.path(tmp, paste0("nhanes_", year, ".rds")))
}

#' Map NHANES survey year to table name suffix
#' @noRd
nhanes_year_suffix <- function(year) {
  suffixes <- c(
    "1999-2000" = "",
    "2001-2002" = "_B",
    "2003-2004" = "_C",
    "2005-2006" = "_D",
    "2007-2008" = "_E",
    "2009-2010" = "_F",
    "2011-2012" = "_G",
    "2013-2014" = "_H",
    "2015-2016" = "_I",
    "2017-2018" = "_J",
    "2019-2020" = "_L"
  )
  if (!year %in% names(suffixes)) {
    stop(
      "Unsupported NHANES year: '", year, "'. ",
      "Supported years: ", paste(names(suffixes), collapse = ", "),
      call. = FALSE
    )
  }
  suffixes[[year]]
}
