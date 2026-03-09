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
#' @returns a data.frame with `n_subjects` rows and the requested covariates
#'   as columns.
#'
#' @export
sample_covariates_nhanes <- function(
  covariates = NULL,
  year       = "2017-2018",
  n_subjects = 100,
  conditional = NULL,
  use_weights = FALSE,
  cache_dir   = nhanes_default_cache_dir(),
  ...
) {
  data <- nhanes_load_merged(year, cache_dir)

  if (!is.null(covariates)) {
    missing_covs <- setdiff(covariates, names(data))
    if (length(missing_covs) > 0) {
      stop(
        "Covariates not found in NHANES data: ",
        paste(missing_covs, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # Apply conditional filters before sampling
  if (!is.null(conditional)) {
    for (key in names(conditional)) {
      data <- dplyr::filter(
        data,
        .data[[key]] >= min(conditional[[key]]) &
          .data[[key]] <= max(conditional[[key]])
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
  if (!is.null(covariates)) {
    data <- dplyr::select(data, dplyr::all_of(covariates))
  } else {
    data <- dplyr::select(data, -"SEQN")
  }

  data
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
