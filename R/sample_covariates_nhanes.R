#' Sample covariates from the NHANES database
#'
#' @param tables character vector of NHANES table names to download and merge,
#'   e.g. `c("DEMO", "BMX")`. Tables are merged on SEQN (respondent sequence
#'   number) using an inner join, so only subjects with data in all requested
#'   tables are retained.
#' @param variables character vector of variable names to include in the
#'   output. If `NULL` (default), all variables from all tables are included
#'   (SEQN is always dropped from output).
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
#'   the downloaded data (i.e., `"DEMO"` must be included in `tables`).
#'   Default is `FALSE` (simple random sampling with replacement).
#' @param ... additional arguments (currently unused)
#'
#' @details
#' Requires the `nhanesA` package. Data is downloaded from the CDC NHANES
#' website and cached locally by `nhanesA`.
#'
#' NHANES uses a complex multi-stage sampling design. Survey weights reflect
#' the probability of selection and non-response. Use `use_weights = TRUE` to
#' account for this when sampling. See the NHANES analytic guidelines for
#' details.
#'
#' @returns a data.frame with `n_subjects` rows and the requested variables as
#'   columns.
#'
#' @export
sample_covariates_nhanes <- function(
  tables,
  variables = NULL,
  year = "2017-2018",
  n_subjects = 100,
  conditional = NULL,
  use_weights = FALSE,
  ...
) {
  if (!requireNamespace("nhanesA", quietly = TRUE)) {
    stop(
      "Package 'nhanesA' is required for NHANES sampling. ",
      "Install it with: install.packages('nhanesA')",
      call. = FALSE
    )
  }

  year_suffix <- nhanes_year_suffix(year)

  # Download and merge tables on SEQN
  data <- NULL
  for (table in tables) {
    tbl_data <- nhanesA::nhanes(paste0(table, year_suffix))
    if (is.null(data)) {
      data <- tbl_data
    } else {
      data <- dplyr::inner_join(data, tbl_data, by = "SEQN")
    }
  }

  if (!is.null(variables)) {
    missing_vars <- setdiff(variables, names(data))
    if (length(missing_vars) > 0) {
      stop(
        "Variables not found in the downloaded tables: ",
        paste(missing_vars, collapse = ", "),
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
        "Survey weights (WTMEC2YR) not found. ",
        "Include \"DEMO\" in `tables` to enable weighted sampling.",
        call. = FALSE
      )
    }
    weights <- data$WTMEC2YR
    weights[is.na(weights)] <- 0
    idx <- sample(seq_len(nrow(data)), size = n_subjects, replace = TRUE, prob = weights)
    data <- data[idx, ]
  } else {
    data <- dplyr::slice_sample(data, n = n_subjects, replace = TRUE)
  }

  # Select output variables and drop SEQN
  if (!is.null(variables)) {
    data <- dplyr::select(data, dplyr::all_of(variables))
  } else {
    data <- dplyr::select(data, -"SEQN")
  }

  data
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
