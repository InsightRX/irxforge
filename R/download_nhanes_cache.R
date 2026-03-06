#' Download and cache NHANES tables locally
#'
#' Downloads NHANES tables for one or more survey years and saves each as an
#' RDS file, so that [sample_covariates_nhanes()] can be used offline via its
#' `cache_dir` argument.
#'
#' @param tables character vector of NHANES table names to download,
#'   e.g. `c("DEMO", "BMX")`.
#' @param years character vector of NHANES survey cycles to download,
#'   e.g. `c("2015-2016", "2017-2018")`. Defaults to `"2017-2018"`.
#' @param path directory where RDS files will be saved. Created automatically
#'   if it does not exist. Defaults to `"nhanes_cache"`.
#' @param overwrite logical. If `FALSE` (default), tables that already exist
#'   in `path` are skipped. Set to `TRUE` to re-download and overwrite.
#' @param ... additional arguments (currently unused)
#'
#' @details
#' Each downloaded table is saved as `{TABLE}{SUFFIX}.rds`
#' (e.g. `DEMO_J.rds`, `BMX_J.rds`) where the suffix encodes the survey
#' year. Pass the same `path` as `cache_dir` in [sample_covariates_nhanes()]
#' to use the cached files.
#'
#' Requires the `nhanesA` package.
#'
#' @returns the `path` directory, invisibly.
#'
#' @export
download_nhanes_cache <- function(
  tables,
  years = "2017-2018",
  path = "nhanes_cache",
  overwrite = FALSE,
  ...
) {
  if (!requireNamespace("nhanesA", quietly = TRUE)) {
    stop(
      "Package 'nhanesA' is required to download NHANES data. ",
      "Install it with: install.packages('nhanesA')",
      call. = FALSE
    )
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  for (year in years) {
    suffix <- nhanes_year_suffix(year)
    for (table in tables) {
      file_name <- paste0(table, suffix, ".rds")
      file_path <- file.path(path, file_name)
      if (file.exists(file_path) && !overwrite) {
        message("Skipping ", file_name, " (already exists; use overwrite = TRUE to re-download)")
        next
      }
      message("Downloading ", table, " for ", year, " ...")
      tbl_data <- nhanesA::nhanes(paste0(table, suffix))
      saveRDS(tbl_data, file_path)
      message("  Saved to ", file_path)
    }
  }

  invisible(path)
}
