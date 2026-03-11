#' Download and cache NHANES data locally
#'
#' Downloads all tables within the specified NHANES data groups for one or
#' more survey years, merges them into a single data frame per year, and
#' saves it as an RDS file. The resulting cache is used automatically by
#' [sample_covariates_nhanes()].
#'
#' @param groups character vector of NHANES data groups to download. Valid
#'   values: `"DEMO"` (Demographics), `"LAB"` (Laboratory), `"EXAM"`
#'   (Examination), `"Q"` (Questionnaire), `"DIET"` (Dietary).
#'   Defaults to `c("DEMO", "LAB", "EXAM")`.
#' @param years character vector of NHANES survey cycles, e.g.
#'   `c("2015-2016", "2017-2018")`. Defaults to `"2017-2018"`.
#' @param path directory where merged RDS files will be saved. Created
#'   automatically if it does not exist. Defaults to the package-level cache
#'   directory returned by `nhanes_default_cache_dir()`.
#' @param overwrite logical. If `FALSE` (default), a year that already has a
#'   merged RDS in `path` is skipped. Set to `TRUE` to re-download and
#'   overwrite.
#' @param ... additional arguments (currently unused)
#'
#' @details
#' Each survey year is saved as a single file `nhanes_<year>.rds`
#' (e.g. `nhanes_2017-2018.rds`) containing all variables from all downloaded
#' tables, merged on the SEQN respondent sequence number. Tables with
#' multiple rows per subject (e.g. dietary recall) are skipped automatically.
#'
#' Requires the `nhanesA` package.
#'
#' @returns the `path` directory, invisibly.
#'
#' @export
download_nhanes_cache <- function(
  groups   = c("DEMO", "LAB", "EXAM"),
  years    = "2017-2018",
  path     = nhanes_default_cache_dir(),
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
    nhanes_year_suffix(year)  # validates year; errors on unsupported values
    out_file <- file.path(path, paste0("nhanes_", year, ".rds"))

    if (file.exists(out_file) && !overwrite) {
      message("Skipping ", year, " (cache exists; use overwrite = TRUE to re-download)")
      next
    }

    year_end <- as.integer(sub(".*-", "", year))
    table_list <- list()

    for (group in groups) {
      message("Fetching table list for group ", group, ", year ", year, " ...")
      tbl_info  <- nhanesA::nhanesTables(group, year_end)
      # nhanesTables() returns a data.frame; table names are in Data.File.Name
      tbl_names <- tbl_info[["Data.File.Name"]]

      for (tbl_name in tbl_names) {
        message("  Downloading ", tbl_name, " ...")
        tbl_data <- tryCatch(
          nhanesA::nhanes(tbl_name),
          error = function(e) {
            message("  Skipping ", tbl_name, ": ", conditionMessage(e))
            NULL
          }
        )
        if (is.null(tbl_data) || !"SEQN" %in% names(tbl_data)) next
        if (anyDuplicated(tbl_data[["SEQN"]]) > 0) {
          message("  Skipping ", tbl_name, " (multiple rows per subject)")
          next
        }
        table_list[[tbl_name]] <- tbl_data
      }
    }

    if (length(table_list) == 0) {
      warning("No tables downloaded for year ", year, "; skipping.")
      next
    }

    message("Merging ", length(table_list), " tables for ", year, " ...")
    merged <- Reduce(
      function(a, b) dplyr::full_join(a, b, by = "SEQN"),
      table_list
    )

    saveRDS(merged, out_file)
    message("Saved merged NHANES data (", nrow(merged), " subjects, ",
            ncol(merged), " variables) to ", out_file)
  }

  invisible(path)
}
