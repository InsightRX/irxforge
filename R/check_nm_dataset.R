#' Run automated quality checks on a NONMEM dataset
#'
#' Performs a comprehensive set of checks on a NONMEM-style dataset and returns
#' a summary table with the result of each check. No errors are thrown; all
#' issues are reported as rows in the output table.
#'
#' @param data a data.frame (or tibble) containing the NONMEM dataset to check.
#' @param dictionary an optional named list that maps standard NONMEM column
#'   names to the actual column names in `data`. For example,
#'   `list(ID = "NMID", TIME = "TAD")` means the column called `"NMID"` in
#'   `data` plays the role of `ID`, and `"TAD"` plays the role of `TIME`.
#'   Any standard name not listed in the dictionary is assumed to match the
#'   column name in `data` as-is. Supported keys: `ID`, `TIME`, `DV`, `MDV`,
#'   `EVID`, `AMT`, `CMT`, `RATE`, `SS`, `ADDL`, `II`.
#' @param exclusion_flag character name of the column used as an exclusion flag
#'   (for `$DATA ACCEPT` / `IGNORE` in NONMEM). If `NULL` (default), the
#'   exclusion-flag check is skipped.
#' @param max_digits integer; threshold for flagging numbers with too many
#'   significant digits (default `10`).
#' @param max_id_digits integer; preferred maximum number of characters for the
#'   ID variable (default `5`).
#' @param extra_reserved character vector of additional reserved NONMEM names
#'   to check for (e.g. PK parameters used in ADVAN/TRANS such as `"CL"`,
#'   `"V"`, `"V1"`, `"K12"`).
#' @param verbose logical; if `TRUE` (default), messages are printed for every
#'   check as it is evaluated.
#'
#' @returns A data.frame with columns `check` (short test name), `description`
#'   (human-readable explanation), and `result` (`"PASS"`, `"FAIL"`, or
#'   `"NA"` when a check is not applicable).
#'
#' @export
check_nm_dataset <- function(
  data,
  dictionary = NULL,
  exclusion_flag = NULL,
  max_digits = 10,
  max_id_digits = 5,
  extra_reserved = NULL,
  verbose = TRUE
) {

  # ──────────────────────────────────────────────────────────────────────────

  # Resolve dictionary: map standard NONMEM names → actual column names

  # ──────────────────────────────────────────────────────────────────────────
  nm_keys <- c("ID", "TIME", "DV", "MDV", "EVID", "AMT", "CMT",
               "RATE", "SS", "ADDL", "II")
  col_map <- stats::setNames(nm_keys, nm_keys)          # default: name == column
  if (!is.null(dictionary)) {
    dict_upper <- stats::setNames(
      as.character(dictionary),
      toupper(names(dictionary))
    )
    unknown <- setdiff(names(dict_upper), nm_keys)
    if (length(unknown) > 0) {
      warning(
        "Dictionary keys not recognised (ignored): ",
        paste(unknown, collapse = ", ")
      )
    }
    for (k in intersect(names(dict_upper), nm_keys)) {
      col_map[[k]] <- dict_upper[[k]]
    }
    if (verbose) {
      mapped <- col_map[col_map != names(col_map)]
      if (length(mapped) > 0) {
        message(
          "Dictionary mapping: ",
          paste(names(mapped), "->", mapped, collapse = ", ")
        )
      }
    }
  }

  # Helper: resolve a standard NONMEM name to its actual column name
  col_for <- function(nm_name) col_map[[nm_name]]

  results <- data.frame(
    check = character(0),
    description = character(0),
    result = character(0),
    stringsAsFactors = FALSE
  )

  log_check <- function(check, description, result) {
    if (verbose) {
      symbol <- switch(result, PASS = "[PASS]", FAIL = "[FAIL]", "[ NA ]")
      message(sprintf("%-8s %s", symbol, description))
    }
    results[nrow(results) + 1, ] <<- list(check, description, result)
  }

  # Column accessor helpers — all use dictionary-resolved names
  has_col <- function(nm_name) col_for(nm_name) %in% names(data)
  get_col <- function(nm_name) data[[col_for(nm_name)]]
  cols <- toupper(names(data))

  # ──────────────────────────────────────────────────────────────────────────
  # Required columns
  # ──────────────────────────────────────────────────────────────────────────
  required <- c("ID", "TIME", "DV")
  present <- vapply(required, function(k) col_for(k) %in% names(data), logical(1))
  log_check(
    "required_cols",
    paste0(
      "Minimum required columns present (",
      paste(vapply(required, col_for, character(1)), collapse = ", "), ")"
    ),
    if (all(present)) "PASS" else "FAIL"
  )

  # ──────────────────────────────────────────────────────────────────────────
  # Unique column names
  # ──────────────────────────────────────────────────────────────────────────
  log_check(
    "unique_colnames",
    "Column names are unique",
    if (anyDuplicated(names(data)) == 0) "PASS" else "FAIL"
  )

  # ──────────────────────────────────────────────────────────────────────────
  # No special characters in column names
  # ──────────────────────────────────────────────────────────────────────────
  bad_names <- grep("[^A-Za-z0-9_]", names(data), value = TRUE)
  log_check(
    "colname_chars",
    "Column names contain no special characters",
    if (length(bad_names) == 0) "PASS" else "FAIL"
  )

  # ──────────────────────────────────────────────────────────────────────────
  # Reserved NONMEM names
  # ──────────────────────────────────────────────────────────────────────────
  reserved <- c(
    "L1", "L2", "DATE", "DAT1", "DAT2", "DAT3",
    "PCMT", "CALL", "CONT", "NEWIND", "W",
    "ETA", "THETA", "EPS", "ERR",
    "IPRED", "IRES", "IWRES", "CWRES", "PHI"
  )
  if (!is.null(extra_reserved)) {
    reserved <- unique(c(reserved, toupper(extra_reserved)))
  }
  used_reserved <- intersect(cols, reserved)
  log_check(
    "reserved_names",
    "No reserved NONMEM variable names used as column names",
    if (length(used_reserved) == 0) "PASS" else "FAIL"
  )

  # ──────────────────────────────────────────────────────────────────────────
  # Character columns warning
  # ──────────────────────────────────────────────────────────────────────────
  char_cols <- names(data)[vapply(data, is.character, logical(1))]
  log_check(
    "char_columns",
    "No character-type columns present (may cause NONMEM compilation failure)",
    if (length(char_cols) == 0) "PASS" else "FAIL"
  )

  # ──────────────────────────────────────────────────────────────────────────
  # NA represented as "." not "NA"
  # ──────────────────────────────────────────────────────────────────────────
  has_na_string <- any(vapply(data, function(x) any(x == "NA", na.rm = TRUE), logical(1)))
  log_check(
    "na_coding",
    "Missing values coded as '.' and not as string 'NA'",
    if (!has_na_string) "PASS" else "FAIL"
  )

  # ──────────────────────────────────────────────────────────────────────────
  # Commas inside character values (problematic for CSV)
  # ──────────────────────────────────────────────────────────────────────────
  has_commas <- any(vapply(
    data[vapply(data, is.character, logical(1))],
    function(x) any(grepl(",", x, fixed = TRUE), na.rm = TRUE),
    logical(1)
  ))
  log_check(
    "csv_commas",
    "Character values do not contain commas (safe for CSV)",
    if (!has_commas) "PASS" else "FAIL"
  )

  # ──────────────────────────────────────────────────────────────────────────
  # Exclusion flag
  # ──────────────────────────────────────────────────────────────────────────
  if (!is.null(exclusion_flag)) {
    if (exclusion_flag %in% names(data)) {
      ef <- data[[exclusion_flag]]
      ef_ok <- !any(is.na(ef)) && is.numeric(ef) && all(ef == round(ef))
      log_check(
        "exclusion_flag",
        paste0("Exclusion flag '", exclusion_flag, "' is non-missing and integer"),
        if (ef_ok) "PASS" else "FAIL"
      )
    } else {
      log_check(
        "exclusion_flag",
        paste0("Exclusion flag '", exclusion_flag, "' exists in dataset"),
        "FAIL"
      )
    }
  } else {
    log_check(
      "exclusion_flag",
      "Exclusion flag check (not specified)",
      "NA"
    )
  }

  # ──────────────────────────────────────────────────────────────────────────
  # TIME checks
  # ──────────────────────────────────────────────────────────────────────────
  if (has_col("TIME")) {
    time <- get_col("TIME")
    log_check(
      "time_numeric",
      "TIME is numeric",
      if (is.numeric(time)) "PASS" else "FAIL"
    )
    if (is.numeric(time)) {
      log_check(
        "time_nonneg",
        "TIME is non-negative",
        if (all(time >= 0, na.rm = TRUE)) "PASS" else "FAIL"
      )
    }
  }

  # ──────────────────────────────────────────────────────────────────────────
  # TIME non-decreasing within ID (except EVID 3, 4)
  # ──────────────────────────────────────────────────────────────────────────
  if (has_col("TIME") && has_col("ID") && is.numeric(get_col("TIME"))) {
    time_ok <- TRUE
    id_vec <- get_col("ID")
    time_vec <- get_col("TIME")
    evid_vec <- if (has_col("EVID")) get_col("EVID") else NULL
    for (id in unique(id_vec)) {
      idx <- id_vec == id
      t <- time_vec[idx]
      if (!is.null(evid_vec)) {
        keep <- !(evid_vec[idx] %in% c(3, 4))
        t <- t[keep]
      }
      if (length(t) > 1 && any(diff(t) < 0, na.rm = TRUE)) {
        time_ok <- FALSE
        break
      }
    }
    log_check(
      "time_order",
      "TIME is non-decreasing within each ID (excluding EVID 3/4 reset events)",
      if (time_ok) "PASS" else "FAIL"
    )
  }

  # ──────────────────────────────────────────────────────────────────────────
  # EVID checks
  # ──────────────────────────────────────────────────────────────────────────
  if (has_col("EVID")) {
    evid <- get_col("EVID")
    log_check(
      "evid_values",
      "EVID values are in {0, 1, 2, 3, 4}",
      if (all(evid %in% c(0, 1, 2, 3, 4), na.rm = TRUE)) "PASS" else "FAIL"
    )
  }

  # ──────────────────────────────────────────────────────────────────────────
  # CMT checks
  # ──────────────────────────────────────────────────────────────────────────
  if (has_col("CMT")) {
    cmt <- get_col("CMT")
    log_check(
      "cmt_integer",
      "CMT values are integers",
      if (is.numeric(cmt) && all(cmt == round(cmt), na.rm = TRUE)) "PASS" else "FAIL"
    )
    if (has_col("EVID")) {
      evid <- get_col("EVID")
      non_reset <- evid != 3
      cmt_sub <- cmt[non_reset]
      cmt_sub <- cmt_sub[!is.na(cmt_sub)]
      log_check(
        "cmt_positive",
        "CMT is non-zero for non-reset (EVID!=3) records (negative allowed for emptying)",
        if (all(cmt_sub != 0)) "PASS" else "FAIL"
      )
    }
  }

  # ──────────────────────────────────────────────────────────────────────────
  # DV checks
  # ──────────────────────────────────────────────────────────────────────────
  if (has_col("DV")) {
    dv <- get_col("DV")
    log_check(
      "dv_numeric",
      "DV is numeric",
      if (is.numeric(dv)) "PASS" else "FAIL"
    )
    if (has_col("EVID")) {
      evid <- get_col("EVID")
      dose_evids <- evid %in% c(1, 4)
      dv_at_dose <- dv[dose_evids]
      log_check(
        "dv_missing_dose",
        "DV is missing (NA or 0) for dosing records (EVID 1, 4)",
        if (all(is.na(dv_at_dose) | dv_at_dose == 0)) "PASS" else "FAIL"
      )
    }
    if (has_col("EVID") && is.numeric(dv)) {
      evid <- get_col("EVID")
      obs_dv <- dv[evid == 0]
      log_check(
        "dv_obs_nonmissing",
        "DV is non-missing for observation records (EVID 0)",
        if (!any(is.na(obs_dv))) "PASS" else "FAIL"
      )
    }
  }

  # ──────────────────────────────────────────────────────────────────────────
  # MDV checks
  # ──────────────────────────────────────────────────────────────────────────
  if (has_col("MDV")) {
    mdv <- get_col("MDV")
    log_check(
      "mdv_binary",
      "MDV is binary (0 or 1)",
      if (all(mdv %in% c(0, 1), na.rm = TRUE)) "PASS" else "FAIL"
    )
    if (has_col("DV") && is.numeric(get_col("DV")) && has_col("EVID")) {
      dv <- get_col("DV")
      evid <- get_col("EVID")
      obs <- evid == 0
      expected_mdv <- ifelse(is.na(dv[obs]), 1, 0)
      log_check(
        "mdv_dv_consistent",
        "MDV is consistent with is.na(DV) for observation records (EVID 0)",
        if (all(mdv[obs] == expected_mdv, na.rm = TRUE)) "PASS" else "FAIL"
      )
      dosing <- evid %in% c(1, 4)
      log_check(
        "mdv_dosing",
        "MDV is 1 for dosing records (EVID 1, 4)",
        if (all(mdv[dosing] == 1, na.rm = TRUE)) "PASS" else "FAIL"
      )
    }
    if (has_col("DV") && is.numeric(get_col("DV"))) {
      dv <- get_col("DV")
      nonmissing_obs <- !is.na(dv) & dv != 0
      if (has_col("EVID")) {
        nonmissing_obs <- nonmissing_obs & get_col("EVID") == 0
      }
      log_check(
        "mdv_nonmissing_obs",
        "MDV is 0 for non-missing observation records (DV > 0)",
        if (all(mdv[nonmissing_obs] == 0, na.rm = TRUE)) "PASS" else "FAIL"
      )
    }
  }

  # ──────────────────────────────────────────────────────────────────────────
  # AMT checks
  # ──────────────────────────────────────────────────────────────────────────
  if (has_col("AMT")) {
    amt <- get_col("AMT")
    log_check(
      "amt_numeric",
      "AMT is numeric",
      if (is.numeric(amt)) "PASS" else "FAIL"
    )
    if (has_col("EVID") && is.numeric(amt)) {
      evid <- get_col("EVID")
      obs_other <- evid %in% c(0, 2)
      amt_obs <- amt[obs_other]
      log_check(
        "amt_zero_obs",
        "AMT is 0 or missing for observation/other-type records (EVID 0, 2)",
        if (all(is.na(amt_obs) | amt_obs == 0)) "PASS" else "FAIL"
      )
      dose_recs <- evid %in% c(1, 4)
      amt_dose <- amt[dose_recs]
      log_check(
        "amt_positive_dose",
        "AMT is positive for dosing records (EVID 1, 4)",
        if (all(amt_dose > 0, na.rm = TRUE)) "PASS" else "FAIL"
      )
    }
    if (has_col("EVID") && is.numeric(amt)) {
      evid <- get_col("EVID")
      amt_pos <- !is.na(amt) & amt > 0
      log_check(
        "amt_evid_consistent",
        "Records with AMT > 0 have EVID = 1 (or 4)",
        if (all(evid[amt_pos] %in% c(1, 4))) "PASS" else "FAIL"
      )
    }
    if (has_col("EVID")) {
      evid <- get_col("EVID")
      dose_recs <- evid %in% c(1, 4)
      log_check(
        "amt_nonmissing_dose",
        "AMT is non-missing for dosing records (EVID 1, 4)",
        if (!any(is.na(amt[dose_recs]))) "PASS" else "FAIL"
      )
    }
  }

  # ──────────────────────────────────────────────────────────────────────────
  # DV / MDV / BLQ consistency
  # ──────────────────────────────────────────────────────────────────────────
  if (has_col("DV") && has_col("MDV") && has_col("EVID") && is.numeric(get_col("DV"))) {
    dv <- get_col("DV")
    mdv <- get_col("MDV")
    evid <- get_col("EVID")
    obs <- evid == 0
    dv_leq_zero <- obs & !is.na(dv) & dv <= 0
    log_check(
      "dv_leq_zero_mdv",
      "DV <= 0 observations have MDV = 1 (or BLQ flag)",
      if (sum(dv_leq_zero) == 0 || all(mdv[dv_leq_zero] == 1)) "PASS" else "FAIL"
    )
  }

  # ──────────────────────────────────────────────────────────────────────────
  # RATE checks
  # ──────────────────────────────────────────────────────────────────────────
  if (has_col("RATE")) {
    rate <- get_col("RATE")
    log_check(
      "rate_numeric",
      "RATE is numeric",
      if (is.numeric(rate)) "PASS" else "FAIL"
    )
    if (is.numeric(rate) && has_col("EVID")) {
      evid <- get_col("EVID")
      dose_recs <- evid %in% c(1, 4)
      rate_dose <- rate[dose_recs]
      rate_dose <- rate_dose[!is.na(rate_dose)]
      log_check(
        "rate_dose_values",
        "RATE for dosing records is -2, -1, 0, or positive",
        if (all(rate_dose %in% c(-2, -1, 0) | rate_dose > 0)) "PASS" else "FAIL"
      )
    }
  } else {
    log_check("rate_present", "RATE column present", "NA")
  }

  # ──────────────────────────────────────────────────────────────────────────
  # SS checks
  # ──────────────────────────────────────────────────────────────────────────
  if (has_col("SS")) {
    ss <- get_col("SS")
    log_check(
      "ss_numeric",
      "SS is numeric",
      if (is.numeric(ss)) "PASS" else "FAIL"
    )
    if (is.numeric(ss) && has_col("EVID")) {
      evid <- get_col("EVID")
      dose_recs <- evid %in% c(1, 4)
      ss_dose <- ss[dose_recs]
      ss_dose <- ss_dose[!is.na(ss_dose)]
      log_check(
        "ss_values",
        "SS for dosing records is 0 or 1",
        if (all(ss_dose %in% c(0, 1))) "PASS" else "FAIL"
      )
    }
  } else {
    log_check("ss_present", "SS column present", "NA")
  }

  # ──────────────────────────────────────────────────────────────────────────
  # ADDL / II checks
  # ──────────────────────────────────────────────────────────────────────────
  if (has_col("ADDL") || has_col("II")) {
    log_check(
      "addl_ii_paired",
      "ADDL and II are both present (required together)",
      if (has_col("ADDL") && has_col("II")) "PASS" else "FAIL"
    )
    if (has_col("ADDL")) {
      addl <- get_col("ADDL")
      log_check(
        "addl_nonneg_int",
        "ADDL is a non-negative integer for dosing records",
        if (is.numeric(addl) && has_col("EVID")) {
          evid <- get_col("EVID")
          dose_recs <- evid %in% c(1, 4)
          addl_d <- addl[dose_recs]
          addl_d <- addl_d[!is.na(addl_d)]
          all(addl_d >= 0) && all(addl_d == round(addl_d))
        } else {
          FALSE
        }
      )
    }
    if (has_col("II")) {
      ii <- get_col("II")
      log_check(
        "ii_nonneg_int",
        "II is a non-negative integer for dosing records",
        if (is.numeric(ii) && has_col("EVID")) {
          evid <- get_col("EVID")
          dose_recs <- evid %in% c(1, 4)
          ii_d <- ii[dose_recs]
          ii_d <- ii_d[!is.na(ii_d)]
          all(ii_d >= 0) && all(ii_d == round(ii_d))
        } else {
          FALSE
        }
      )
    }
  } else {
    log_check("addl_ii_present", "ADDL/II columns present", "NA")
  }

  # ──────────────────────────────────────────────────────────────────────────
  # Overlapping ADDL-expanded doses
  # ──────────────────────────────────────────────────────────────────────────
  if (has_col("ADDL") && has_col("II") && has_col("EVID") && has_col("ID") &&
      has_col("TIME") && is.numeric(get_col("ADDL")) && is.numeric(get_col("II"))) {
    overlap_found <- FALSE
    id_vec <- get_col("ID")
    time_vec <- get_col("TIME")
    evid_vec <- get_col("EVID")
    addl_vec <- get_col("ADDL")
    ii_vec <- get_col("II")
    for (id in unique(id_vec)) {
      idx <- which(id_vec == id)
      dose_idx <- idx[evid_vec[idx] %in% c(1, 4)]
      if (length(dose_idx) < 2) next
      for (i in seq_len(length(dose_idx) - 1)) {
        ri <- dose_idx[i]
        rn <- dose_idx[i + 1]
        addl_i <- addl_vec[ri]
        ii_i <- ii_vec[ri]
        if (!is.na(addl_i) && !is.na(ii_i) && addl_i > 0 && ii_i > 0) {
          last_expanded <- time_vec[ri] + addl_i * ii_i
          if (last_expanded >= time_vec[rn]) {
            overlap_found <- TRUE
            break
          }
        }
      }
      if (overlap_found) break
    }
    log_check(
      "addl_overlap",
      "No overlapping ADDL-expanded doses (last expanded dose before next explicit dose)",
      if (!overlap_found) "PASS" else "FAIL"
    )
  }

  # ──────────────────────────────────────────────────────────────────────────
  # ID checks
  # ──────────────────────────────────────────────────────────────────────────
  if (has_col("ID")) {
    ids <- as.character(get_col("ID"))
    has_leading_zeros <- any(grepl("^0\\d+", ids))
    log_check(
      "id_leading_zeros",
      "IDs do not have leading zeros (would be lost when NONMEM reads the data)",
      if (!has_leading_zeros) "PASS" else "FAIL"
    )
    max_len <- max(nchar(ids))
    log_check(
      "id_digits",
      paste0("ID variable has <= ", max_id_digits, " digits (longer may need $TABLE FORMAT)"),
      if (max_len <= max_id_digits) "PASS" else "FAIL"
    )
  }

  # ──────────────────────────────────────────────────────────────────────────
  # Unique records (ID, TIME, CMT, EVID)
  # ──────────────────────────────────────────────────────────────────────────
  dup_nm_keys <- c("ID", "TIME", "CMT", "EVID")
  dup_actual <- vapply(dup_nm_keys, col_for, character(1))
  dup_cols <- dup_actual[dup_actual %in% names(data)]
  if (length(dup_cols) >= 2) {
    dup_check <- duplicated(data[, dup_cols, drop = FALSE])
    log_check(
      "unique_records",
      paste0("Unique combination of ", paste(dup_cols, collapse = ", ")),
      if (!any(dup_check)) "PASS" else "FAIL"
    )
  }

  # ──────────────────────────────────────────────────────────────────────────
  # Dataset ordering (ID, TIME)
  # ──────────────────────────────────────────────────────────────────────────
  if (has_col("ID") && has_col("TIME")) {
    id_vec <- get_col("ID")
    time_vec <- get_col("TIME")
    ord <- order(id_vec, time_vec)
    is_sorted <- identical(
      paste(id_vec, time_vec),
      paste(id_vec[ord], time_vec[ord])
    )
    log_check(
      "data_ordered",
      "Dataset is ordered by at least ID and TIME",
      if (is_sorted) "PASS" else "FAIL"
    )
  }

  # ──────────────────────────────────────────────────────────────────────────
  # Large numbers (precision issues)
  # ──────────────────────────────────────────────────────────────────────────
  num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  if (length(num_cols) > 0) {
    too_many_digits <- FALSE
    for (col in num_cols) {
      vals <- data[[col]]
      vals <- vals[!is.na(vals) & vals != 0]
      if (length(vals) > 0) {
        n_digits <- nchar(gsub("[^0-9]", "", format(vals, scientific = FALSE)))
        if (any(n_digits > max_digits)) {
          too_many_digits <- TRUE
          break
        }
      }
    }
    log_check(
      "precision",
      paste0("Numeric values have <= ", max_digits, " significant digits"),
      if (!too_many_digits) "PASS" else "FAIL"
    )
  }

  # ──────────────────────────────────────────────────────────────────────────
  # Missing covariate values
  # ──────────────────────────────────────────────────────────────────────────
  # Covariates are columns that are not standard NONMEM columns (mapped or not)
  nm_standard <- c(
    "ID", "TIME", "DV", "MDV", "EVID", "AMT", "RATE", "SS",
    "II", "ADDL", "CMT", "GROUP", "BLQ", "LLOQ", "TAD"
  )
  # Include both the standard names and the dictionary-mapped actual names
  all_standard <- unique(c(
    nm_standard,
    vapply(intersect(nm_keys, nm_standard), col_for, character(1))
  ))
  cov_cols <- setdiff(
    names(data),
    c(all_standard, if (!is.null(exclusion_flag)) exclusion_flag)
  )
  if (length(cov_cols) > 0) {
    has_missing_cov <- FALSE
    for (col in cov_cols) {
      vals <- data[[col]]
      has_sentinel <- FALSE

      if (is.numeric(vals)) {
        has_sentinel <- any(vals %in% c(-999, -99), na.rm = TRUE)
      } else if (is.character(vals) || is.factor(vals)) {
        has_sentinel <- any(as.character(vals) %in% c(".", "NA"), na.rm = TRUE)
      }

      if (any(is.na(vals)) || has_sentinel) {
        has_missing_cov <- TRUE
        break
      }
    }
    log_check(
      "covariate_missing",
      "No missing or sentinel values (-999, -99, '.', 'NA') in covariate columns",
      if (!has_missing_cov) "PASS" else "FAIL"
    )
  }

  # ──────────────────────────────────────────────────────────────────────────
  # RATE column consistency with AMT
  # ──────────────────────────────────────────────────────────────────────────
  if (has_col("RATE") && has_col("AMT") &&
      is.numeric(get_col("RATE")) && is.numeric(get_col("AMT"))) {
    if (has_col("EVID")) {
      evid <- get_col("EVID")
      dose_recs <- evid %in% c(1, 4)
      rate_dose <- get_col("RATE")[dose_recs]
      amt_dose <- get_col("AMT")[dose_recs]
      real_rate <- !is.na(rate_dose) & !is.na(amt_dose) & rate_dose > 0
      log_check(
        "rate_amt_consistency",
        "Dosing records with positive RATE (mg/h) also have positive AMT",
        if (sum(real_rate) == 0 || all(amt_dose[real_rate] > 0)) "PASS" else "FAIL"
      )
    }
  }

  if (verbose) {
    n_pass <- sum(results$result == "PASS")
    n_fail <- sum(results$result == "FAIL")
    n_na <- sum(results$result == "NA")
    message(sprintf(
      "\n--- Summary: %d checks | %d PASS | %d FAIL | %d NA ---",
      nrow(results), n_pass, n_fail, n_na
    ))
  }

  results
}
