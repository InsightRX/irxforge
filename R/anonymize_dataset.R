#' Anonymize a NONMEM-style dataset
#'
#' Creates an anonymized NONMEM-style dataset by renumbering subjects, sampling
#' new covariates, cloning observed dosing/observation designs, and replacing
#' observed concentrations with FeRx-simulated values.
#'
#' @param data original NONMEM-style `data.frame` or `tibble`.
#' @param covariates character vector of covariates to include in the
#'   anonymized dataset.
#' @param model_file path to a FeRx model file used to simulate concentrations.
#' @param dictionary named list mapping expected NONMEM column names to columns
#'   in `data`. Supported keys are `ID`, `EVID`, `AMT`, `TIME`, `DV`, and
#'   optionally `RATE`. Defaults use the same names.
#' @param lloq optional lower limit of quantification.
#' @param censoring how to handle simulated observations below `lloq`.
#'   `"drop"` removes below-LLOQ observation rows. `"cens"` adds a `CENS`
#'   column, with `1` for below-LLOQ observations and `0` otherwise.
#' @param seed optional integer seed used for covariate sampling and FeRx
#'   simulation.
#' @param ... additional arguments passed to
#'   [sample_covariates_mice_timevarying()].
#'
#' @returns anonymized NONMEM-style data.frame.
#'
#' @export
anonymize_dataset <- function(
  data,
  covariates,
  model_file,
  dictionary = list(
    ID = "ID",
    EVID = "EVID",
    AMT = "AMT",
    TIME = "TIME",
    DV = "DV",
    RATE = "RATE"
  ),
  lloq = NULL,
  censoring = c("drop", "cens"),
  seed = NULL,
  ...
) {
  censoring <- match.arg(censoring)
  data <- as.data.frame(data)
  dictionary <- normalize_anonymize_dictionary(dictionary)
  validate_anonymize_inputs(
    data = data,
    covariates = covariates,
    model_file = model_file,
    dictionary = dictionary,
    lloq = lloq
  )

  id_var <- dictionary$ID
  time_var <- dictionary$TIME
  evid_var <- dictionary$EVID
  amt_var <- dictionary$AMT
  dv_var <- dictionary$DV

  sample_data <- as.data.frame(data[, c(id_var, time_var, covariates), drop = FALSE])
  cat_covs <- covariates[
    vapply(sample_data[covariates], function(x) {
      is.factor(x) || is.character(x)
    }, logical(1))
  ]

  sampled_covs <- sample_covariates_mice_timevarying(
    data = sample_data,
    id_var = id_var,
    time_var = time_var,
    time_varying_covs = covariates,
    cat_covs = cat_covs,
    design_match = "propensity",
    design_match_covs = covariates,
    design_id_var = ".design_id",
    n_subjects = length(unique(data[[id_var]])),
    seed = seed,
    ...
  )

  anonymized <- build_anonymized_simulation_input(
    data = data,
    sampled_covs = sampled_covs,
    covariates = covariates,
    dictionary = dictionary
  )

  sim <- simulate_anonymized_concentrations(
    model_file = model_file,
    data = anonymized,
    seed = seed
  )
  anonymized <- apply_simulated_concentrations(
    data = anonymized,
    sim = sim,
    dictionary = dictionary
  )
  apply_anonymize_lloq(
    data = anonymized,
    lloq = lloq,
    censoring = censoring,
    dictionary = dictionary
  )
}

normalize_anonymize_dictionary <- function(dictionary) {
  defaults <- list(
    ID = "ID",
    EVID = "EVID",
    AMT = "AMT",
    TIME = "TIME",
    DV = "DV",
    RATE = "RATE"
  )
  aliases <- c(
    subject_id = "ID",
    subject = "ID",
    id = "ID",
    evid = "EVID",
    dose_amount = "AMT",
    dose = "AMT",
    amt = "AMT",
    time = "TIME",
    dependent = "DV",
    dv = "DV",
    conc = "DV",
    rate = "RATE"
  )
  for (alias in intersect(names(dictionary), names(aliases))) {
    dictionary[[aliases[[alias]]]] <- dictionary[[alias]]
  }
  utils::modifyList(defaults, dictionary)
}

validate_anonymize_inputs <- function(data, covariates, model_file, dictionary, lloq) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble.", call. = FALSE)
  }
  if (!is.character(covariates) || length(covariates) == 0) {
    stop("`covariates` must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.character(model_file) || length(model_file) != 1 || !file.exists(model_file)) {
    stop("`model_file` must point to an existing FeRx model file.", call. = FALSE)
  }
  required_keys <- c("ID", "EVID", "AMT", "TIME", "DV")
  missing_cols <- setdiff(unname(unlist(dictionary[required_keys])), names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Required mapped columns were not found in `data`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  missing_covs <- setdiff(covariates, names(data))
  if (length(missing_covs) > 0) {
    stop(
      "Covariates were not found in `data`: ",
      paste(missing_covs, collapse = ", "),
      call. = FALSE
    )
  }
  structural <- unique(unname(unlist(dictionary[c("ID", "EVID", "AMT", "TIME", "DV", "RATE")])))
  cov_overlap <- intersect(covariates, structural)
  if (length(cov_overlap) > 0) {
    stop(
      "`covariates` cannot include mapped NONMEM columns: ",
      paste(cov_overlap, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.null(lloq) && (!is.numeric(lloq) || length(lloq) != 1 || is.na(lloq))) {
    stop("`lloq` must be a single numeric value.", call. = FALSE)
  }
}

build_anonymized_simulation_input <- function(data, sampled_covs, covariates, dictionary) {
  id_var <- dictionary$ID
  time_var <- dictionary$TIME
  evid_var <- dictionary$EVID
  amt_var <- dictionary$AMT
  dv_var <- dictionary$DV
  rate_var <- dictionary$RATE

  if (!".design_id" %in% names(sampled_covs)) {
    stop("Internal error: sampled covariates do not contain `.design_id`.", call. = FALSE)
  }

  optional_event_cols <- intersect(c(rate_var, "CMT", "MDV", "ADDL", "II", "SS"), names(data))
  event_cols <- unique(c(id_var, time_var, evid_var, amt_var, optional_event_cols))

  out <- vector("list", length(unique(sampled_covs[[id_var]])))
  i <- 1L
  for (sim_id in unique(sampled_covs[[id_var]])) {
    cov_rows <- sampled_covs[sampled_covs[[id_var]] == sim_id, , drop = FALSE]
    design_id <- cov_rows[[".design_id"]][[1]]
    design_rows <- data[data[[id_var]] == design_id, , drop = FALSE]
    design_rows <- design_rows[order(design_rows[[time_var]]), , drop = FALSE]
    if (nrow(design_rows) != nrow(cov_rows)) {
      stop(
        "Matched design row count does not match sampled covariate row count.",
        call. = FALSE
      )
    }
    if (!isTRUE(all.equal(design_rows[[time_var]], cov_rows[[time_var]], check.attributes = FALSE))) {
      stop(
        "Matched design times do not match sampled covariate times.",
        call. = FALSE
      )
    }

    events <- design_rows[, event_cols, drop = FALSE]
    names(events)[names(events) == id_var] <- "ID"
    names(events)[names(events) == time_var] <- "TIME"
    names(events)[names(events) == evid_var] <- "EVID"
    names(events)[names(events) == amt_var] <- "AMT"
    if (rate_var %in% names(events)) names(events)[names(events) == rate_var] <- "RATE"
    events[["ID"]] <- sim_id
    events[["DV"]] <- 0

    cov_out <- cov_rows[, covariates, drop = FALSE]
    out[[i]] <- cbind(events, cov_out)
    i <- i + 1L
  }
  out <- dplyr::bind_rows(out)
  order_cols <- unique(c("ID", "TIME", "EVID", "AMT", "RATE", "DV", "CMT", "MDV", "ADDL", "II", "SS", covariates))
  out[, intersect(order_cols, names(out)), drop = FALSE]
}

simulate_anonymized_concentrations <- function(model_file, data, seed = NULL) {
  if (!requireNamespace("ferx", quietly = TRUE)) {
    stop("Package `ferx` is required to simulate anonymized concentrations.", call. = FALSE)
  }
  sim_seed <- if (is.null(seed)) 42L else as.integer(seed)
  sim_data <- data
  sim_data_path <- tempfile(fileext = ".csv")
  utils::write.csv(sim_data, sim_data_path, row.names = FALSE, na = ".")
  ferx::ferx_simulate(
    model = model_file,
    data = sim_data_path,
    n_sim = 1L,
    seed = sim_seed
  )
}

apply_simulated_concentrations <- function(data, sim, dictionary) {
  obs_idx <- which(data$EVID == 0)
  if (!"DV_SIM" %in% names(sim)) {
    stop("FeRx simulation output must contain a `DV_SIM` column.", call. = FALSE)
  }
  if (length(obs_idx) != nrow(sim)) {
    stop(
      "FeRx simulation row count does not match the number of observation records.",
      call. = FALSE
    )
  }
  data[["DV"]][obs_idx] <- sim[["DV_SIM"]]
  data
}

apply_anonymize_lloq <- function(data, lloq, censoring, dictionary) {
  if (is.null(lloq)) {
    return(data)
  }
  obs <- data$EVID == 0
  blq <- obs & data$DV < lloq
  if (censoring == "drop") {
    return(data[!blq, , drop = FALSE])
  }
  data$CENS <- 0L
  data$CENS[blq] <- 1L
  data
}
