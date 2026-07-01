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
#' @param method time-varying covariate sampler used to resample the covariates.
#'   `"mice"` (default) uses [sample_covariates_mice_timevarying()], `"lme"` uses
#'   [sample_covariates_lme_timevarying()], and `"copulas"` uses
#'   [sample_covariates_copulas_timevarying()]. Only `"mice"` supports
#'   categorical covariates; `"lme"` and `"copulas"` require all covariates to be
#'   continuous.
#' @param loq optional numeric limit of quantification. Simulated observations
#'   below `loq` are handled according to `blq_method`.
#' @param blq_method how to handle simulated observations below `loq`.
#'   `"remove"` (default) removes the below-LOQ observation rows. `"cens"` keeps
#'   them, sets their `DV` to `0`, and adds a `CENS` column with `1` for
#'   below-LOQ observations and `0` otherwise.
#' @param sigdig number of significant digits to round the sampled covariates
#'   and simulated concentrations (`DV`) to, via [signif()], to further obscure
#'   the values. Default `4`. `NULL` disables rounding. Rounding is applied
#'   before the `loq` comparison.
#' @param n_candidates number of independent candidates to draw and score against
#'   the observed data, keeping the closest match. Default `1` (no scoring; a
#'   single candidate is used). When greater than 1, the candidate with the
#'   smallest distance to the observed data (see `score_on`) is selected. The
#'   chosen candidate's score is attached to the result as the
#'   `"similarity_score"` attribute.
#' @param similarity distribution-similarity metric used to score candidates.
#'   `"energy"` (default) is the energy distance between the observed and
#'   simulated feature matrices, standardized by the observed spread. Lower is
#'   closer. The score is population-level, not per-subject, so it does not push
#'   individual simulated subjects toward specific real subjects.
#' @param score_on what to score candidates on when `n_candidates > 1`.
#'   `"concentration"` (default) scores the simulated concentration-time data:
#'   each candidate is fully simulated and its `(time, log concentration)`
#'   observation cloud is compared to the observed concentrations, so the PK
#'   simulation is run once per candidate. `"covariate"` scores only the sampled
#'   covariate trajectories (baseline, mean, slope and within-subject SD per
#'   continuous covariate; per-subject time fraction in each level per
#'   categorical covariate), which is cheaper because the PK simulation is then
#'   run only once, on the selected covariate sample.
#' @param seed optional integer seed used for covariate sampling and FeRx
#'   simulation. `NULL` (default) does not seed, so each call draws
#'   independently.
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
  method = c("mice", "lme", "copulas"),
  loq = NULL,
  blq_method = c("remove", "cens"),
  sigdig = 4,
  n_candidates = 1,
  similarity = c("energy"),
  score_on = c("concentration", "covariate"),
  seed = NULL,
  ...
) {
  method <- match.arg(method)
  blq_method <- match.arg(blq_method)
  similarity <- match.arg(similarity)
  score_on <- match.arg(score_on)
  if (!is.null(sigdig) &&
      (!is.numeric(sigdig) || length(sigdig) != 1 || is.na(sigdig) ||
       sigdig < 1 || sigdig != round(sigdig))) {
    stop("`sigdig` must be a single positive integer or NULL.", call. = FALSE)
  }
  if (length(n_candidates) != 1 || !is.numeric(n_candidates) ||
      is.na(n_candidates) || n_candidates < 1 || n_candidates != round(n_candidates)) {
    stop("`n_candidates` must be a single positive integer.", call. = FALSE)
  }
  data <- as.data.frame(data)
  dictionary <- normalize_anonymize_dictionary(dictionary)
  validate_anonymize_inputs(
    data = data,
    covariates = covariates,
    model_file = model_file,
    dictionary = dictionary,
    loq = loq
  )

  id_var <- dictionary$ID
  time_var <- dictionary$TIME

  sample_data <- as.data.frame(data[, c(id_var, time_var, covariates), drop = FALSE])
  cat_covs <- covariates[
    vapply(sample_data[covariates], function(x) {
      is.factor(x) || is.character(x)
    }, logical(1))
  ]
  if (method != "mice" && length(cat_covs) > 0) {
    stop(
      "`method = \"", method, "\"` supports only continuous covariates, but these ",
      "are categorical: ", paste(cat_covs, collapse = ", "),
      ". Use `method = \"mice\"` for categorical covariates.",
      call. = FALSE
    )
  }

  selected <- select_best_candidate(
    data = data,
    sample_data = sample_data,
    covariates = covariates,
    method = method,
    cat_covs = cat_covs,
    dictionary = dictionary,
    model_file = model_file,
    id_var = id_var,
    time_var = time_var,
    n_subjects = length(unique(data[[id_var]])),
    n_candidates = n_candidates,
    similarity = similarity,
    score_on = score_on,
    seed = seed,
    ...
  )

  anonymized <- apply_anonymize_sigdig(
    data = selected$data,
    covariates = covariates,
    sigdig = sigdig
  )
  result <- apply_anonymize_blq(
    data = anonymized,
    loq = loq,
    blq_method = blq_method
  )
  attr(result, "similarity_score") <- selected$score
  result
}

#' Draw one time-varying covariate candidate with the chosen sampler
#'
#' Dispatches to the MICE, LME or copula time-varying sampler, in all cases
#' requesting propensity design-matching (where supported) and emitting the
#' `.design_id` bookkeeping column that [build_anonymized_simulation_input()]
#' uses to align each simulated subject with an observed dosing/observation
#' design.
#'
#' @noRd
draw_anonymize_candidate <- function(
  method, sample_data, covariates, cat_covs, id_var, time_var, n_subjects, seed, ...
) {
  switch(
    method,
    mice = sample_covariates_mice_timevarying(
      data = sample_data, id_var = id_var, time_var = time_var,
      time_varying_covs = covariates, cat_covs = cat_covs,
      design_match = "propensity", design_match_covs = covariates,
      design_id_var = ".design_id", n_subjects = n_subjects, seed = seed, ...
    ),
    lme = sample_covariates_lme_timevarying(
      data = sample_data, id_var = id_var, time_var = time_var,
      time_varying_covs = covariates,
      design_match = "propensity", design_match_covs = covariates,
      design_id_var = ".design_id", n_subjects = n_subjects, seed = seed, ...
    ),
    copulas = sample_covariates_copulas_timevarying(
      data = sample_data, id_var = id_var, time_var = time_var,
      time_varying_covs = covariates,
      design_id_var = ".design_id", n_subjects = n_subjects, seed = seed, ...
    )
  )
}

#' Draw, simulate and score candidates, returning the best anonymized dataset
#'
#' Orchestrates candidate selection for [anonymize_dataset()]. With
#' `n_candidates == 1` a single candidate is sampled and simulated. With
#' `score_on == "covariate"` the closest covariate sample is chosen first and the
#' PK model is simulated once. With `score_on == "concentration"` every candidate
#' is sampled *and* simulated, and the one whose simulated concentration-time
#' data is closest to the observed concentrations is kept. Returns a list with
#' the selected anonymized data (concentrations filled in) and its score.
#'
#' @noRd
select_best_candidate <- function(
  data,
  sample_data,
  covariates,
  method,
  cat_covs,
  dictionary,
  model_file,
  id_var,
  time_var,
  n_subjects,
  n_candidates,
  similarity,
  score_on,
  seed,
  ...
) {
  draw_candidate <- function(candidate_seed) {
    draw_anonymize_candidate(
      method = method, sample_data = sample_data, covariates = covariates,
      cat_covs = cat_covs, id_var = id_var, time_var = time_var,
      n_subjects = n_subjects, seed = candidate_seed, ...
    )
  }
  simulate_candidate <- function(sampled, candidate_seed) {
    anonymized <- build_anonymized_simulation_input(
      data = data, sampled_covs = sampled,
      covariates = covariates, dictionary = dictionary
    )
    sim <- simulate_anonymized_concentrations(
      model_file = model_file, data = anonymized, seed = candidate_seed
    )
    apply_simulated_concentrations(data = anonymized, sim = sim)
  }

  if (n_candidates == 1) {
    return(list(data = simulate_candidate(draw_candidate(seed), seed), score = NULL))
  }

  if (score_on == "covariate") {
    sampled <- select_best_covariate_sample(
      sample_data = sample_data, observed = data, covariates = covariates,
      method = method, cat_covs = cat_covs, id_var = id_var, time_var = time_var,
      n_subjects = n_subjects, n_candidates = n_candidates,
      similarity = similarity, seed = seed, ...
    )
    return(list(
      data = simulate_candidate(sampled, seed),
      score = attr(sampled, "similarity_score")
    ))
  }

  # score_on == "concentration": simulate every candidate and compare the
  # simulated concentration-time cloud to the observed concentrations.
  observed_cloud <- build_concentration_cloud(
    data = data, evid_var = dictionary$EVID,
    time_var = time_var, dv_var = dictionary$DV
  )
  best <- NULL
  best_score <- Inf
  for (i in seq_len(n_candidates)) {
    candidate_seed <- if (is.null(seed)) NULL else as.integer(seed + i - 1L)
    anonymized <- simulate_candidate(draw_candidate(candidate_seed), candidate_seed)
    score <- score_concentration_similarity(observed_cloud, anonymized, similarity)
    if (score < best_score) {
      best_score <- score
      best <- anonymized
    }
  }
  list(data = best, score = best_score)
}

#' Build a (time, log-concentration) observation point cloud
#'
#' Extracts the observation rows (`EVID == 0`) with a positive concentration and
#' returns a two-column matrix of time and log concentration, used to score the
#' similarity of simulated concentrations to the observed data.
#'
#' @noRd
build_concentration_cloud <- function(data, evid_var, time_var, dv_var) {
  evid <- data[[evid_var]]
  obs <- data[!is.na(evid) & evid == 0, , drop = FALSE]
  dv <- obs[[dv_var]]
  tm <- obs[[time_var]]
  keep <- is.finite(dv) & dv > 0 & is.finite(tm)
  cbind(TIME = tm[keep], LOGDV = log(dv[keep]))
}

#' Energy distance between observed and simulated concentration clouds
#'
#' Builds the candidate's `(time, log concentration)` cloud (using the standard
#' `EVID`/`TIME`/`DV` columns produced by the simulation step), standardizes both
#' clouds by the observed spread, and returns the requested distance.
#'
#' @noRd
score_concentration_similarity <- function(observed_cloud, candidate, similarity) {
  candidate_cloud <- build_concentration_cloud(
    data = candidate, evid_var = "EVID", time_var = "TIME", dv_var = "DV"
  )
  if (nrow(candidate_cloud) == 0 || nrow(observed_cloud) == 0) {
    return(Inf)
  }
  sds <- apply(observed_cloud, 2, stats::sd)
  sds[!is.finite(sds) | sds == 0] <- 1
  observed <- sweep(observed_cloud, 2, sds, "/")
  candidate_std <- sweep(candidate_cloud, 2, sds, "/")
  switch(
    similarity,
    energy = energy_distance(observed, candidate_std)
  )
}

#' Draw and score `n_candidates` covariate samples, returning the closest match
#'
#' Samples the time-varying covariates `n_candidates` times (with derived,
#' reproducible seeds), scores each candidate against the observed covariate data
#' with the requested population-level distance, and returns the candidate with
#' the smallest distance. With `n_candidates == 1` a single sample is returned
#' unchanged, preserving the historical behaviour.
#'
#' @noRd
select_best_covariate_sample <- function(
  sample_data,
  observed,
  covariates,
  method,
  cat_covs,
  id_var,
  time_var,
  n_subjects,
  n_candidates,
  similarity,
  seed,
  ...
) {
  draw_candidate <- function(candidate_seed) {
    draw_anonymize_candidate(
      method = method, sample_data = sample_data, covariates = covariates,
      cat_covs = cat_covs, id_var = id_var, time_var = time_var,
      n_subjects = n_subjects, seed = candidate_seed, ...
    )
  }

  if (n_candidates == 1) {
    return(draw_candidate(seed))
  }

  cat_levels <- lapply(cat_covs, function(cov) {
    sort(unique(as.character(observed[[cov]])))
  })
  names(cat_levels) <- cat_covs
  observed_feats <- build_covariate_features(
    data = observed, id_var = id_var, time_var = time_var,
    covariates = covariates, cat_covs = cat_covs, cat_levels = cat_levels
  )

  best <- NULL
  best_score <- Inf
  for (i in seq_len(n_candidates)) {
    candidate_seed <- if (is.null(seed)) NULL else as.integer(seed + i - 1L)
    candidate <- draw_candidate(candidate_seed)
    candidate_feats <- build_covariate_features(
      data = candidate, id_var = id_var, time_var = time_var,
      covariates = covariates, cat_covs = cat_covs, cat_levels = cat_levels
    )
    score <- score_covariate_similarity(
      observed_feats = observed_feats,
      candidate_feats = candidate_feats,
      similarity = similarity
    )
    if (score < best_score) {
      best_score <- score
      best <- candidate
    }
  }
  attr(best, "similarity_score") <- best_score
  best
}

#' Build a per-subject covariate-trajectory feature matrix
#'
#' For each subject, summarises every continuous covariate by its baseline value,
#' mean, ordinary-least-squares slope over time and within-subject SD, and every
#' categorical covariate by the fraction of the subject's records spent in each
#' observed level. Returns a data.frame with one row per subject.
#'
#' @noRd
build_covariate_features <- function(
  data,
  id_var,
  time_var,
  covariates,
  cat_covs,
  cat_levels
) {
  cont_covs <- setdiff(covariates, cat_covs)
  profiles <- split(data, data[[id_var]])
  rows <- lapply(profiles, function(profile) {
    profile <- profile[order(profile[[time_var]]), , drop = FALSE]
    times <- profile[[time_var]]
    feat <- list()
    for (cov in cont_covs) {
      x <- profile[[cov]]
      non_na <- !is.na(x)
      feat[[paste0(cov, "_base")]] <- x[[1]]
      feat[[paste0(cov, "_mean")]] <- mean(x, na.rm = TRUE)
      feat[[paste0(cov, "_slope")]] <- trajectory_slope(times, x)
      feat[[paste0(cov, "_sd")]] <- if (sum(non_na) > 1) stats::sd(x, na.rm = TRUE) else 0
    }
    for (cov in cat_covs) {
      values <- as.character(profile[[cov]])
      for (lvl in cat_levels[[cov]]) {
        feat[[paste0(cov, "_", lvl)]] <- mean(values == lvl, na.rm = TRUE)
      }
    }
    as.data.frame(feat, check.names = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Ordinary-least-squares slope of `x` against `t`, robust to sparse data
#'
#' @noRd
trajectory_slope <- function(t, x) {
  ok <- !is.na(t) & !is.na(x)
  if (sum(ok) < 2 || length(unique(t[ok])) < 2) {
    return(0)
  }
  unname(stats::coef(stats::lm(x[ok] ~ t[ok]))[[2]])
}

#' Population-level distance between observed and candidate feature matrices
#'
#' Standardises both matrices by the observed per-feature spread (dropping
#' zero-variance features) and returns the requested distribution distance.
#'
#' @noRd
score_covariate_similarity <- function(observed_feats, candidate_feats, similarity) {
  shared <- intersect(names(observed_feats), names(candidate_feats))
  observed_feats <- observed_feats[, shared, drop = FALSE]
  candidate_feats <- candidate_feats[, shared, drop = FALSE]
  sds <- vapply(observed_feats, stats::sd, numeric(1), na.rm = TRUE)
  # Drop (near-)constant features with a relative threshold: a feature whose
  # between-subject SD is only floating-point noise would otherwise be divided to
  # ~1e15 and wreck the energy distance through catastrophic cancellation.
  scale <- pmax(abs(vapply(observed_feats, mean, numeric(1), na.rm = TRUE)), 1)
  keep <- shared[is.finite(sds) & sds > 1e-8 * scale]
  if (length(keep) == 0) {
    return(0)
  }
  observed <- sweep(as.matrix(observed_feats[, keep, drop = FALSE]), 2, sds[keep], "/")
  candidate <- sweep(as.matrix(candidate_feats[, keep, drop = FALSE]), 2, sds[keep], "/")
  observed[!is.finite(observed)] <- 0
  candidate[!is.finite(candidate)] <- 0
  switch(
    similarity,
    energy = energy_distance(observed, candidate)
  )
}

#' Energy distance between two standardized feature matrices
#'
#' `E = 2 * mean||o_i - s_j|| - mean||o_i - o_j|| - mean||s_i - s_j||`. Lower
#' values indicate more similar multivariate distributions.
#'
#' @noRd
energy_distance <- function(observed, simulated) {
  cross <- mean(cross_distances(observed, simulated))
  within_o <- mean(as.matrix(stats::dist(observed)))
  within_s <- mean(as.matrix(stats::dist(simulated)))
  2 * cross - within_o - within_s
}

#' Pairwise Euclidean distances between the rows of two matrices
#'
#' @noRd
cross_distances <- function(a, b) {
  a2 <- rowSums(a^2)
  b2 <- rowSums(b^2)
  d2 <- outer(a2, b2, "+") - 2 * a %*% t(b)
  sqrt(pmax(d2, 0))
}

#' Round sampled covariates and simulated concentrations to `sigdig` figures
#'
#' Applies [signif()] to the numeric covariate columns and the `DV` column to
#' obscure the exact simulated values. Categorical/non-numeric covariates are
#' left untouched. `sigdig = NULL` returns `data` unchanged.
#'
#' @noRd
apply_anonymize_sigdig <- function(data, covariates, sigdig) {
  if (is.null(sigdig)) {
    return(data)
  }
  cols <- intersect(c(covariates, "DV"), names(data))
  for (col in cols) {
    if (is.numeric(data[[col]])) {
      data[[col]] <- signif(data[[col]], sigdig)
    }
  }
  data
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

validate_anonymize_inputs <- function(data, covariates, model_file, dictionary, loq) {
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
  if (!is.null(loq) && (!is.numeric(loq) || length(loq) != 1 || is.na(loq))) {
    stop("`loq` must be a single numeric value.", call. = FALSE)
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

  # Pre-split both frames so each simulated subject is an O(1) lookup rather
  # than a full-table scan. Designs are keyed on the character form of the ID
  # because `.design_id` is stored as character by the sampler.
  sim_ids <- unique(sampled_covs[[id_var]])
  sampled_split <- split(sampled_covs, as.character(sampled_covs[[id_var]]))
  data_split <- split(data, as.character(data[[id_var]]))

  out <- vector("list", length(sim_ids))
  skipped <- character(0)
  i <- 1L
  for (sim_id in sim_ids) {
    cov_rows <- sampled_split[[as.character(sim_id)]]
    design_id <- as.character(cov_rows[[".design_id"]][[1]])
    design_rows <- data_split[[design_id]]
    if (is.null(design_rows)) {
      skipped <- c(skipped, as.character(sim_id))
      next
    }
    design_rows <- design_rows[order(design_rows[[time_var]]), , drop = FALSE]
    aligned <- nrow(design_rows) == nrow(cov_rows) &&
      isTRUE(all.equal(
        design_rows[[time_var]],
        cov_rows[[time_var]],
        check.attributes = FALSE
      ))
    if (!aligned) {
      skipped <- c(skipped, as.character(sim_id))
      next
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
  if (length(skipped) > 0) {
    warning(
      "Dropped ", length(skipped), " simulated subject(s) whose matched design ",
      "could not be aligned with the sampled covariate times: ",
      paste(skipped, collapse = ", "),
      call. = FALSE
    )
  }
  out <- dplyr::bind_rows(out)
  order_cols <- unique(c("ID", "TIME", "EVID", "AMT", "RATE", "DV", "CMT", "MDV", "ADDL", "II", "SS", covariates))
  out[, intersect(order_cols, names(out)), drop = FALSE]
}

simulate_anonymized_concentrations <- function(model_file, data, seed = NULL) {
  if (!requireNamespace("ferx", quietly = TRUE)) {
    stop("Package `ferx` is required to simulate anonymized concentrations.", call. = FALSE)
  }
  sim_data <- data
  sim_data_path <- tempfile(fileext = ".csv")
  utils::write.csv(sim_data, sim_data_path, row.names = FALSE, na = ".")
  sim_args <- list(
    model = model_file,
    data = sim_data_path,
    n_sim = 1L
  )
  if (!is.null(seed)) sim_args$seed <- as.integer(seed)
  do.call(ferx::ferx_simulate, sim_args)
}

apply_simulated_concentrations <- function(data, sim) {
  if (!"DV_SIM" %in% names(sim)) {
    stop("FeRx simulation output must contain a `DV_SIM` column.", call. = FALSE)
  }
  obs_idx <- which(data$EVID == 0)
  if (length(obs_idx) != nrow(sim)) {
    stop(
      "FeRx simulation row count does not match the number of observation records.",
      call. = FALSE
    )
  }
  # Match simulated concentrations back onto observation rows by ID and TIME
  # rather than by position: FeRx is not guaranteed to return rows in the input
  # order, so a positional copy can silently scramble subjects/timepoints.
  if (all(c("ID", "TIME") %in% names(sim))) {
    obs <- data[obs_idx, c("ID", "TIME"), drop = FALSE]
    obs[[".obs_order"]] <- seq_along(obs_idx)
    merged <- merge(
      obs,
      sim[, c("ID", "TIME", "DV_SIM"), drop = FALSE],
      by = c("ID", "TIME"),
      sort = FALSE
    )
    if (nrow(merged) != length(obs_idx)) {
      stop(
        "FeRx simulation output could not be uniquely matched to observation ",
        "records by ID and TIME.",
        call. = FALSE
      )
    }
    merged <- merged[order(merged[[".obs_order"]]), , drop = FALSE]
    data[["DV"]][obs_idx] <- merged$DV_SIM
  } else {
    data[["DV"]][obs_idx] <- sim[["DV_SIM"]]
  }
  data
}

apply_anonymize_blq <- function(data, loq, blq_method) {
  if (is.null(loq)) {
    return(data)
  }
  obs <- data$EVID == 0
  # Guard against NA simulated concentrations: an NA `DV` would otherwise make
  # `blq` contain NA, which inserts all-NA rows on the remove path and errors on
  # the cens path ("NAs are not allowed in subscripted assignments").
  blq <- obs & !is.na(data$DV) & data$DV < loq
  if (blq_method == "remove") {
    return(data[!blq, , drop = FALSE])
  }
  data$CENS <- 0L
  data$CENS[blq] <- 1L
  data$DV[blq] <- 0
  data
}
