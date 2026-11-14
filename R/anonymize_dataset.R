#' Anonymize a NONMEM-style dataset
#'
#' Creates an anonymized NONMEM-style dataset by renumbering subjects, sampling
#' new covariates, cloning observed dosing/observation designs, and replacing
#' observed concentrations with FeRx-simulated values.
#'
#' @param data original NONMEM-style `data.frame` or `tibble`.
#' @param covariates character vector of covariates to include in the
#'   anonymized dataset. May be `character(0)` for datasets without covariate
#'   columns: covariate sampling is then skipped, and each observed subject's
#'   dosing/observation design is cloned (in random order) onto a renumbered
#'   subject `1:n` before the concentrations are simulated.
#' @param model_file path to a FeRx model file used to simulate concentrations.
#'   The file is not used verbatim: a model written to fit the original data
#'   usually still describes that dataset, so the simulation runs on a temporary
#'   copy adapted to the anonymized dataset. In that copy the `[data_selection]`
#'   block (whose filters refer to original records and columns) and the
#'   deprecated `[initial_values]` block are removed; in the `[data]` block,
#'   `path` is replaced by the absolute path of the simulation dataset, and a
#'   column map is kept only when it renames a sampled covariate (from
#'   `covariates`) whose new name does not collide with a column that remains
#'   under its own name after all retained renames are applied (renames are
#'   simultaneous, so `HTCM = WT` with `WT = HT` is kept). Maps onto or from
#'   the structural columns (e.g. `AMT = DOSE`, `DV = lndv`) are dropped,
#'   because the anonymized dataset already uses the canonical names `ID`,
#'   `TIME`, `EVID`, `AMT`, `DV` and `RATE` (plus `CMT`, `MDV`, `ADDL`, `II`
#'   and `SS` when present in `data`). FeRx matches covariate names
#'   case-sensitively, so when the model references a covariate under a
#'   spelling that differs only by case from its column (e.g. `CONC0` for a
#'   `conc0` column) and the `[data]` block does not map it, a rename such as
#'   `CONC0 = conc0` is added. This is checked before any covariate is sampled:
#'   a reference that matches several columns case-insensitively is an error,
#'   and a reference to a column of `data` that is in neither `covariates` nor
#'   `design_cols` raises a warning. A relative `[priors]` `from_fit` path is
#'   made absolute against the directory of `model_file`, so it keeps resolving
#'   from the copy. Comments (`#` and `//`) are handled as FeRx handles them.
#' @param dictionary named list mapping expected NONMEM column names to columns
#'   in `data`. Supported keys are `ID`, `EVID`, `AMT`, `TIME`, `DV`, and
#'   optionally `RATE`. Defaults use the same names.
#' @param method time-varying covariate sampler used to resample the covariates.
#'   `"mice"` (default) uses [sample_covariates_mice_timevarying()], `"lme"` uses
#'   [sample_covariates_lme_timevarying()], and `"copulas"` uses
#'   [sample_covariates_copulas_timevarying()]. Only `"mice"` supports
#'   categorical covariates; `"lme"` and `"copulas"` require all covariates to be
#'   continuous.
#' @param cat_covs optional character vector of categorical covariates, in
#'   addition to factor and character covariates (which are always treated as
#'   categorical). Use this for numerically coded categorical covariates, e.g.
#'   `SEX = 0/1`, so they are not sampled as continuous values. They are
#'   returned with their original numeric codes. With `method = "mice"`, a
#'   warning is raised for numeric covariates that look like categorical codes
#'   (whole numbers with at most 3 distinct values) but are not listed here.
#'   Categorical covariates (including factor and character columns) with
#'   more than 20 distinct values are rejected with an error reporting the
#'   level count of each categorical covariate: such a column is almost always
#'   a continuous one (e.g. a time, dose or occasion counter) passed as
#'   categorical by mistake, and would make the MICE sampler extremely slow.
#'   Pass `max_cat_levels` (through `...`) to raise this limit.
#' @param loq optional numeric limit of quantification. Simulated observations
#'   below `loq` are handled according to `blq_method`.
#' @param blq_method how to handle simulated observations below `loq`.
#'   `"remove"` (default) removes the below-LOQ observation rows. `"cens"` keeps
#'   them, sets their `DV` to `0`, and adds a `CENS` column with `1` for
#'   below-LOQ observations and `0` otherwise. If `data` contains an `MDV`
#'   column, it is recomputed from the simulated data: `1` for non-observation
#'   records and observations with a missing `DV`, `0` otherwise (censored
#'   observations keep `MDV = 0` and are flagged via `CENS`).
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
#' @param design_cols optional character vector of study-design columns to
#'   carry through verbatim from the matched source design, e.g. analyte or
#'   readout selectors (`DVID`), occasion indices for IOV (`OCC`), L2 pairing
#'   indices (`PAIR`), carried dose history (`TLST`, `LASTDOSE`) or row filters
#'   used by the model (`FLAG`). Unlike `covariates`, these columns are never
#'   resampled, imputed or rounded: each anonymized subject receives the values
#'   of the observed subject whose dosing/observation design it clones, record by
#'   record. They are included in the data passed to the FeRx simulation, so the
#'   model can reference them, and in the returned dataset. Must not overlap with
#'   `covariates` or the mapped `ID`, `EVID`, `AMT`, `TIME`, `DV` and `RATE`
#'   columns. Columns already carried through as part of the event design (`CMT`,
#'   `MDV`, `ADDL`, `II`, `SS`) are accepted and kept as usual.
#' @param ... additional arguments passed to the time-varying sampler selected
#'   by `method` ([sample_covariates_mice_timevarying()],
#'   [sample_covariates_lme_timevarying()] or
#'   [sample_covariates_copulas_timevarying()]) and, for `"mice"` and `"lme"`,
#'   on to the internal MICE implementation (e.g. `maxit`). Every argument must
#'   be named and accepted by that sampler. Unknown names (typos, or arguments
#'   added in a newer irxforge release) and arguments that
#'   `anonymize_dataset()` sets itself (e.g. `id_var`, `n_subjects`) are
#'   rejected up front with an "unused argument" error.
#'
#' @returns anonymized NONMEM-style data.frame, with one block of records per
#'   anonymized subject (`ID` `1:n`). Each block is a clone of one observed
#'   subject's dosing/observation design, returned **ordered by time within
#'   subject** (a stable sort on `TIME`, so records sharing a time keep their
#'   source order and records with a missing time are placed last). The result
#'   carries these attributes:
#'   * `"design_map"`: a `data.frame` with one row per anonymized subject and
#'     columns `ID` (the anonymized subject ID) and `DESIGN_ID` (the ID of the
#'     observed subject in `data` whose design was cloned, in its original
#'     type). Use it to relate anonymized subjects back to their source design
#'     without reconstructing it from the event records. Note that this mapping
#'     links the anonymized dataset to the source subjects, so it should not be
#'     shared alongside the anonymized data.
#'   * `"similarity_score"`: the selected candidate's score when
#'     `n_candidates > 1`, otherwise `NULL`.
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
  cat_covs = NULL,
  loq = NULL,
  blq_method = c("remove", "cens"),
  sigdig = 4,
  n_candidates = 1,
  similarity = c("energy"),
  score_on = c("concentration", "covariate"),
  seed = NULL,
  design_cols = NULL,
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
  # validate the `...` names before any of them is evaluated, so an unknown
  # argument is reported as such even if evaluating it would fail
  dots <- as.list(match.call(expand.dots = FALSE)$...)
  check_anonymize_dots(names(dots), n_dots = length(dots), method = method)
  # force only `replicates` (if supplied), leaving other promises untouched
  replicates <- (function(..., replicates = NULL) replicates)(...)
  if (!is.null(replicates) && !identical(as.numeric(replicates), 1)) {
    stop(
      "`replicates` is not supported by `anonymize_dataset()`: each call ",
      "returns a single anonymized dataset. Call it repeatedly (with ",
      "different seeds) to generate multiple datasets.",
      call. = FALSE
    )
  }
  data <- as.data.frame(data)
  dictionary <- normalize_anonymize_dictionary(dictionary)
  validate_anonymize_inputs(
    data = data,
    covariates = covariates,
    model_file = model_file,
    dictionary = dictionary,
    loq = loq,
    design_cols = design_cols
  )
  check_anonymize_model_covariates(
    model_file = model_file,
    data = data,
    covariates = covariates,
    dictionary = dictionary,
    design_cols = design_cols
  )
  id_var <- dictionary$ID
  time_var <- dictionary$TIME
  check_anonymize_monotone_time(
    data = data,
    id_var = id_var,
    time_var = time_var,
    evid_var = dictionary$EVID
  )

  if (!is.null(cat_covs) && !all(cat_covs %in% covariates)) {
    stop("`cat_covs` must be a subset of `covariates`.", call. = FALSE)
  }
  sample_data <- as.data.frame(data[, c(id_var, time_var, covariates), drop = FALSE])
  # categorical covariates: those specified by the caller (e.g. numerically
  # coded, such as SEX = 0/1) plus all factor or character covariates
  cat_covs <- union(cat_covs, covariates[
    vapply(sample_data[covariates], function(x) {
      is.factor(x) || is.character(x)
    }, logical(1))
  ])
  if (method != "mice" && length(cat_covs) > 0) {
    stop(
      "`method = \"", method, "\"` supports only continuous covariates, but these ",
      "are categorical: ", paste(cat_covs, collapse = ", "),
      ". Use `method = \"mice\"` for categorical covariates.",
      call. = FALSE
    )
  }
  if (length(covariates) == 0 && n_candidates > 1 && score_on == "covariate") {
    stop(
      "`score_on = \"covariate\"` requires at least one covariate; use ",
      "`score_on = \"concentration\"` or `n_candidates = 1` instead.",
      call. = FALSE
    )
  }

  selected <- select_best_candidate(
    data = data,
    sample_data = sample_data,
    covariates = covariates,
    method = method,
    cat_covs = cat_covs,
    design_cols = design_cols,
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
  result <- recompute_anonymized_mdv(result)
  attr(result, "design_map") <- attr(selected$data, "design_map")
  attr(result, "similarity_score") <- selected$score
  result
}

#' Validate the names of the `...` arguments of [anonymize_dataset()]
#'
#' `...` is forwarded through several internal functions to the time-varying
#' sampler selected by `method` (and, for MICE/LME, on to [mice_impute()]).
#' Without an up-front check, a misspelled or unsupported argument either
#' vanishes silently or surfaces deep in the call stack as a confusing error
#' such as "formal argument ... matched by multiple actual arguments". This
#' rejects unnamed arguments, arguments `anonymize_dataset()` already sets
#' itself, and arguments the selected sampler does not accept.
#'
#' @param dot_names names of the `...` arguments (`NULL` if none are named).
#' @param n_dots number of `...` arguments.
#' @param method the selected sampler (`"mice"`, `"lme"` or `"copulas"`).
#'
#' @noRd
check_anonymize_dots <- function(dot_names, n_dots, method) {
  if (n_dots == 0) {
    return(invisible(NULL))
  }
  if (length(dot_names) != n_dots || any(is.na(dot_names) | dot_names == "")) {
    stop(
      "All arguments passed to `anonymize_dataset()` through `...` must be named.",
      call. = FALSE
    )
  }
  # arguments anonymize_dataset() passes to the sampler (or to mice_impute())
  # itself; supplying them again through `...` would clash
  reserved <- c(
    "data", "id_var", "time_var", "time_varying_covs", "cat_covs",
    "design_match", "design_match_covs", "design_id_var", "n_subjects", "seed",
    "method", "m"
  )
  sampler <- switch(
    method,
    mice = sample_covariates_mice_timevarying,
    lme = sample_covariates_lme_timevarying,
    copulas = sample_covariates_copulas_timevarying
  )
  accepted <- names(formals(sampler))
  # MICE and LME forward their own `...` to the internal MICE implementation
  if (method %in% c("mice", "lme")) {
    accepted <- c(accepted, names(formals(mice_impute)))
  }
  accepted <- setdiff(accepted, c("...", reserved))

  shadowing <- unique(intersect(dot_names, reserved))
  if (length(shadowing) > 0) {
    stop(
      "Cannot pass ", paste0("`", shadowing, "`", collapse = ", "),
      " to `anonymize_dataset()` through `...`: ",
      if (length(shadowing) == 1) "it is" else "they are",
      " set by `anonymize_dataset()` itself.",
      call. = FALSE
    )
  }
  unknown <- unique(setdiff(dot_names, accepted))
  if (length(unknown) > 0) {
    version <- tryCatch(
      as.character(utils::packageVersion("irxforge")),
      error = function(e) "unknown version"
    )
    stop(
      "unused argument", if (length(unknown) > 1) "s", " ",
      paste0("`", unknown, "`", collapse = ", "),
      " in `anonymize_dataset()`: not supported by this build of irxforge (",
      version, ") with `method = \"", method, "\"`. Check the spelling, or ",
      "update irxforge if ", if (length(unknown) > 1) "these were" else "it was",
      " added in a newer version. Arguments accepted through `...`: ",
      paste0("`", accepted, "`", collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(NULL)
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
  if (length(covariates) == 0) {
    return(clone_anonymize_designs(
      sample_data = sample_data, id_var = id_var, time_var = time_var, seed = seed
    ))
  }
  sampled <- switch(
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
  # restore numerically coded categorical covariates (sampled as factors) to
  # their original numeric codes
  for (cov in cat_covs) {
    if (is.numeric(sample_data[[cov]]) && !is.numeric(sampled[[cov]])) {
      sampled[[cov]] <- as.numeric(as.character(sampled[[cov]]))
    }
  }
  sampled
}

#' Clone observed designs onto renumbered subjects without covariates
#'
#' Used by [anonymize_dataset()] when `covariates` is empty, so there is nothing
#' to sample. Each observed subject's design is assigned, in random order, to a
#' new subject ID `1:n`, producing the same `id_var`/`time_var`/`.design_id`
#' layout as the time-varying covariate samplers so that
#' `build_anonymized_simulation_input()` can clone the dosing/observation
#' records.
#'
#' @noRd
clone_anonymize_designs <- function(sample_data, id_var, time_var, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  design_ids <- unique(sample_data[[id_var]])
  design_ids <- design_ids[sample.int(length(design_ids))]
  data_split <- split(sample_data, as.character(sample_data[[id_var]]))
  out <- lapply(seq_along(design_ids), function(i) {
    design_id <- as.character(design_ids[[i]])
    times <- data_split[[design_id]][[time_var]]
    times <- times[order(times)]
    rows <- data.frame(
      id = rep(i, length(times)),
      .design_id = rep(design_id, length(times)),
      time = times,
      stringsAsFactors = FALSE
    )
    names(rows) <- c(id_var, ".design_id", time_var)
    rows
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
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
  design_cols = NULL,
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
      covariates = covariates, dictionary = dictionary,
      design_cols = design_cols
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
  if (nrow(observed_cloud) == 0) {
    stop(
      "`score_on = \"concentration\"` requires positive observed concentrations ",
      "in `data`; use `score_on = \"covariate\"` or `n_candidates = 1` instead.",
      call. = FALSE
    )
  }
  best <- NULL
  best_score <- Inf
  for (i in seq_len(n_candidates)) {
    candidate_seed <- if (is.null(seed)) NULL else as.integer(seed + i - 1L)
    anonymized <- simulate_candidate(draw_candidate(candidate_seed), candidate_seed)
    score <- score_concentration_similarity(observed_cloud, anonymized, similarity)
    # keep the first candidate as fallback so a dataset is always returned,
    # even if no candidate yields a finite score
    if (is.null(best) || score < best_score) {
      best_score <- score
      best <- anonymized
    }
  }
  if (!is.finite(best_score)) {
    warning(
      "No candidate produced positive simulated concentrations; returning the ",
      "first candidate without a similarity score.",
      call. = FALSE
    )
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
    if (is.null(best) || score < best_score) {
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
  profiles <- split(data, data[[id_var]], drop = TRUE)
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

validate_anonymize_inputs <- function(data, covariates, model_file, dictionary, loq,
                                      design_cols = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble.", call. = FALSE)
  }
  if (!is.character(covariates)) {
    stop(
      "`covariates` must be a character vector (use `character(0)` for none).",
      call. = FALSE
    )
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
  if (!is.null(design_cols)) {
    if (!is.character(design_cols) || anyNA(design_cols)) {
      stop("`design_cols` must be a character vector or NULL.", call. = FALSE)
    }
    missing_design <- setdiff(design_cols, names(data))
    if (length(missing_design) > 0) {
      stop(
        "`design_cols` were not found in `data`: ",
        paste(missing_design, collapse = ", "),
        call. = FALSE
      )
    }
    # also reserve the standard output names, which the mapped columns are
    # renamed to in the anonymized dataset
    reserved <- union(structural, c("ID", "EVID", "AMT", "TIME", "DV", "RATE"))
    design_structural <- intersect(design_cols, reserved)
    if (length(design_structural) > 0) {
      stop(
        "`design_cols` cannot include mapped NONMEM columns: ",
        paste(design_structural, collapse = ", "),
        call. = FALSE
      )
    }
    design_cov_overlap <- intersect(design_cols, covariates)
    if (length(design_cov_overlap) > 0) {
      stop(
        "Columns cannot be both resampled `covariates` and verbatim ",
        "`design_cols`: ", paste(design_cov_overlap, collapse = ", "),
        call. = FALSE
      )
    }
  }
  if (!is.null(loq) && (!is.numeric(loq) || length(loq) != 1 || is.na(loq))) {
    stop("`loq` must be a single numeric value.", call. = FALSE)
  }
  check_anonymize_observed_covariates(
    data = data,
    covariates = covariates,
    id_var = dictionary$ID,
    time_var = dictionary$TIME
  )
}

#' Check that every covariate has observed values overall and at baseline
#'
#' The time-varying covariate samplers impute baseline covariates from the first
#' record of each subject (after ordering by time), so a covariate that is
#' entirely missing on those baseline rows cannot be imputed even when it is
#' observed later on. This check fails fast, before any sampling or design
#' matching, and names all offending covariates at once, distinguishing
#' covariates with no observed values at all from those with no observed values
#' at baseline only.
#'
#' @param data NONMEM-style data.frame.
#' @param covariates character vector of covariate columns in `data`.
#' @param id_var,time_var names of the subject ID and time columns in `data`.
#'
#' @returns `invisible(TRUE)`; errors when any covariate lacks observed values
#'   overall or on the baseline rows.
#'
#' @noRd
check_anonymize_observed_covariates <- function(data, covariates, id_var, time_var) {
  if (length(covariates) == 0 || nrow(data) == 0) return(invisible(TRUE))
  # baseline rows: first record per subject after a stable sort on ID and time
  # (missing times last), mirroring the time-varying samplers
  ord <- order(data[[id_var]], data[[time_var]], na.last = TRUE, method = "radix")
  sorted_ids <- data[[id_var]][ord]
  baseline_rows <- ord[!duplicated(sorted_ids)]
  observed_any <- vapply(covariates, function(cov) any(!is.na(data[[cov]])), logical(1))
  observed_baseline <- vapply(covariates, function(cov) {
    any(!is.na(data[[cov]][baseline_rows]))
  }, logical(1))
  none <- covariates[!observed_any]
  none_baseline <- covariates[observed_any & !observed_baseline]
  if (length(none) == 0 && length(none_baseline) == 0) return(invisible(TRUE))
  problems <- c(
    if (length(none) > 0) {
      paste0("no observed values: ", paste(none, collapse = ", "))
    },
    if (length(none_baseline) > 0) {
      paste0(
        "no observed values at baseline (first record per subject): ",
        paste(none_baseline, collapse = ", ")
      )
    }
  )
  stop(
    "Covariates cannot be imputed because they have ",
    paste(problems, collapse = "; and "), ". ",
    "Drop these covariates",
    if (length(none_baseline) > 0) {
      ", or reshape the data so they are observed on each subject's first record"
    },
    ".",
    call. = FALSE
  )
}

#' Check that time is non-decreasing within every subject
#'
#' The time-varying covariate samplers require a single, monotone time axis per
#' subject. Reset records (`EVID` 3 or 4) are allowed as long as the time scale
#' does not restart: only datasets whose `TIME` actually decreases within a
#' subject (in record order) are rejected, and the offending subjects are
#' reported. Missing times are ignored.
#'
#' @param data NONMEM-style data.frame.
#' @param id_var,time_var,evid_var names of the subject ID, time and EVID
#'   columns in `data`.
#'
#' @returns `invisible(TRUE)`; errors when any subject's time axis decreases.
#'
#' @noRd
check_anonymize_monotone_time <- function(data, id_var, time_var, evid_var) {
  time <- data[[time_var]]
  mono <- tapply(seq_len(nrow(data)), data[[id_var]], function(i) {
    t <- time[i]
    t <- t[!is.na(t)]
    length(t) < 2 || all(diff(t) >= 0)
  })
  mono <- mono[!is.na(mono)]
  bad <- names(mono)[!mono]
  if (length(bad) == 0) return(invisible(TRUE))
  shown <- utils::head(bad, 10)
  more <- if (length(bad) > length(shown)) {
    paste0(" (and ", length(bad) - length(shown), " more)")
  } else {
    ""
  }
  has_reset <- any(data[[evid_var]][data[[id_var]] %in% bad] %in% c(3, 4))
  stop(
    "`anonymize_dataset()` requires a single, monotone (non-decreasing) time ",
    "axis per subject for the time-varying covariate samplers, but `",
    time_var, "` decreases within ", length(bad), " subject(s): ",
    paste(shown, collapse = ", "), more, ".",
    if (has_reset) {
      paste0(
        " These subjects contain time-reset records (EVID 3 or 4) that ",
        "restart the time scale; reset records are supported only when ",
        "time keeps increasing across them."
      )
    },
    call. = FALSE
  )
}

build_anonymized_simulation_input <- function(data, sampled_covs, covariates, dictionary,
                                              design_cols = NULL) {
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
  # design columns (e.g. DVID, OCC, PAIR, dose history) are cloned verbatim
  # from the matched design, record by record, and never resampled
  design_cols <- setdiff(unique(design_cols), event_cols)

  # Pre-split both frames so each simulated subject is an O(1) lookup rather
  # than a full-table scan. Designs are keyed on the character form of the ID
  # because `.design_id` is stored as character by the sampler.
  sim_ids <- unique(sampled_covs[[id_var]])
  sampled_split <- split(sampled_covs, as.character(sampled_covs[[id_var]]))
  data_split <- split(data, as.character(data[[id_var]]))

  out <- vector("list", length(sim_ids))
  design_map <- vector("list", length(sim_ids))
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
    # Designs are cloned in time order (stable, so ties keep source order and
    # missing times go last); this matches the time order of the sampled
    # covariates and is documented in `?anonymize_dataset`.
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

    events <- design_rows[, c(event_cols, design_cols), drop = FALSE]
    names(events)[names(events) == id_var] <- "ID"
    names(events)[names(events) == time_var] <- "TIME"
    names(events)[names(events) == evid_var] <- "EVID"
    names(events)[names(events) == amt_var] <- "AMT"
    if (rate_var %in% names(events)) names(events)[names(events) == rate_var] <- "RATE"
    events[["ID"]] <- sim_id
    events[["DV"]] <- 0

    if (length(covariates) > 0) {
      events <- cbind(events, cov_rows[, covariates, drop = FALSE])
    }
    out[[i]] <- events
    design_map[[i]] <- data.frame(
      ID = sim_id,
      DESIGN_ID = design_rows[[id_var]][[1]],
      stringsAsFactors = FALSE
    )
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
  order_cols <- unique(c(
    "ID", "TIME", "EVID", "AMT", "RATE", "DV", "CMT", "MDV", "ADDL", "II", "SS",
    design_cols, covariates
  ))
  out <- out[, intersect(order_cols, names(out)), drop = FALSE]
  design_map <- do.call(rbind, design_map[seq_len(i - 1L)])
  if (is.null(design_map)) {
    design_map <- data.frame(ID = sim_ids[0], DESIGN_ID = data[[id_var]][0])
  }
  rownames(design_map) <- NULL
  attr(out, "design_map") <- design_map
  out
}

simulate_anonymized_concentrations <- function(model_file, data, seed = NULL) {
  if (!requireNamespace("ferx", quietly = TRUE)) {
    stop("Package `ferx` is required to simulate anonymized concentrations.", call. = FALSE)
  }
  sim_data <- data
  sim_data_path <- tempfile(fileext = ".csv")
  utils::write.csv(sim_data, sim_data_path, row.names = FALSE, na = ".")
  sim_model_path <- prepare_simulation_model_file(
    model_file = model_file,
    data_path = sim_data_path,
    columns = names(sim_data)
  )
  on.exit(unlink(c(sim_data_path, sim_model_path)), add = TRUE)
  sim_args <- list(
    model = sim_model_path,
    data = sim_data_path,
    n_sim = 1L
  )
  if (!is.null(seed)) sim_args$seed <- as.integer(seed)
  do.call(ferx::ferx_simulate, sim_args)
}

#' Structural columns of the anonymized simulation frame
#'
#' The anonymized frame built by [build_anonymized_simulation_input()] carries
#' these columns under their canonical NONMEM names; every other column is a
#' sampled covariate under its name in the source data.
#'
#' @noRd
anonymized_structural_columns <- function() {
  c("ID", "TIME", "EVID", "AMT", "RATE", "DV", "CMT", "MDV", "ADDL", "II", "SS", "CENS")
}

#' Write a simulation-ready copy of a FeRx model file
#'
#' A model file written to *fit* the original dataset usually still describes
#' that dataset, which the anonymized simulation frame no longer matches. This
#' writes a temporary copy of `model_file` adapted to the anonymized frame:
#'
#' * the `[data_selection]` block is removed: its filters refer to the original
#'   records and columns, most of which de-identification dropped;
#' * the deprecated `[initial_values]` block (no longer read by FeRx) is
#'   removed;
#' * in the `[data]` block, `path` is rewritten to the absolute path of the
#'   simulation dataset (so it no longer depends on where the model file
#'   lives), and a column map `<target> = <source>` is kept only when it
#'   renames a sampled covariate that is present in the anonymized frame and
#'   its target does not collide with a column left in place by the retained
#'   renames (see `retain_data_maps()`). Maps onto (or from) the
#'   structural columns are dropped, because the anonymized frame already uses
#'   the canonical names (`ID`, `TIME`, `EVID`, `AMT`, `DV`, ...). A covariate
#'   the model references under a spelling that differs only by case from its
#'   column gets an added rename (see `ferx_covariate_case_bridges()`); if no
#'   `[data]` block exists, one is appended for it.
#'
#' * a relative `[priors]` `from_fit` path, which FeRx resolves against the
#'   model file's directory, is made absolute against the directory of
#'   `model_file`, so it still resolves from the relocated copy.
#'
#' Lines are classified after stripping `#` and `//` comments, as FeRx does.
#' All other lines, including comments, are copied verbatim.
#'
#' @param model_file path to the original FeRx model file.
#' @param data_path path to the simulation dataset (CSV).
#' @param columns column names of the simulation dataset.
#' @param path path to write the simulation copy to. Defaults to a temporary
#'   `.ferx` file.
#'
#' @returns `path`, invisibly.
#'
#' @noRd
prepare_simulation_model_file <- function(
  model_file,
  data_path,
  columns,
  path = tempfile(fileext = ".ferx")
) {
  lines <- readLines(model_file, warn = FALSE)
  model_dir <- dirname(normalizePath(model_file, winslash = "/", mustWork = FALSE))
  parsed <- parse_ferx_model_lines(lines)
  stripped <- parsed$stripped
  is_header <- parsed$is_header
  block <- parsed$block
  split_key_value <- function(i) {
    content <- trimws(stripped[i])
    if (!nzchar(content) || !grepl("=", content, fixed = TRUE)) return(NULL)
    value <- trimws(sub("^[^=]*=", "", content))
    list(
      key = trimws(sub("=.*$", "", content)),
      value = gsub("^[\"']|[\"']$", "", value)
    )
  }
  indent_of <- function(i) sub("^(\\s*).*$", "\\1", lines[i])

  # `[priors] from_fit` is resolved by FeRx against the model file's directory;
  # the copy lives elsewhere, so make a relative reference absolute against the
  # original model's directory.
  for (i in which(!is_header & !is.na(block) & block == "priors")) {
    kv <- split_key_value(i)
    if (is.null(kv) || kv$key != "from_fit" || !nzchar(kv$value)) next
    if (!is_absolute_path(kv$value)) {
      abs_fit <- normalizePath(
        file.path(model_dir, kv$value), winslash = "/", mustWork = FALSE
      )
      lines[i] <- paste0(indent_of(i), "from_fit = \"", abs_fit, "\"")
    }
  }

  drop_blocks <- c("data_selection", "initial_values")
  keep <- is.na(block) | !block %in% drop_blocks

  data_body <- which(!is_header & !is.na(block) & block == "data")
  if (length(data_body) > 0) {
    covariate_cols <- setdiff(columns, anonymized_structural_columns())
    path_written <- FALSE
    abs_data_path <- normalizePath(data_path, winslash = "/", mustWork = FALSE)
    map_idx <- integer(0)
    map_target <- character(0)
    map_source <- character(0)
    map_key <- character(0)
    for (i in data_body) {
      kv <- split_key_value(i)
      if (is.null(kv)) next
      key <- kv$key
      value <- kv$value
      if (tolower(key) == "path") {
        if (path_written) {
          keep[i] <- FALSE
        } else {
          lines[i] <- paste0(indent_of(i), "path = \"", abs_data_path, "\"")
          path_written <- TRUE
          path_idx <- i
        }
        next
      }
      if (toupper(value) %in% toupper(covariate_cols)) {
        map_idx <- c(map_idx, i)
        map_target <- c(map_target, toupper(key))
        map_source <- c(map_source, toupper(value))
        map_key <- c(map_key, key)
      } else {
        keep[i] <- FALSE
      }
    }
    retained <- retain_data_maps(map_target, map_source, toupper(columns))
    keep[map_idx[!retained]] <- FALSE
    map_key <- map_key[retained]
    map_source <- map_source[retained]
  } else {
    map_key <- character(0)
    map_source <- character(0)
  }

  # Columns as FeRx will see them once the retained `[data]` renames are
  # applied (FeRx resolves rename sources case-insensitively).
  renamed <- columns
  for (j in seq_along(map_key)) {
    renamed[toupper(columns) == map_source[j]] <- map_key[j]
  }
  # FeRx matches the model's covariate references to data columns
  # case-sensitively, so a covariate the model spells `CONC0` is not found in a
  # `conc0` column. Bridge case-only differences with extra `[data]` renames.
  bridges <- ferx_covariate_case_bridges(
    stripped = stripped,
    block = block,
    is_header = is_header,
    columns = renamed,
    renamable = setdiff(
      columns[!toupper(columns) %in% map_source],
      anonymized_structural_columns()
    )
  )
  bridge_lines <- if (length(bridges) > 0) {
    paste0("  ", names(bridges), " = ", unname(bridges))
  } else {
    character(0)
  }

  if (length(data_body) > 0) {
    insert_at <- if (path_written) path_idx else which(is_header & block == "data")[1]
    if (!path_written) {
      # A `[data]` block without `path` fails to parse; add the simulation path
      # right after the block header.
      bridge_lines <- c(paste0("  path = \"", abs_data_path, "\""), bridge_lines)
    }
    lines <- append(lines, bridge_lines, after = insert_at)
    keep <- append(keep, rep(TRUE, length(bridge_lines)), after = insert_at)
  } else if (length(bridge_lines) > 0) {
    abs_data_path <- normalizePath(data_path, winslash = "/", mustWork = FALSE)
    lines <- c(lines, "", "[data]", paste0("  path = \"", abs_data_path, "\""), bridge_lines)
    keep <- c(keep, rep(TRUE, 3 + length(bridge_lines)))
  }

  writeLines(lines[keep], path)
  invisible(path)
}

#' Identifiers a FeRx model file references and defines
#'
#' A lightweight, conservative scan of a FeRx model file (it does not
#' reimplement the FeRx parser). Identifiers are collected from the model's
#' expression blocks, i.e. every block except `[parameters]`, `[data]`,
#' `[data_selection]`, `[initial_values]`, `[fit_options]`, `[priors]`,
#' `[simulation]` and `[output]`, after stripping comments and quoted strings.
#' An identifier is *defined* when it is assigned (`NAME =`, including named
#' arguments such as `cl = CL`) or declared in `[parameters]`, and a *call* when
#' it is followed by `(`. Everything else is a candidate covariate reference.
#'
#' @param stripped model-file lines with comments stripped.
#' @param block lower-cased block name of each line (`NA` before the first
#'   header).
#' @param is_header logical, whether each line is a block header.
#'
#' @returns a list with character vectors `referenced` (candidate covariate
#'   references, not defined anywhere in the model) and `defined`.
#'
#' @noRd
ferx_model_identifiers <- function(stripped, block, is_header) {
  ident_re <- "(?<![A-Za-z0-9_.])[A-Za-z_][A-Za-z0-9_]*"
  extract <- function(text, pattern) {
    unlist(regmatches(text, gregexpr(pattern, text, perl = TRUE)))
  }
  body <- !is_header & !is.na(block)
  text <- gsub("\"[^\"]*\"|'[^']*'", " ", stripped)
  param_text <- text[body & block == "parameters"]
  non_expression <- c(
    "parameters", "data", "data_selection", "initial_values", "fit_options",
    "priors", "simulation", "output"
  )
  expr_text <- text[body & !block %in% non_expression]
  # ODE compartments: `states = [a, b]`, `d/dt(a)` and `obs_cmt = a`
  state_lists <- extract(expr_text, "states\\s*=\\s*\\[[^]]*\\]")
  states <- c(
    extract(sub("^[^[]*", "", state_lists), ident_re),
    sub("^d\\s*/\\s*dt\\s*\\(\\s*", "", extract(
      expr_text, "(?<![A-Za-z0-9_])d\\s*/\\s*dt\\s*\\(\\s*[A-Za-z_][A-Za-z0-9_]*"
    )),
    sub("^obs_cmt\\s*=\\s*", "", extract(
      expr_text, "(?<![A-Za-z0-9_])obs_cmt\\s*=\\s*[A-Za-z_][A-Za-z0-9_]*"
    ))
  )
  defined <- unique(c(
    extract(param_text, ident_re),
    extract(expr_text, paste0(ident_re, "(?=\\s*=(?!=))")),
    states
  ))
  calls <- extract(expr_text, paste0(ident_re, "(?=\\s*\\()"))
  referenced <- extract(expr_text, paste0(ident_re, "(?![A-Za-z0-9_])(?!\\s*[(=])"))
  # `==` comparisons are references, not assignments
  referenced <- c(referenced, extract(expr_text, paste0(ident_re, "(?=\\s*==)")))
  referenced <- setdiff(unique(referenced), c(defined, calls, ferx_grammar_tokens()))
  list(referenced = referenced, defined = defined)
}

#' FeRx grammar keywords that are never covariate references
#'
#' Bare words of the model-file syntax that the identifier scan would
#' otherwise read as references: structural-model keywords (`pk`, `ode`,
#' `ode_template`), the `d/dt` derivative notation, conditional and logical
#' keywords, `[covariates]` type words, and literals. Matched exactly (FeRx
#' keywords are lower case), so an upper-case `PK` covariate is still a
#' reference.
#'
#' @noRd
ferx_grammar_tokens <- function() {
  c(
    "pk", "ode", "ode_template", "d", "dt", "states", "obs_cmt",
    "if", "then", "else", "elif", "elseif", "end", "and", "or", "not",
    "continuous", "cont", "categorical", "cat", "levels", "auto",
    "true", "false", "TRUE", "FALSE", "inf", "Inf", "nan", "NaN"
  )
}

#' `[data]` renames that bridge case-only covariate name differences
#'
#' FeRx matches a model's covariate references to data columns
#' case-sensitively. When the model references a name (e.g. `CONC0`) that no
#' column carries exactly, but exactly one renamable covariate column matches
#' it case-insensitively (e.g. `conc0`), a `CONC0 = conc0` rename makes the
#' column available under the model's spelling. A reference that matches
#' several columns case-insensitively, or several references that differ only
#' by case and match the same column, cannot be bridged and are reported as an
#' error naming the mismatch. A column the model already references under its
#' exact spelling is never renamed.
#'
#' @param stripped model-file lines with comments stripped.
#' @param block,is_header block name of each line and header flags.
#' @param columns column names as FeRx sees them after the retained `[data]`
#'   renames.
#' @param renamable covariate columns not already renamed by a retained
#'   `[data]` map, i.e. those a bridge may rename.
#'
#' @returns named character vector `c(<model name> = <column>)` of the renames
#'   to add (possibly empty).
#'
#' @noRd
ferx_covariate_case_bridges <- function(stripped, block, is_header, columns,
                                        renamable) {
  all_refs <- ferx_model_identifiers(stripped, block, is_header)$referenced
  # only references that no column carries under the model's exact spelling,
  # and that some column carries under another case
  refs <- all_refs[!all_refs %in% columns & toupper(all_refs) %in% toupper(columns)]
  # a column the model references under its exact spelling is never renamed
  # away: a case-variant reference to it is then most likely not a covariate
  # reference at all (the identifier scan is heuristic), and renaming would
  # break the exact reference
  refs <- refs[!vapply(refs, function(ref) {
    any(columns[toupper(columns) == toupper(ref)] %in% all_refs)
  }, logical(1))]
  if (length(refs) == 0) return(character(0))
  structural <- toupper(anonymized_structural_columns())
  refs <- refs[!toupper(refs) %in% structural]
  bridges <- character(0)
  problems <- character(0)
  for (ref in refs) {
    matches <- columns[toupper(columns) == toupper(ref)]
    variants <- refs[toupper(refs) == toupper(ref)]
    if (length(matches) > 1) {
      problems <- c(problems, paste0(
        "`", ref, "` matches several columns that differ only by case (",
        paste0("`", matches, "`", collapse = ", "), ")"
      ))
    } else if (length(variants) > 1) {
      if (identical(ref, variants[[1]])) {
        problems <- c(problems, paste0(
          paste0("`", variants, "`", collapse = ", "),
          " differ only by case but must all resolve to column `", matches, "`"
        ))
      }
    } else if (matches %in% renamable) {
      bridges[[ref]] <- matches
    } else {
      problems <- c(problems, paste0(
        "`", ref, "` matches column `", matches, "` only case-insensitively, ",
        "and that column is already renamed in the `[data]` block"
      ))
    }
  }
  if (length(problems) > 0) {
    stop(
      "The model references covariate(s) whose spelling matches the ",
      "anonymized dataset's columns only case-insensitively, which FeRx does ",
      "not resolve: ", paste(problems, collapse = "; "), ". Spell the ",
      "covariates in the model as they are spelled in `data`, or map them in ",
      "the model's `[data]` block (e.g. `CONC0 = conc0`).",
      call. = FALSE
    )
  }
  bridges
}

#' Classify the lines of a FeRx model file by block
#'
#' Comments are stripped the way FeRx does (`#` and `//`) before any line is
#' classified, so a commented header or key is read as FeRx reads it.
#'
#' @param lines raw model-file lines.
#'
#' @returns a list with `stripped` (lines without comments), `is_header`
#'   (logical) and `block` (lower-cased name of the block each line belongs
#'   to, `NA` before the first header).
#'
#' @noRd
parse_ferx_model_lines <- function(lines) {
  stripped <- strip_ferx_comment(lines)
  header_re <- "^\\s*\\[\\s*([A-Za-z_][A-Za-z0-9_]*)(\\s+[^]]*)?\\]\\s*$"
  is_header <- grepl(header_re, stripped)
  block <- rep(NA_character_, length(lines))
  current <- NA_character_
  for (i in seq_along(lines)) {
    if (is_header[i]) {
      current <- tolower(sub(header_re, "\\1", stripped[i]))
    }
    block[i] <- current
  }
  list(stripped = stripped, is_header = is_header, block = block)
}

#' Check the model's covariate references against the anonymized frame
#'
#' Runs before any covariate is sampled, so a column-naming problem is reported
#' as such instead of surfacing from inside the FeRx simulation. It prepares
#' the simulation copy of `model_file` (see `prepare_simulation_model_file()`)
#' for the columns the anonymized frame will carry, which bridges covariates
#' the model spells in another case and errors on references that cannot be
#' bridged. It also warns about model references that match a column of
#' `data` that is in neither `covariates` nor `design_cols` (and so not in the
#' anonymized frame).
#'
#' @param model_file path to the FeRx model file.
#' @param data original data.frame.
#' @param covariates covariates to sample.
#' @param dictionary normalized column dictionary.
#' @param design_cols design columns cloned verbatim into the anonymized frame.
#'
#' @returns `invisible(TRUE)`.
#'
#' @noRd
check_anonymize_model_covariates <- function(model_file, data, covariates, dictionary,
                                             design_cols = NULL) {
  optional <- c("CMT", "MDV", "ADDL", "II", "SS")
  # mirrors the columns of `build_anonymized_simulation_input()`: design
  # columns are cloned under their own names
  event_cols <- unique(c(
    unlist(dictionary[c("ID", "TIME", "EVID", "AMT", "RATE")], use.names = FALSE),
    optional
  ))
  design_cols <- setdiff(unique(design_cols), event_cols)
  frame_cols <- unique(c(
    "ID", "TIME", "EVID", "AMT",
    if (dictionary$RATE %in% names(data)) "RATE",
    "DV",
    intersect(optional, names(data)),
    design_cols,
    covariates
  ))
  sim_model <- tempfile(fileext = ".ferx")
  on.exit(unlink(sim_model), add = TRUE)
  prepare_simulation_model_file(
    model_file = model_file,
    data_path = tempfile(fileext = ".csv"),
    columns = frame_cols,
    path = sim_model
  )

  parsed <- parse_ferx_model_lines(readLines(sim_model, warn = FALSE))
  refs <- ferx_model_identifiers(parsed$stripped, parsed$block, parsed$is_header)$referenced
  data_maps <- parsed$stripped[!parsed$is_header & parsed$block %in% "data"]
  data_maps <- trimws(sub("=.*$", "", data_maps[grepl("=", data_maps, fixed = TRUE)]))
  available <- toupper(c(frame_cols, data_maps))
  structural <- unique(c(
    toupper(anonymized_structural_columns()),
    toupper(unlist(dictionary, use.names = FALSE))
  ))
  dropped <- setdiff(
    names(data),
    c(covariates, design_cols, unlist(dictionary, use.names = FALSE))
  )
  dropped <- dropped[!toupper(dropped) %in% structural]
  refs <- refs[!toupper(refs) %in% c(available, structural)]
  missing <- dropped[toupper(dropped) %in% toupper(refs)]
  if (length(missing) > 0) {
    warning(
      "The model appears to reference column(s) of `data` that are not in ",
      "`covariates` or `design_cols`, so they are not part of the anonymized ",
      "dataset: ", paste0("`", missing, "`", collapse = ", "), ". Add them to ",
      "`covariates` (sampled) or `design_cols` (cloned from the source design) ",
      "if the model needs them, or the simulation will fail.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Strip a trailing FeRx line comment
#'
#' FeRx ends a model-file line at the first `#` or `//`, whichever comes first
#' (`strip_line_comment()` in ferx-core). Vectorized over `lines`.
#'
#' @noRd
strip_ferx_comment <- function(lines) {
  sub("(#|//).*$", "", lines)
}

#' Whether a file path is absolute (POSIX, home-relative or Windows)
#'
#' @noRd
is_absolute_path <- function(path) {
  grepl("^(/|~|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)
}

#' Decide which `[data]` column renames can be applied together
#'
#' FeRx resolves all `[data]` renames against the original headers at once, so
#' a rename may target a column that another retained rename moves aside (e.g.
#' `HTCM = WT` together with `WT = HT`). A rename is retained when its target
#' is not mapped by an earlier retained rename and does not collide with a
#' column that survives under its own name, i.e. one that no retained rename
#' moves away. Dropping a rename puts its source column back under its own
#' name, which can create new collisions, so this iterates to a fixed point.
#'
#' @param target,source upper-cased targets and sources of the candidate
#'   renames (sources are already known to be present in the data).
#' @param columns upper-cased column names of the simulation dataset.
#'
#' @returns logical vector, `TRUE` for renames to keep.
#'
#' @noRd
retain_data_maps <- function(target, source, columns) {
  keep <- !duplicated(target) & !duplicated(source)
  repeat {
    surviving <- setdiff(columns, source[keep & target != source])
    collides <- keep & target != source & target %in% surviving
    if (!any(collides)) break
    keep <- keep & !collides
  }
  keep
}

apply_simulated_concentrations <- function(data, sim) {
  if (!"DV_SIM" %in% names(sim)) {
    stop("FeRx simulation output must contain a `DV_SIM` column.", call. = FALSE)
  }
  # FeRx only simulates records that carry an observation: `EVID == 0` rows
  # flagged `MDV == 1` (e.g. scheduled samples that were never taken) get no
  # simulated row. Count observations the same way, and give those records a
  # missing `DV` so they keep `MDV = 1` downstream.
  evid_obs <- !is.na(data$EVID) & data$EVID == 0
  if ("MDV" %in% names(data)) {
    mdv_flagged <- evid_obs & !is.na(data$MDV) & data$MDV != 0
  } else {
    mdv_flagged <- rep(FALSE, nrow(data))
  }
  obs_idx <- which(evid_obs & !mdv_flagged)
  if (length(obs_idx) != nrow(sim)) {
    n_evid_obs <- sum(evid_obs)
    if (n_evid_obs == nrow(sim)) {
      # FeRx returned a row for every `EVID == 0` record, including the
      # `MDV == 1` ones; use them all.
      obs_idx <- which(evid_obs)
      mdv_flagged <- rep(FALSE, nrow(data))
    } else {
      msg <- paste0(
        "FeRx returned ", nrow(sim), " rows for ", n_evid_obs,
        " `EVID == 0` records"
      )
      n_flagged <- sum(mdv_flagged)
      if (n_flagged > 0) {
        msg <- paste0(
          msg, ". ", n_flagged, " of them are flagged `MDV == 1` and carry no ",
          "observation, leaving ", length(obs_idx), " observation records; ",
          "the remaining mismatch could not be explained."
        )
      } else {
        msg <- paste0(msg, "; expected one simulated row per observation record.")
      }
      stop(msg, call. = FALSE)
    }
  }
  data[["DV"]][mdv_flagged] <- NA
  # Match simulated concentrations back onto observation rows by ID and TIME
  # (and CMT when available) rather than by position: FeRx is not guaranteed to
  # return rows in the input order, so a positional copy can silently scramble
  # subjects/timepoints. Records sharing the same key (e.g. replicate samples)
  # are matched in order of occurrence via `.occ`.
  if (all(c("ID", "TIME") %in% names(sim))) {
    obs <- data[obs_idx, intersect(c("ID", "TIME", "CMT"), names(data)), drop = FALSE]
    sim_keyed <- sim
    if ("CMT" %in% names(sim) && "CMT" %in% names(data)) {
      # A blank `CMT` on an observation record means "the model's default
      # compartment", and an `NA` key matches nothing in `merge()`. Fill blanks
      # the way FeRx does (compartment 1) on both sides so populated
      # compartments still disambiguate records sharing ID/TIME.
      obs$CMT[is.na(obs$CMT)] <- 1
      sim_keyed$CMT[is.na(sim_keyed$CMT)] <- 1
      merged <- match_sim_by_keys(obs, sim_keyed, c("ID", "TIME", "CMT"))
      if (is.null(merged) &&
          (anyNA(data$CMT[obs_idx]) || anyNA(sim$CMT)) &&
          !any_key_spans_compartments(data[obs_idx, , drop = FALSE])) {
        # The default compartment FeRx assigned to blank records differs from
        # 1. Without CMT, records are matched by ID/TIME and occurrence; that
        # is only safe when no ID/TIME pair is observed in more than one
        # compartment, which was just checked.
        merged <- match_sim_by_keys(obs, sim_keyed, c("ID", "TIME"))
      }
      keys <- c("ID", "TIME", "CMT")
    } else {
      keys <- c("ID", "TIME")
      merged <- match_sim_by_keys(obs, sim_keyed, keys)
    }
    if (is.null(merged)) {
      stop(
        "FeRx simulation output could not be uniquely matched to observation ",
        "records by ", paste(keys, collapse = ", "), ".",
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

# Match FeRx `sim` rows onto observation rows `obs` by `keys`, pairing records
# that share a key in order of occurrence. Returns the merged rows (with
# `.obs_order` and `DV_SIM` columns), or `NULL` when the rows cannot all be matched one-to-one.
match_sim_by_keys <- function(obs, sim, keys) {
  occurrence <- function(df) {
    grp <- interaction(df[keys], drop = TRUE, lex.order = TRUE)
    stats::ave(seq_len(nrow(df)), grp, FUN = seq_along)
  }
  obs <- obs[, keys, drop = FALSE]
  obs[[".obs_order"]] <- seq_len(nrow(obs))
  obs[[".occ"]] <- occurrence(obs)
  sim <- sim[, c(keys, "DV_SIM"), drop = FALSE]
  sim[[".occ"]] <- occurrence(sim)
  merged <- merge(obs, sim, by = c(keys, ".occ"), sort = FALSE)
  if (nrow(merged) != nrow(obs)) return(NULL)
  merged
}

# TRUE when some ID/TIME pair among the observation records appears with more
# than one distinct CMT value (a blank CMT counts as its own value).
any_key_spans_compartments <- function(obs) {
  cmt <- ifelse(is.na(obs$CMT), "<NA>", as.character(obs$CMT))
  n_cmt <- tapply(cmt, paste(obs$ID, obs$TIME, sep = "\r"), function(x) {
    length(unique(x))
  })
  any(n_cmt > 1)
}

#' Recompute MDV from the final EVID and DV values
#'
#' The `MDV` column is cloned from the source data, but `DV` is replaced by the
#' simulated concentrations (and possibly by BLQ handling), so the source flags
#' no longer apply. Non-observation records and observations without a `DV` get
#' `MDV = 1`; all other observations (including censored ones, which are
#' flagged via `CENS`) get `MDV = 0`.
#'
#' @noRd
recompute_anonymized_mdv <- function(data) {
  if (!"MDV" %in% names(data)) {
    return(data)
  }
  data$MDV <- as.integer(is.na(data$EVID) | data$EVID != 0 | is.na(data$DV))
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
