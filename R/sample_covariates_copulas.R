#' Sample covariates using copula modeling
#'
#' Implements the copula-based virtual-patient covariate simulation of Zwep et
#' al. (2024). A vine copula with kernel-density marginals is fitted to the
#' observed covariates and new virtual subjects are drawn from it, reproducing
#' both the individual covariate distributions and their full dependence
#' structure. This is the cross-sectional (time-invariant) counterpart of
#' [sample_covariates_copulas_timevarying()].
#'
#' @param data data.frame or tibble (n x p) of observed, time-invariant
#'   covariates, one row per subject (an ID column should not be included). Only
#'   continuous covariates are supported.
#' @param conditional optional named list of `c(min, max)` ranges restricting the
#'   sampled population, e.g. `list(AGE = c(60, 80), WT = c(70, 100))`. The
#'   observed data are filtered to these ranges before the copula is fitted.
#' @param n_subjects number of simulated subjects. Default is the number of rows
#'   in `data`. Unlike resampling methods, the copula is generative, so
#'   `n_subjects` may exceed the number of observed subjects.
#' @param family_set copula families considered by `rvinecopulib::vine()`.
#'   Default `"all"`.
#' @param selcrit model-selection criterion for the vine copula, default
#'   `"aic"`. Use `"loglik"` for the tightest in-sample reproduction.
#' @param bw_mult bandwidth multiplier for the kernel-density marginals passed to
#'   `rvinecopulib::vine()` (`margins_controls$mult`). Default `1`. Values below
#'   `1` reduce marginal smoothing, tightening the simulated marginals toward the
#'   observed distribution.
#' @param truncate logical; if `TRUE` (default) sampled covariate values are
#'   clamped to the observed `[min, max]` range of each covariate, preventing
#'   nonphysical extrapolated values.
#' @param replicates number of independent simulated datasets to sample. Default
#'   is 1. When greater than 1, a `.replicate` column is included.
#' @param seed integer random seed passed to [set.seed()] for reproducibility.
#'   Default `NULL` does not set a seed.
#' @param ... reserved for future use.
#'
#' @details
#' Only continuous covariates are supported; categorical covariates should be
#' handled with [sample_covariates_mice()]. Rows with missing values are dropped
#' before fitting. Covariates that are constant across the (filtered) data are
#' held fixed and re-attached after sampling.
#'
#' Requires the `rvinecopulib` package.
#'
#' @returns data.frame with the simulated covariates, with `n_subjects` rows
#'   (times `replicates`) and `p` columns.
#'
#' @references
#' Zwep LB, Guo T, Nagler T, Knibbe CAJ, Meulman JJ, van Hasselt JGC. Virtual
#' Patient Simulation Using Copula Modeling. Clin Pharmacol Ther.
#' 2024;115(4):795-804. \doi{10.1002/cpt.3099}
#'
#' @seealso [sample_covariates_copulas_timevarying()] for the time-varying
#'   variant.
#'
#' @export
sample_covariates_copulas <- function(
  data,
  conditional = NULL,
  n_subjects = nrow(data),
  family_set = "all",
  selcrit = "aic",
  bw_mult = 1,
  truncate = TRUE,
  replicates = 1,
  seed = NULL,
  ...
) {
  if (!requireNamespace("rvinecopulib", quietly = TRUE)) {
    stop(
      "Package `rvinecopulib` is required for `sample_covariates_copulas()`.",
      call. = FALSE
    )
  }
  if (!is.null(seed)) set.seed(seed)
  if (length(bw_mult) != 1 || !is.numeric(bw_mult) || is.na(bw_mult) || bw_mult <= 0) {
    stop("`bw_mult` must be a single positive number.", call. = FALSE)
  }
  if (replicates < 1) {
    stop("`replicates` must be at least 1.", call. = FALSE)
  }
  if (n_subjects < 1) {
    stop("`n_subjects` must be at least 1.", call. = FALSE)
  }

  is_tbl <- tibble::is_tibble(data)
  data <- as.data.frame(data)
  non_numeric <- names(data)[!vapply(data, is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop(
      "Only continuous covariates are supported; these are not numeric: ",
      paste(non_numeric, collapse = ", "),
      ". Use `sample_covariates_mice()` for categorical covariates.",
      call. = FALSE
    )
  }

  data <- data[stats::complete.cases(data), , drop = FALSE]
  if (!is.null(conditional)) {
    for (key in names(conditional)) {
      data <- data[
        data[[key]] >= min(conditional[[key]]) &
          data[[key]] <= max(conditional[[key]]),
        ,
        drop = FALSE
      ]
    }
  }
  if (nrow(data) == 0) {
    stop(
      "No observations remain to fit the copula after applying `conditional`.",
      call. = FALSE
    )
  }

  col_order <- names(data)
  observed_ranges <- lapply(data, range, na.rm = TRUE)

  # Constant covariates cannot be modelled by the copula's kernel marginals;
  # hold them fixed and re-attach after sampling.
  spread <- vapply(data, function(x) stats::sd(x, na.rm = TRUE), numeric(1))
  scale <- pmax(abs(vapply(data, function(x) mean(x, na.rm = TRUE), numeric(1))), 1)
  varying_cols <- names(data)[spread > 1e-8 * scale]
  fixed_values <- vapply(data[setdiff(col_order, varying_cols)], mean, numeric(1))
  if (length(varying_cols) == 0) {
    stop("No covariate has any variation to model.", call. = FALSE)
  }

  copula <- fit_copula_model(
    varying = data[, varying_cols, drop = FALSE],
    family_set = family_set,
    selcrit = selcrit,
    bw_mult = bw_mult
  )

  out <- vector("list", replicates)
  for (replicate_idx in seq_len(replicates)) {
    sampled <- simulate_copula_subjects(
      copula = copula,
      varying_cols = varying_cols,
      fixed_values = fixed_values,
      n_subjects = n_subjects
    )
    sampled <- sampled[, col_order, drop = FALSE]
    if (truncate) {
      for (cov in col_order) {
        rng <- observed_ranges[[cov]]
        sampled[[cov]] <- pmin(pmax(sampled[[cov]], rng[[1]]), rng[[2]])
      }
    }
    if (replicates > 1) {
      sampled[[".replicate"]] <- replicate_idx
    }
    out[[replicate_idx]] <- sampled
  }

  out <- dplyr::bind_rows(out)
  if (is_tbl) out <- tibble::as_tibble(out)
  out
}
