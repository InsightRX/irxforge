#' Sample covariates using multivariate imputation using chained equations
#' (mice)
#' 
#' In contrast to sampling methods bootstrap and NHANES, categorical covariates
#' need to be specified using the `cat_covs` argument, otherwise they will be 
#' treated as continuous.
#'
#' @param data data.frame (n x p) containing the original, observed,
#' time-invariant covariates (ID should not be included) that will be used to
#' inform the imputation.
#' @param cat_covs character vector containing the names of the categorical
#' covariates in orgCovs.
#' @param n_subjects number of simulated subjects, default is the number of
#' subjects in the data.
#' @param conditional list with conditional limits for sampled population. For
#' continuous covariates, specify a numeric vector of length 2 giving the
#' `c(min, max)` range, e.g. `list("WT" = c(40, 60), "BMI" = c(15, 25))`.
#' For categorical covariates (those listed in `cat_covs`), specify a character
#' vector of the allowed category values, e.g. `list("SEX" = c("F"))`.
#' @param cont_method method used to impute continuous covariates, either
#' `pmm` (predictive mean matching, default) or `norm` (Bayesian linear
#' regression).
#' @param replicates number of multiple imputations replicates to sample.
#' Default is 1.
#' @param seed integer random seed passed to [set.seed()] for reproducibility.
#' Default `NULL` does not set a seed.
#' @param ... additional arguments passed to the internal MICE implementation,
#' e.g. `maxit` (number of iterations, default 5) or `donors` (number of
#' donors for predictive mean matching, default 5).
#'
#' @details missing values in `data` must be coded as NA.
#'
#' The imputation uses a minimal, internal re-implementation of the subset of
#' the `mice` package that is needed here (predictive mean matching or
#' Bayesian linear regression for continuous covariates, logistic regression
#' for binary and polytomous regression for other categorical covariates).
#' This avoids a dependency on `mice` and its dependency tree.
#'
#' @returns data.frame with the simulated covariates, with n_subjects * m
#' rows and p columns
#'
#' @export
sample_covariates_mice <- function(
  data,
  cat_covs = NULL,
  conditional = NULL,
  n_subjects = nrow(data),
  cont_method = c("pmm", "norm"),
  replicates = 1,
  seed = NULL,
  ...
) {
  cont_method <- match.arg(cont_method)
  if (!is.null(seed)) set.seed(seed)

  is_tbl <- tibble::is_tibble(data)
  data <- as.data.frame(data)
  data[cat_covs] <- lapply(data[cat_covs], as.factor)
  rownames(data) <- NULL

  # names of continuous covariates
  cont_covs <- setdiff(names(data), cat_covs)
  method <- mice_default_method(data)
  method[cont_covs] <- cont_method

  # impute missing data once
  if (anyNA(data)) {
    data <- mice_impute(data, method = method, m = 1, maxit = 15)[[1]]
  }

  # empty rows (with column types preserved) for the simulated subjects
  mi_data <- data[rep(NA_integer_, n_subjects), , drop = FALSE]
  rownames(mi_data) <- NULL

  if(!is.null(conditional)) {
    seed_covs <- names(conditional)
    pool_seed <- data
    for(key in seed_covs) {
      if(key %in% cat_covs) {
        pool_seed <- dplyr::filter(pool_seed, .data[[key]] %in% conditional[[key]])
      } else {
        pool_seed <- dplyr::filter(
          pool_seed,
          .data[[key]] >= min(conditional[[key]]) &
          .data[[key]] <= max(conditional[[key]])
        )
      }
    }
    if (nrow(pool_seed) == 0) {
      stop("No subjects in `data` satisfy the `conditional` restrictions.")
    }
    mi_data[seed_covs] <- pool_seed[
      sample.int(nrow(pool_seed), n_subjects, replace = TRUE), seed_covs,
      drop = FALSE
    ]
  }

  comb <- rbind(data, mi_data)
  sim_rows <- nrow(data) + seq_len(n_subjects)

  ## Run MICE
  imp_data <- mice_impute(
    comb,
    method = method,
    m = replicates,
    ...
  )

  ## Refactor and return
  out <- do.call(rbind, lapply(imp_data, function(x) x[sim_rows, , drop = FALSE]))
  rownames(out) <- NULL
  if (is_tbl) out <- tibble::as_tibble(out)
  out

}
