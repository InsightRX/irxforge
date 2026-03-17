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
#' @param cont_method method used to predict continuous covariates within mice,
#' default is `pmm`.
#' @param replicates number of multiple imputations replicates to sample.
#' Default is 1.
#' @param seed integer random seed passed to [set.seed()] for reproducibility.
#' Default `NULL` does not set a seed.
#' @param ... additional arguments passed to `mice::mice()` function
#'
#' @details missing values in `data` must be coded as NA
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
  cont_method = "pmm",
  replicates = 1,
  seed = NULL,
  ...
) {
  if (!is.null(seed)) set.seed(seed)

  # names of continuous covariates
  cont_covs <- setdiff(names(data), cat_covs)
  miss_vars <- names(data)[colSums(is.na(data)) > 0]

  # impute missing data once with mice
  if(length(miss_vars) > 0) {
    data_f <- data |> # create copy of the original data set with factor version of categorical covariates
      # TODO: mutate_at() is superseded by across()
      dplyr::mutate_at(cat_covs, function(x) as.factor(x))
    imp1 <- suppressWarnings(mice::mice( # Suppress "Number of logged events"
      data_f,
      m = 1,
      printFlag = FALSE,
      maxit = 15
    ))
    data <- mice::complete(imp1)
  }
  mi_data <- data[1:n_subjects, ] |>
    # TODO: mutate_all() is superseded by across()
    dplyr::mutate_all(function(x) NA)

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
    mi_data[seed_covs] <- pool_seed[
      sample(1:nrow(pool_seed), n_subjects, replace = T), seed_covs
    ]
  }

  comb <- data |>
    dplyr::mutate(Type = "Original") |>
    dplyr::bind_rows(
      mi_data |>
        dplyr::mutate(Type = "Simulated")
    ) |>
    # TODO: mutate_at() is superseded by across()
    dplyr::mutate_at(cat_covs, function(x) as.factor(x))

  pred <- mice::make.predictorMatrix(comb)
  pred[, c("Type")] <- 0
  method <- mice::make.method(comb)
  method[cont_covs] <- cont_method

  ## Run MICE
  suppressWarnings( ## mice throws warning about partial matching that (in R 4+)
    imp_data <- mice::mice(
      comb,
      m = replicates,
      printFlag = FALSE,
      predictorMatrix = pred,
      method = method,
      ...
    )
  )

  ## Refactor and return
  out <- imp_data |>
    tidyr::complete(action = "long") |>
    dplyr::filter(.data$Type == "Simulated") |>
    dplyr::select(-".id", -"Type", -".imp")
  if (tibble::is_tibble(data)) out <- tibble::as_tibble(out)
  out
  
}
