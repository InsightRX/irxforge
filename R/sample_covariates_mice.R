#' Sample covariates using multivariate imputation using chained equations
#' (mice)
#'
#' @param data data.frame (n x p) containing the original, observed,
#' time-invariant covariates (ID should not be included) that will be used to
#' inform the imputation.
#' @param cat_covs character vector containing the names of the categorical
#' covariates in orgCovs.
#' @param n_subjects number of simulated subjects, default is the number of
#' subjects in the data.
#' @param conditional list with conditional limits for sampled population, e.g. 
#' `list("WT" = c(40, 60), "BMI" = c(15, 25))`.
#' @param cont_method method used to predict continuous covariates within mice,
#' default is `pmm`.
#' @param iterations number of MICE iterations. Default is 1.
#' @param ... additional arguments passed to `mice::mice()` function
#'
#' @returns data.frame with the simulated covariates, with n_subjects * m
#' rows and p columns
#'
#' @details missing values in `data` must be coded as NA
#'
#' @export
#'
sample_covariates_mice <- function(
  data,
  cat_covs = NULL,
  conditional = NULL,
  n_subjects = nrow(data),
  cont_method = "pmm",
  iterations = 1,
  ...
) {

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
      pool_seed <- dplyr::filter(
        pool_seed,
        .data[[key]] >= min(conditional[[key]]) & 
        .data[[key]] <= max(conditional[[key]])
      ) 
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
      m = 1,
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
