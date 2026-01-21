# Sample covariates using multivariate imputation using chained equations (mice)

Sample covariates using multivariate imputation using chained equations
(mice)

## Usage

``` r
sample_covariates_mice(
  data,
  cat_covs = NULL,
  conditional = NULL,
  n_subjects = nrow(data),
  cont_method = "pmm",
  replicates = 1,
  ...
)
```

## Arguments

- data:

  data.frame (n x p) containing the original, observed, time-invariant
  covariates (ID should not be included) that will be used to inform the
  imputation.

- cat_covs:

  character vector containing the names of the categorical covariates in
  orgCovs.

- conditional:

  list with conditional limits for sampled population, e.g.
  `list("WT" = c(40, 60), "BMI" = c(15, 25))`.

- n_subjects:

  number of simulated subjects, default is the number of subjects in the
  data.

- cont_method:

  method used to predict continuous covariates within mice, default is
  `pmm`.

- replicates:

  number of multiple imputations replicates to sample. Default is 1.

- ...:

  additional arguments passed to
  [`mice::mice()`](https://amices.org/mice/reference/mice.html) function

## Value

data.frame with the simulated covariates, with n_subjects \* m rows and
p columns

## Details

missing values in `data` must be coded as NA
