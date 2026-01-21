# Sample covariates from multivariate normal distributions

Sample covariates from multivariate normal distributions

## Usage

``` r
sample_covariates_mvtnorm(
  data,
  cat_covs = NULL,
  n_subjects = nrow(data),
  exponential = FALSE,
  conditional = NULL,
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

- n_subjects:

  number of simulated subjects, default is the number of subjects in the
  data.

- exponential:

  sample from exponential distribution? Default `FALSE`.

- conditional:

  description...

- ...:

  additional arguments passed to `mvrnorm()` function

## Value

a data.frame with the simulated covariates, with `n_subjects` rows and
`p` columns

## Note

missing values in `data` must be coded as NA
