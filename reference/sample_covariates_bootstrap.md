# Sample covariates using bootstrap

Sample covariates using bootstrap

## Usage

``` r
sample_covariates_bootstrap(
  data,
  n_subjects = nrow(data),
  conditional = NULL,
  seed = NULL,
  na.rm = TRUE,
  ...
)
```

## Arguments

- data:

  data.frame (n x p) containing the original, observed, time-invariant
  covariates (ID should not be included) that will be used to inform the
  imputation.

- n_subjects:

  number of simulated subjects, default is the number of subjects in the
  data.

- conditional:

  list with conditional limits for sampled population. For continuous
  covariates, specify a numeric vector of length 2 giving the
  `c(min, max)` range, e.g. `list("WT" = c(40, 60), "BMI" = c(15, 25))`.
  For categorical covariates (those listed in `cat_covs`), specify a
  character vector of the allowed category values, e.g.
  `list("SEX" = c("F"))`.

- seed:

  integer random seed passed to
  [`set.seed()`](https://rdrr.io/r/base/Random.html) for
  reproducibility. Default `NULL` does not set a seed.

- na.rm:

  logical. If `TRUE` (default), rows with `NA` in any column are dropped
  before sampling.

- ...:

  additional arguments passed to
  [`mice::mice()`](https://amices.org/mice/reference/mice.html) function

## Value

a data.frame with the simulated covariates, with `n_subjects` rows and
`p` columns
