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
  cat_covs = NULL,
  noise = NULL,
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

- cat_covs:

  character vector of categorical covariates. Used to filter categorical
  `conditional` limits by value and to exclude these columns from the
  multiplicative `noise` jitter.

- noise:

  optional single non-negative number giving the log-scale SD of
  multiplicative log-normal noise applied to the sampled continuous
  covariates (e.g. `0.05` for ~5%), to obscure the original values.
  `NULL` (default) applies no noise.

- ...:

  additional arguments (ignored).

## Value

a data.frame with the simulated covariates, with `n_subjects` rows and
`p` columns
