# Sample covariates using a variety of methods

Sample covariates using a variety of methods

## Usage

``` r
sample_covariates(
  method = c("mvtnorm", "mice", "bootstrap", "nhanes"),
  seed = NULL,
  ...
)
```

## Arguments

- method:

  sampling method, one of `mvtnorm`, `bootstrap`, `mice`, or `nhanes`.
  E.g. `list(AGE = c(60, 80), WT = c(70, 100))`.

- seed:

  integer random seed passed to
  [`set.seed()`](https://rdrr.io/r/base/Random.html) for
  reproducibility. Default `NULL` does not set a seed.

- ...:

  arguments passed to lower-level function(s).

## Value

data.frame with covariates in each column
