# Sample covariates using a variety of methods

Sample covariates using a variety of methods

## Usage

``` r
sample_covariates(method = c("mvtnorm", "mice", "bootstrap"), ...)
```

## Arguments

- method:

  sampling method, one of `mvtnorm`, `bootstrap`, or `mice`. E.g.
  `list(AGE = c(60, 80), WT = c(70, 100))`.

- ...:

  arguments passed to lower-level function(s).

## Value

data.frame with covariates in each column
