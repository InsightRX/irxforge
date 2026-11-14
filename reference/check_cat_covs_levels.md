# Reject categorical covariates with implausibly many levels

Each categorical covariate is imputed with a (multinomial) logistic
regression, whose cost grows quickly with the number of levels. A
categorical covariate with hundreds of levels is almost always a
continuous column (e.g. a time or dose counter) passed in `cat_covs` by
mistake, and would make the sampler run for hours without any output.
Covariates listed in `cat_covs`, as well as factor and character columns
(which are always treated as categorical), are checked against
`max_cat_levels`.

## Usage

``` r
check_cat_covs_levels(data, covs, cat_covs = NULL, max_cat_levels = 20)
```

## Arguments

- data:

  data.frame with covariates

- covs:

  names of the covariates to check

- cat_covs:

  names of the covariates declared categorical

- max_cat_levels:

  maximum number of distinct (non-missing) values allowed for a
  categorical covariate. `Inf` disables the check.

## Value

named integer vector with the number of levels of each categorical
covariate, invisibly
