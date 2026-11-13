# Warn about integer-coded covariates that will be treated as continuous

Numeric covariates with only whole-number values and 2 or 3 distinct
values (e.g. `SEX = 0/1`, `FED = 0/1`) are most likely categorical
codes. Covariates with fewer than 10 non-missing values are not checked.
If they are not listed in `cat_covs`, they are imputed as continuous,
which gives non-integer values and can make the imputation models
(nearly) singular.

## Usage

``` r
warn_integer_coded_covariates(data, covs, cat_covs = NULL)
```

## Arguments

- data:

  data.frame with covariates

- covs:

  names of the covariates to check

- cat_covs:

  names of the covariates declared categorical

## Value

names of the suspicious covariates, invisibly
