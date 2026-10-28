# Draw imputations for a single variable

Draw imputations for a single variable

## Usage

``` r
mice_impute_variable(y, ry, X, method, donors = 5L, ridge = 1e-05)
```

## Arguments

- y:

  vector with current (completed) values of the variable

- ry:

  logical vector, `TRUE` where `y` was originally observed

- X:

  design matrix

- method:

  named character vector with the imputation method per variable.
  Variables not in `method` get a default method based on their type:
  `pmm` for numeric, `logreg` for factors with 2 levels, and `polyreg`
  for factors with more than 2 levels.

- donors:

  number of donors to sample from in predictive mean matching.

- ridge:

  ridge penalty used in the linear regression draws, to stabilize the
  estimation in case of collinear predictors.

## Value

vector of imputed values for `y[!ry]`
