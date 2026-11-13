# Identify (near-)collinear predictors in a design matrix

Uses a pivoted QR decomposition of the column-scaled design matrix.
Columns that are (near-)linear combinations of the others are reported,
together with the columns they depend on.

## Usage

``` r
mice_collinear_predictors(X, tol = 1e-07)
```

## Arguments

- X:

  design matrix with column names

- tol:

  tolerance for the QR rank determination

## Value

character vector with names of the collinear predictors (without the
intercept), possibly empty
