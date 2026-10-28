# Augment binary data with weighted pseudo-observations

For each predictor (excluding the intercept) and each outcome category,
two pseudo-observations are added at the predictor mean +/- half a
standard deviation (other predictors at their mean). The
pseudo-observations have a total weight of `p + 1`, with `p` the number
of predictors (White, Daniel & Royston, 2010).

## Usage

``` r
mice_augment(y, X)
```

## Arguments

- y:

  integer outcome vector (0/1)

- X:

  design matrix (first column is the intercept)

## Value

list with augmented `y`, `X`, and weights `w`
