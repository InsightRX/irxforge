# Draw parameters of a Bayesian linear regression model

Following Rubin (1987, p. 167), as implemented in `mice:::.norm.draw()`.

## Usage

``` r
mice_norm_draw(y, X, ridge = 1e-05)
```

## Arguments

- y:

  numeric outcome vector (observed values)

- X:

  design matrix for observed values

- ridge:

  ridge penalty used in the linear regression draws, to stabilize the
  estimation in case of collinear predictors.

## Value

list with least-squares estimate `coef`, drawn coefficients `beta` and
drawn residual standard deviation `sigma`
