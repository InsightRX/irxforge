# Draw parameters of a Bayesian linear regression model

Following Rubin (1987, p. 167), as implemented in `mice:::.norm.draw()`.

## Usage

``` r
mice_norm_draw(y, X, ridge = 1e-05, var = NULL)
```

## Arguments

- y:

  numeric outcome vector (observed values)

- X:

  design matrix for observed values

- ridge:

  ridge penalty used in the linear regression draws, to stabilize the
  estimation in case of collinear predictors.

- var:

  name of the variable being imputed, used in messages.

## Value

list with least-squares estimate `coef`, drawn coefficients `beta` and
drawn residual standard deviation `sigma`

## Details

The penalized normal equations are solved on the correlation scale (the
columns of `X` scaled to unit root-mean-square), which is mathematically
identical to applying the ridge penalty `ridge * diag(crossprod(X))` but
prevents predictors on very different scales from making the system
numerically singular. If the system is still singular (e.g. because of
(near-)collinear predictors), a pseudo-inverse is used instead and a
warning naming the imputed variable and the collinear predictors is
raised.
