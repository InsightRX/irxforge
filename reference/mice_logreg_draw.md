# Draw parameters of a Bayesian logistic regression model

Fits a logistic regression on data augmented with pseudo-observations
(White, Daniel & Royston, 2010) to prevent perfect prediction, and draws
coefficients from the asymptotic normal distribution of the estimates.

## Usage

``` r
mice_logreg_draw(y, X)
```

## Arguments

- y:

  integer outcome vector (0/1)

- X:

  design matrix (first column is the intercept)

## Value

numeric vector of drawn coefficients (aliased coefficients are 0)
