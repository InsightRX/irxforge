# Multivariate imputation by chained equations

Minimal, dependency-free re-implementation of the subset of the `mice`
package (van Buuren & Groothuis-Oudshoorn, 2011) that is used by
[`sample_covariates_mice()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice.md).
Missing values are imputed variable-by-variable using a Gibbs sampler:
each incomplete variable is regressed on all other (current, completed)
predictors, and new imputations are drawn from the posterior predictive
distribution of that model. This is repeated for `maxit` iterations,
independently for each of the `m` replicates.

## Usage

``` r
mice_impute(
  data,
  method = NULL,
  predictors = names(data),
  m = 1,
  maxit = 5,
  donors = 5L,
  ridge = 1e-05
)
```

## Arguments

- data:

  data.frame with missing values coded as `NA`. Categorical variables
  must be factors.

- method:

  named character vector with the imputation method per variable.
  Variables not in `method` get a default method based on their type:
  `pmm` for numeric, `logreg` for factors with 2 levels, and `polyreg`
  for factors with more than 2 levels.

- predictors:

  character vector with names of variables that may be used as
  predictors. Default is all variables in `data`.

- m:

  number of imputed data sets (replicates).

- maxit:

  number of iterations of the Gibbs sampler.

- donors:

  number of donors to sample from in predictive mean matching.

- ridge:

  ridge penalty used in the linear regression draws, to stabilize the
  estimation in case of collinear predictors.

## Value

list of `m` completed data.frames.

## Details

Supported imputation methods:

- `pmm`: predictive mean matching (type 1 matching, as in `mice`).
  Imputed values are always values that are observed in the data.

- `norm`: Bayesian linear regression (Rubin, 1987, p. 167).

- `logreg`: Bayesian logistic regression, for binary factors. Uses data
  augmentation (White, Daniel & Royston, 2010) to deal with perfect
  prediction.

- `polyreg`: polytomous regression for unordered factors with more than
  two levels, approximated by separate baseline-category binary logistic
  regressions (Begg & Gray, 1984).

- `""`: variable is not imputed.

## References

van Buuren S, Groothuis-Oudshoorn K (2011). mice: Multivariate
Imputation by Chained Equations in R. Journal of Statistical Software,
45(3), 1-67.

Rubin DB (1987). Multiple Imputation for Nonresponse in Surveys. Wiley.

White IR, Daniel R, Royston P (2010). Avoiding bias due to perfect
prediction in multiple imputation of incomplete categorical variables.
Computational Statistics & Data Analysis, 54(10), 2267-2275.

Begg CB, Gray R (1984). Calculation of polychotomous logistic regression
parameters using individualized regressions. Biometrika, 71(1), 11-18.
