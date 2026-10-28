# Sample covariates using multivariate imputation using chained equations (mice)

In contrast to sampling methods bootstrap and NHANES, categorical
covariates need to be specified using the `cat_covs` argument, otherwise
they will be treated as continuous.

## Usage

``` r
sample_covariates_mice(
  data,
  cat_covs = NULL,
  conditional = NULL,
  n_subjects = nrow(data),
  cont_method = c("pmm", "norm"),
  replicates = 1,
  seed = NULL,
  ...
)
```

## Arguments

- data:

  data.frame (n x p) containing the original, observed, time-invariant
  covariates (ID should not be included) that will be used to inform the
  imputation.

- cat_covs:

  character vector containing the names of the categorical covariates in
  orgCovs.

- conditional:

  list with conditional limits for sampled population. For continuous
  covariates, specify a numeric vector of length 2 giving the
  `c(min, max)` range, e.g. `list("WT" = c(40, 60), "BMI" = c(15, 25))`.
  For categorical covariates (those listed in `cat_covs`), specify a
  character vector of the allowed category values, e.g.
  `list("SEX" = c("F"))`.

- n_subjects:

  number of simulated subjects, default is the number of subjects in the
  data.

- cont_method:

  method used to impute continuous covariates, either `pmm` (predictive
  mean matching, default) or `norm` (Bayesian linear regression).

- replicates:

  number of multiple imputations replicates to sample. Default is 1.

- seed:

  integer random seed passed to
  [`set.seed()`](https://rdrr.io/r/base/Random.html) for
  reproducibility. Default `NULL` does not set a seed.

- ...:

  additional arguments passed to the internal MICE implementation, e.g.
  `maxit` (number of iterations, default 5) or `donors` (number of
  donors for predictive mean matching, default 5).

## Value

data.frame with the simulated covariates, with n_subjects \* m rows and
p columns

## Details

missing values in `data` must be coded as NA.

The imputation uses a minimal, internal re-implementation of the subset
of the `mice` package that is needed here (predictive mean matching or
Bayesian linear regression for continuous covariates, logistic
regression for binary and polytomous regression for other categorical
covariates). This avoids a dependency on `mice` and its dependency tree.
