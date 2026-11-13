# Sample covariates using copula modeling

Implements the copula-based virtual-patient covariate simulation of Zwep
et al. (2024). A vine copula with kernel-density marginals is fitted to
the observed covariates and new virtual subjects are drawn from it,
reproducing both the individual covariate distributions and their full
dependence structure. This is the cross-sectional (time-invariant)
counterpart of
[`sample_covariates_copulas_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_copulas_timevarying.md).

## Usage

``` r
sample_covariates_copulas(
  data,
  conditional = NULL,
  n_subjects = nrow(data),
  family_set = "all",
  selcrit = "aic",
  bw_mult = 1,
  truncate = TRUE,
  replicates = 1,
  seed = NULL,
  ...
)
```

## Arguments

- data:

  data.frame or tibble (n x p) of observed, time-invariant covariates,
  one row per subject (an ID column should not be included). Only
  continuous covariates are supported.

- conditional:

  optional named list of `c(min, max)` ranges restricting the sampled
  population, e.g. `list(AGE = c(60, 80), WT = c(70, 100))`. The
  observed data are filtered to these ranges before the copula is
  fitted.

- n_subjects:

  number of simulated subjects. Default is the number of rows in `data`.
  Unlike resampling methods, the copula is generative, so `n_subjects`
  may exceed the number of observed subjects.

- family_set:

  copula families considered by
  [`rvinecopulib::vine()`](https://vinecopulib.github.io/rvinecopulib/reference/vine.html).
  Default `"all"`.

- selcrit:

  model-selection criterion for the vine copula, default `"aic"`. Use
  `"loglik"` for the tightest in-sample reproduction.

- bw_mult:

  bandwidth multiplier for the kernel-density marginals passed to
  [`rvinecopulib::vine()`](https://vinecopulib.github.io/rvinecopulib/reference/vine.html)
  (`margins_controls$mult`). Default `1`. Values below `1` reduce
  marginal smoothing, tightening the simulated marginals toward the
  observed distribution.

- truncate:

  logical; if `TRUE` (default) sampled covariate values are clamped to
  the observed `[min, max]` range of each covariate, preventing
  nonphysical extrapolated values.

- replicates:

  number of independent simulated datasets to sample. Default is 1. When
  greater than 1, a `.replicate` column is included.

- seed:

  integer random seed passed to
  [`set.seed()`](https://rdrr.io/r/base/Random.html) for
  reproducibility. Default `NULL` does not set a seed.

- ...:

  reserved for future use.

## Value

data.frame with the simulated covariates, with `n_subjects` rows (times
`replicates`) and `p` columns.

## Details

Only continuous covariates are supported; categorical covariates should
be handled with
[`sample_covariates_mice()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice.md).
Rows with missing values are dropped before fitting. Covariates that are
constant across the (filtered) data are held fixed and re-attached after
sampling.

Requires the `rvinecopulib` package.

## References

Zwep LB, Guo T, Nagler T, Knibbe CAJ, Meulman JJ, van Hasselt JGC.
Virtual Patient Simulation Using Copula Modeling. Clin Pharmacol Ther.
2024;115(4):795-804.
[doi:10.1002/cpt.3099](https://doi.org/10.1002/cpt.3099)

## See also

[`sample_covariates_copulas_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_copulas_timevarying.md)
for the time-varying variant.
