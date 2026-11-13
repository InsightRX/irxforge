# Sample time-varying covariates using copula modeling

Implements the copula-based virtual-patient simulation of Zwep et al.
(2024) for time-varying covariates. Each continuous time-varying
covariate trajectory is summarised by the per-subject coefficients of a
random-effects polynomial regression on time; a vine copula is then
fitted to the joint distribution of those coefficients together with the
(continuous) static covariates. New virtual subjects are drawn from the
copula and their trajectories are reconstructed by evaluating the
sampled polynomial coefficients over a time grid. Modelling dependence
in coefficient space, with nonparametric (kernel) marginals, reproduces
the realistic covariate domain and the full correlation structure
without resampling or extrapolating individual observed values.

## Usage

``` r
sample_covariates_copulas_timevarying(
  data,
  id_var = "ID",
  time_var = "TIME",
  time_grid = NULL,
  static_covs = NULL,
  time_varying_covs = NULL,
  degree = 1,
  family_set = "all",
  selcrit = "aic",
  bw_mult = 1,
  noise = NULL,
  truncate = TRUE,
  design_id_var = NULL,
  n_subjects = length(unique(data[[id_var]])),
  replicates = 1,
  seed = NULL,
  ...
)
```

## Arguments

- data:

  long-format `data.frame` or `tibble` containing one row per subject
  and time.

- id_var:

  subject identifier column name.

- time_var:

  time column name. Must be numeric.

- time_grid:

  optional numeric vector of simulated times. If supplied, every
  simulated subject's trajectory is reconstructed on this grid. If
  `NULL` (default), each simulated subject clones an observed subject's
  observation-time vector, assigned deterministically in observed
  subject order and recycled if `n_subjects` exceeds the number of
  observed subjects.

- static_covs:

  character vector of continuous covariates that are constant within a
  subject. Sampled jointly with the trajectory coefficients and carried
  forward unchanged.

- time_varying_covs:

  character vector of continuous covariates to model over time. Default
  is all columns except `id_var`, `time_var`, and `static_covs`.

- degree:

  degree of the per-subject polynomial in `time_var` used to summarise
  each time-varying covariate trajectory. Default `1` (a
  subject-specific intercept and slope). Higher degrees capture
  curvature but require more timepoints per subject.

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
  observed (empirical) distribution; this is the main lever for matching
  the original covariate distributions most closely. Very small values
  risk overfitting the observed sample.

- noise:

  optional single non-negative number giving the log-scale SD of
  multiplicative log-normal noise (e.g. `0.05` for ~5%) applied to the
  reconstructed output values to obscure the generated data. `NULL`
  (default) applies no noise. Time-varying covariates are jittered
  independently at each timepoint (adding measurement-like scatter
  around the trajectory); static covariates are jittered once per
  subject so they stay constant over time. Applied before `truncate`.

- truncate:

  logical; if `TRUE` (default) reconstructed time-varying covariate
  values are clamped to the observed `[min, max]` range of each
  covariate, preventing nonphysical extrapolated values.

- design_id_var:

  optional column name for retaining the observed subject ID whose
  observation-time design was cloned for each simulated subject. Only
  usable when `time_grid = NULL`. Default `NULL` omits this column.

- n_subjects:

  number of simulated subjects. Default is the number of unique observed
  subjects.

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

long-format data.frame with simulated subject IDs, time, static
covariates, and time-varying covariates.

## Details

The algorithm follows Zwep et al. (2024):

1.  For each time-varying covariate `X_j`, fit a random-effects
    polynomial regression
    `X_j ~ poly(time, degree) + (poly(time, degree) | id)` and extract
    the per-subject coefficient vector. If the full random-slope model
    fails to converge, a random-intercept model is used and the
    population-level slope coefficients are shared across subjects.

2.  Assemble a subject-level table of all trajectory coefficients and
    the static covariates, and fit a vine copula with kernel-density
    marginals via
    [`rvinecopulib::vine()`](https://vinecopulib.github.io/rvinecopulib/reference/vine.html).

3.  Draw `n_subjects` coefficient vectors from the copula and
    reconstruct each trajectory by evaluating its polynomial over the
    requested times.

Only continuous covariates are supported; categorical covariates should
be handled with
[`sample_covariates_mice_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice_timevarying.md).

Requires the `rvinecopulib` package.

## References

Zwep LB, Guo T, Nagler T, Knibbe CAJ, Meulman JJ, van Hasselt JGC.
Virtual Patient Simulation Using Copula Modeling. Clin Pharmacol Ther.
2024;115(4):795-804.
[doi:10.1002/cpt.3099](https://doi.org/10.1002/cpt.3099)

## See also

[`sample_covariates_mice_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice_timevarying.md)
and
[`sample_covariates_lme_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_lme_timevarying.md)
for the resampling and mixed-effects alternatives.
