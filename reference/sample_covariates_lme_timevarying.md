# Sample time-varying covariates using linear mixed-effects transition models

Samples baseline covariates with
[`sample_covariates_mice()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice.md),
then samples later timepoints sequentially from per-covariate linear
mixed-effects models fitted to the observed first-order transitions.
Each continuous time-varying covariate is modelled as a function of the
previous values of all time-varying covariates (capturing both
autocorrelation and between-covariate relationships), the elapsed time
since the previous measurement, the static covariates and, optionally,
absolute time. A subject-level random intercept captures within-subject
correlation; new random effects are drawn for each simulated subject.

## Usage

``` r
sample_covariates_lme_timevarying(
  data,
  id_var = "ID",
  time_var = "TIME",
  time_grid = NULL,
  static_covs = NULL,
  time_varying_covs = NULL,
  cat_covs = NULL,
  trend = c("previous", "time"),
  random_intercept = TRUE,
  measurement_pattern = c("change", "nonmissing", "all"),
  design_match = c("clone", "propensity"),
  design_match_covs = NULL,
  design_id_var = NULL,
  n_subjects = length(unique(data[[id_var]])),
  baseline_method = c("mice", "bootstrap"),
  noise = NULL,
  conditional = NULL,
  cont_method = "pmm",
  replicates = 1,
  seed = NULL,
  ...,
  max_cat_levels = 20
)
```

## Arguments

- data:

  long-format `data.frame` or `tibble` containing one row per subject
  and time.

- id_var:

  subject identifier column name.

- time_var:

  time column name.

- time_grid:

  optional numeric vector of simulated times. If supplied, every
  simulated subject uses this same time grid. If `NULL` (default), each
  simulated subject clones an observed subject's covariate
  observation-time design. Designs are assigned deterministically in
  observed subject order and recycled if `n_subjects` exceeds the number
  of observed subjects.

- static_covs:

  character vector of covariates that should be sampled at baseline and
  then carried forward unchanged. Categorical covariates are only
  supported as static covariates (see `cat_covs`).

- time_varying_covs:

  character vector of covariates to sample at each timepoint. Default is
  all columns except `id_var`, `time_var`, and `static_covs`.
  Time-varying covariates must be continuous.

- cat_covs:

  character vector containing categorical covariates. These may only
  appear among `static_covs`; categorical time-varying covariates are
  not supported by the mixed-effects engine.

- trend:

  predictors used to model each transition. `"previous"` (default)
  regresses each covariate on the previous covariate values and the
  elapsed time only (a Markov / autoregressive transition). `"time"`
  additionally includes absolute `time_var` as a fixed effect, capturing
  a population time trajectory on top of the dependence on the previous
  value.

- random_intercept:

  logical, whether to include a subject-level random intercept (default
  `TRUE`). When `FALSE`, or when the mixed-effects fit fails to
  converge, the transition is modelled with ordinary least squares.

- measurement_pattern:

  how to determine when a time-varying covariate receives a new
  measurement in the sampled observed profile. `"change"` (default)
  treats a covariate as newly measured when its value changes from the
  previous row, which is useful for datasets where covariates have been
  carried forward. `"nonmissing"` treats non-missing values as
  measurements. `"all"` updates every time-varying covariate at every
  sampled row.

- design_match:

  method used to assign observed covariate observation-time designs to
  simulated subjects when `time_grid = NULL`. `"clone"` (default)
  assigns designs deterministically in observed subject order.
  `"propensity"` fits a logistic propensity model comparing observed
  baseline covariates to sampled baseline covariates, then assigns each
  simulated subject the design from the unmatched observed subject with
  the nearest propensity score.

- design_match_covs:

  character vector of baseline covariates to use for propensity-score
  design matching. Default is all static and time-varying covariates.

- design_id_var:

  optional column name for retaining the observed subject ID whose
  observation-time design was assigned to each simulated subject. Only
  usable when `time_grid = NULL`. Default `NULL` omits this column.

- n_subjects:

  number of simulated subjects. Default is the number of unique observed
  subjects.

- baseline_method:

  how baseline covariates are sampled. `"mice"` (default) generates
  baselines with
  [`sample_covariates_mice()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice.md).
  `"bootstrap"` resamples observed baseline rows with replacement via
  [`sample_covariates_bootstrap()`](https://insightrx.github.io/irxforge/reference/sample_covariates_bootstrap.md),
  which reproduces the observed joint baseline distribution instead of
  shrinking it toward the multivariate mean as full-row MICE imputation
  tends to, especially in small datasets.

- noise:

  optional single non-negative number giving the log-scale SD of
  multiplicative log-normal noise (e.g. `0.05` for ~5%) applied to the
  simulated output values to obscure them. `NULL` (default) applies no
  noise. Time-varying covariates are jittered independently at each
  timepoint; static covariates are jittered once per subject so they
  stay constant over time.

- conditional:

  list with conditional limits applied to the **baseline** covariate
  sample. Sequential transitions are not constrained. See
  [`sample_covariates_mice()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice.md)
  for the accepted format.

- cont_method:

  method used to impute continuous covariates for the baseline sample,
  either `pmm` (predictive mean matching, default) or `norm` (Bayesian
  linear regression).

- replicates:

  number of independent simulated datasets to sample. Default is 1. When
  greater than 1, a `.replicate` column is included.

- seed:

  integer random seed passed to
  [`set.seed()`](https://rdrr.io/r/base/Random.html) for
  reproducibility. Default `NULL` does not set a seed.

- ...:

  additional arguments passed to
  [`sample_covariates_mice()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice.md)
  for the baseline sample.

- max_cat_levels:

  maximum number of distinct values allowed for a categorical covariate
  (listed in `cat_covs`, or a factor/character column). Default `20`.
  Covariates with more levels are rejected up front with an error that
  reports the level count of each categorical covariate, because they
  are almost always continuous columns passed as categorical by mistake
  and make the polytomous imputation extremely slow. Use `Inf` to
  disable.

## Value

long-format data.frame with simulated subject IDs, time, static
covariates, and time-varying covariates.

## Details

This function implements a first-order linear mixed-effects transition
sampler. For each continuous time-varying covariate `X_j` it fits
`X_j(t) ~ X_1(t-1) + ... + X_p(t-1) + delta_time [+ time] + static + (1 | id)`
to the observed transitions, then generates simulated trajectories by
drawing a fresh random intercept per simulated subject and adding
residual noise at each timepoint.

In contrast to
[`sample_covariates_mice_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice_timevarying.md),
which resamples observed values via chained equations, this sampler
interpolates and extrapolates through the fitted linear models and so
can produce covariate values not present in the observed data. It is
intended as a lightweight parametric alternative when smooth,
model-based trajectories are preferred.

Missing values in `data` must be coded as `NA`. Transitions with missing
predictors or response are dropped when fitting the models.

## See also

[`sample_covariates_mice_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice_timevarying.md)
for the chained-equations resampling alternative.
