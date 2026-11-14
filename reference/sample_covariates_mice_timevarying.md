# Sample time-varying covariates using sequential MICE

Samples baseline covariates with
[`sample_covariates_mice()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice.md),
then samples later timepoints sequentially from a transition model. Each
transition is imputed conditional on the previous simulated covariate
values, time, elapsed time, and any static covariates.

## Usage

``` r
sample_covariates_mice_timevarying(
  data,
  id_var = "ID",
  time_var = "TIME",
  time_grid = NULL,
  static_covs = NULL,
  time_varying_covs = NULL,
  cat_covs = NULL,
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
  min_donors = 1000,
  progress = interactive()
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
  then carried forward unchanged.

- time_varying_covs:

  character vector of covariates to sample at each timepoint. Default is
  all columns except `id_var`, `time_var`, and `static_covs`.

- cat_covs:

  character vector containing categorical covariates among `static_covs`
  and `time_varying_covs`. A warning is raised for numeric covariates
  that look like categorical codes (whole numbers with at most 3
  distinct values, e.g. `SEX = 0/1`) but are not listed here.

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
  observation-time design was assigned to each simulated subject.
  Default `NULL` omits this bookkeeping column.

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

  method used to impute continuous covariates, either `pmm` (predictive
  mean matching, default) or `norm` (Bayesian linear regression).

- replicates:

  number of independent simulated datasets to sample. Default is 1. When
  greater than 1, a `.replicate` column is included.

- seed:

  integer random seed passed to
  [`set.seed()`](https://rdrr.io/r/base/Random.html) for
  reproducibility. Default `NULL` does not set a seed.

- ...:

  additional arguments passed to the internal MICE implementation (e.g.
  `maxit`).

- min_donors:

  minimum number of observed transitions used as donors when imputing
  each time index. Transitions are pooled by their time index (the
  position of the row within its subject's profile): at time index `k`
  the donor pool holds the transitions at index `k`, widened
  symmetrically to neighbouring indices until it contains at least
  `min_donors` rows. This keeps the cost of each step proportional to
  the number of subjects still active at that index rather than to
  `nrow(data)`, and restricts donors to transitions at comparable
  positions in the profile. Default `1000`. When `data` has at most
  `min_donors` transitions, every step uses all of them. Use `Inf` to
  always use all observed transitions.

- progress:

  logical; show a progress bar over the sequential time indices. Default
  is [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

long-format data.frame with simulated subject IDs, time, static
covariates, and time-varying covariates.

## Details

This function implements a first-order empirical transition sampler:
`X(t) ~ X(t - 1), time, delta_time, static covariates`. It is intended
for simulation from observed longitudinal covariate data, not for
replacing a mechanistic or mixed-effects longitudinal model.

Missing values in `data` must be coded as `NA`.
