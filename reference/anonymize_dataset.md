# Anonymize a NONMEM-style dataset

Creates an anonymized NONMEM-style dataset by renumbering subjects,
sampling new covariates, cloning observed dosing/observation designs,
and replacing observed concentrations with FeRx-simulated values.

## Usage

``` r
anonymize_dataset(
  data,
  covariates,
  model_file,
  dictionary = list(ID = "ID", EVID = "EVID", AMT = "AMT", TIME = "TIME", DV = "DV", RATE
    = "RATE"),
  method = c("mice", "lme", "copulas"),
  cat_covs = NULL,
  loq = NULL,
  blq_method = c("remove", "cens"),
  sigdig = 4,
  n_candidates = 1,
  similarity = c("energy"),
  score_on = c("concentration", "covariate"),
  seed = NULL,
  ...
)
```

## Arguments

- data:

  original NONMEM-style `data.frame` or `tibble`.

- covariates:

  character vector of covariates to include in the anonymized dataset.
  May be `character(0)` for datasets without covariate columns:
  covariate sampling is then skipped, and each observed subject's
  dosing/observation design is cloned (in random order) onto a
  renumbered subject `1:n` before the concentrations are simulated.

- model_file:

  path to a FeRx model file used to simulate concentrations. The file is
  not used verbatim: a model written to fit the original data usually
  still describes that dataset, so the simulation runs on a temporary
  copy adapted to the anonymized dataset. In that copy the
  `[data_selection]` block (whose filters refer to original records and
  columns) and the deprecated `[initial_values]` block are removed; in
  the `[data]` block, `path` is replaced by the absolute path of the
  simulation dataset, and a column map is kept only when it renames a
  sampled covariate (from `covariates`) whose new name does not collide
  with a column that remains under its own name after all retained
  renames are applied (renames are simultaneous, so `HTCM = WT` with
  `WT = HT` is kept). Maps onto or from the structural columns (e.g.
  `AMT = DOSE`, `DV = lndv`) are dropped, because the anonymized dataset
  already uses the canonical names `ID`, `TIME`, `EVID`, `AMT`, `DV` and
  `RATE` (plus `CMT`, `MDV`, `ADDL`, `II` and `SS` when present in
  `data`). A relative `[priors]` `from_fit` path is made absolute
  against the directory of `model_file`, so it keeps resolving from the
  copy. Comments (`#` and `//`) are handled as FeRx handles them.

- dictionary:

  named list mapping expected NONMEM column names to columns in `data`.
  Supported keys are `ID`, `EVID`, `AMT`, `TIME`, `DV`, and optionally
  `RATE`. Defaults use the same names.

- method:

  time-varying covariate sampler used to resample the covariates.
  `"mice"` (default) uses
  [`sample_covariates_mice_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice_timevarying.md),
  `"lme"` uses
  [`sample_covariates_lme_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_lme_timevarying.md),
  and `"copulas"` uses
  [`sample_covariates_copulas_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_copulas_timevarying.md).
  Only `"mice"` supports categorical covariates; `"lme"` and `"copulas"`
  require all covariates to be continuous.

- cat_covs:

  optional character vector of categorical covariates, in addition to
  factor and character covariates (which are always treated as
  categorical). Use this for numerically coded categorical covariates,
  e.g. `SEX = 0/1`, so they are not sampled as continuous values. They
  are returned with their original numeric codes.

- loq:

  optional numeric limit of quantification. Simulated observations below
  `loq` are handled according to `blq_method`.

- blq_method:

  how to handle simulated observations below `loq`. `"remove"` (default)
  removes the below-LOQ observation rows. `"cens"` keeps them, sets
  their `DV` to `0`, and adds a `CENS` column with `1` for below-LOQ
  observations and `0` otherwise. If `data` contains an `MDV` column, it
  is recomputed from the simulated data: `1` for non-observation records
  and observations with a missing `DV`, `0` otherwise (censored
  observations keep `MDV = 0` and are flagged via `CENS`).

- sigdig:

  number of significant digits to round the sampled covariates and
  simulated concentrations (`DV`) to, via
  [`signif()`](https://rdrr.io/r/base/Round.html), to further obscure
  the values. Default `4`. `NULL` disables rounding. Rounding is applied
  before the `loq` comparison.

- n_candidates:

  number of independent candidates to draw and score against the
  observed data, keeping the closest match. Default `1` (no scoring; a
  single candidate is used). When greater than 1, the candidate with the
  smallest distance to the observed data (see `score_on`) is selected.
  The chosen candidate's score is attached to the result as the
  `"similarity_score"` attribute.

- similarity:

  distribution-similarity metric used to score candidates. `"energy"`
  (default) is the energy distance between the observed and simulated
  feature matrices, standardized by the observed spread. Lower is
  closer. The score is population-level, not per-subject, so it does not
  push individual simulated subjects toward specific real subjects.

- score_on:

  what to score candidates on when `n_candidates > 1`. `"concentration"`
  (default) scores the simulated concentration-time data: each candidate
  is fully simulated and its `(time, log concentration)` observation
  cloud is compared to the observed concentrations, so the PK simulation
  is run once per candidate. `"covariate"` scores only the sampled
  covariate trajectories (baseline, mean, slope and within-subject SD
  per continuous covariate; per-subject time fraction in each level per
  categorical covariate), which is cheaper because the PK simulation is
  then run only once, on the selected covariate sample.

- seed:

  optional integer seed used for covariate sampling and FeRx simulation.
  `NULL` (default) does not seed, so each call draws independently.

- ...:

  additional arguments passed to
  [`sample_covariates_mice_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice_timevarying.md).

## Value

anonymized NONMEM-style data.frame, with one block of records per
anonymized subject (`ID` `1:n`). Each block is a clone of one observed
subject's dosing/observation design, returned **ordered by time within
subject** (a stable sort on `TIME`, so records sharing a time keep their
source order and records with a missing time are placed last). The
result carries these attributes:

- `"design_map"`: a `data.frame` with one row per anonymized subject and
  columns `ID` (the anonymized subject ID) and `DESIGN_ID` (the ID of
  the observed subject in `data` whose design was cloned, in its
  original type). Use it to relate anonymized subjects back to their
  source design without reconstructing it from the event records. Note
  that this mapping links the anonymized dataset to the source subjects,
  so it should not be shared alongside the anonymized data.

- `"similarity_score"`: the selected candidate's score when
  `n_candidates > 1`, otherwise `NULL`.
