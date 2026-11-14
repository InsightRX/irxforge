# Changelog

## irxforge (development version)

- [`anonymize_dataset()`](https://insightrx.github.io/irxforge/reference/anonymize_dataset.md):
  a covariate that the model references under a spelling that differs
  only by case from its data column (e.g. `CONC0` for a `conc0` column)
  is now bridged with an added `[data]` rename (`CONC0 = conc0`) in the
  simulation copy of `model_file`, since FeRx matches covariate names
  case-sensitively. The model’s covariate references are checked before
  any covariate is sampled: a reference that matches several columns
  case-insensitively fails with an error naming the columns, and a
  reference to a column of `data` in neither `covariates` nor
  `design_cols` raises a warning, instead of the simulation failing
  partway through with `E_MISSING_COVARIATE`. FeRx grammar words
  (e.g. the `pk` keyword) and ODE state names are not treated as
  covariate references, and a column the model references under its
  exact spelling is never renamed (#43).

- [`anonymize_dataset()`](https://insightrx.github.io/irxforge/reference/anonymize_dataset.md):
  new `design_cols` argument for study-design columns (e.g. `DVID`,
  `OCC`, L2 `PAIR` indices, carried dose history, model row filters)
  that are cloned verbatim from the matched source design instead of
  being dropped or resampled. They are present in the data handed to
  `ferx_simulate()`, so models that reference them can be simulated, and
  are kept in the returned dataset.

- [`sample_covariates_mice_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice_timevarying.md)
  no longer scales with `max profile length x nrow(data)`. Each
  transition step now draws donors only from observed transitions at
  comparable time indices (positions within the subject’s profile),
  widened to neighbouring indices until at least `min_donors` (default
  `1000`) are available; donor pools are built once per time index and
  reused across replicates. Datasets with at most `min_donors`
  transitions behave as before, and `min_donors = Inf` restores the full
  donor frame. A new `progress` argument (default
  [`interactive()`](https://rdrr.io/r/base/interactive.html)) shows a
  progress bar over the transition steps (#26).

- [`anonymize_dataset()`](https://insightrx.github.io/irxforge/reference/anonymize_dataset.md):
  arguments passed through `...` are now validated up front against the
  time-varying sampler selected by `method` (and, for `"mice"` and
  `"lme"`, the internal MICE implementation). Unknown names (typos, or
  arguments added in a newer irxforge release) fail with “unused
  argument `x` … not supported by this build of irxforge ()”, and
  arguments that
  [`anonymize_dataset()`](https://insightrx.github.io/irxforge/reference/anonymize_dataset.md)
  sets itself (e.g. `id_var`, `n_subjects`) are rejected, instead of
  surfacing deep in the call stack as “formal argument … matched by
  multiple actual arguments” or being silently ignored (#29).

- [`anonymize_dataset()`](https://insightrx.github.io/irxforge/reference/anonymize_dataset.md):
  `model_file` is no longer passed to FeRx verbatim. The simulation runs
  on a temporary copy adapted to the anonymized dataset: the
  `[data_selection]` and deprecated `[initial_values]` blocks are
  removed, `[data] path` is rewritten to the absolute path of the
  simulation dataset, and `[data]` column maps are kept only when they
  rename a surviving covariate (maps onto or from structural columns,
  e.g. `AMT = DOSE` or `DV = lndv`, are dropped because the anonymized
  data already uses the canonical names). Simultaneous renames such as
  `HTCM = WT` with `WT = HT` are preserved, a relative
  `[priors] from_fit` path is made absolute against the original model’s
  directory, and `#` and `//` comments are handled as FeRx handles them
  (#23).

- [`anonymize_dataset()`](https://insightrx.github.io/irxforge/reference/anonymize_dataset.md):
  covariates are now checked up front for observed values, before any
  sampling or design matching. Every covariate needs at least one
  non-missing value overall and at least one on the baseline rows (each
  subject’s first record by time), which the samplers impute baselines
  from. All offending covariates are reported at once, and the error
  says “no observed values at baseline” when a covariate is observed
  only after baseline, instead of failing deep inside
  [`mice_impute()`](https://insightrx.github.io/irxforge/reference/mice_impute.md)
  with “Variable `HT` has no observed values” (#25).

- [`anonymize_dataset()`](https://insightrx.github.io/irxforge/reference/anonymize_dataset.md):
  `EVID == 0` records flagged `MDV == 1` (e.g. scheduled samples that
  were never taken) no longer cause “FeRx simulation row count does not
  match the number of observation records”. Observations are now counted
  the way FeRx counts them (`EVID == 0 & (is.na(MDV) | MDV == 0)`);
  those records keep a missing `DV` and `MDV = 1` in the output. If the
  counts still disagree, the error reports how many rows FeRx returned,
  how many `EVID == 0` records there are, and how many of them are
  flagged `MDV == 1` (#24).

- [`sample_covariates_mice_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice_timevarying.md):
  fixed a crash (“Can’t combine `..1$.SEX_lag` and `..2$.SEX_lag` ”)
  when categorical covariates are stored as integer codes and at least
  one covariate is time-varying. Observed and simulated rows are now
  coerced to factors with a shared level set before stacking (#18).

- [`anonymize_dataset()`](https://insightrx.github.io/irxforge/reference/anonymize_dataset.md):
  the guard against time-reset records now tests what the time-varying
  samplers actually need — a non-decreasing `TIME` within every subject
  — instead of rejecting any dataset containing `EVID` 3 or 4. Reset
  records whose time scale keeps increasing are now accepted, and when
  the check fails the error names the offending subjects (#21).

- [`anonymize_dataset()`](https://insightrx.github.io/irxforge/reference/anonymize_dataset.md):
  `covariates` may now be `character(0)` for datasets without covariate
  columns. Covariate sampling is skipped; subjects are still renumbered
  `1:n` (with each observed design cloned onto a new subject in random
  order) and `DV` is still replaced by simulated concentrations (#20).

- [`check_nm_dataset()`](https://insightrx.github.io/irxforge/reference/check_nm_dataset.md):
  fixed a crash when `TIME` is non-numeric (e.g. date/clock strings) and
  `ADDL`/`II` are present — the overlapping-dose check now also guards
  on `is.numeric(TIME)` so the whole check no longer aborts before
  returning results.

- [`check_nm_dataset()`](https://insightrx.github.io/irxforge/reference/check_nm_dataset.md):
  the `unique_records` check now skips (returns `NA`) instead of
  emitting a false `FAIL` when the subject `ID` does not resolve or
  `TIME` is not numeric (a character clock-of-day TIME, or a key
  collapsed across subjects, produced spurious duplicates).

- [`check_nm_dataset()`](https://insightrx.github.io/irxforge/reference/check_nm_dataset.md):
  failing `char_columns`, `reserved_names`, and `covariate_missing`
  checks now name the offending column(s) in their `description`.

- Initial CRAN submission.
