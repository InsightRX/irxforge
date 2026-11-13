# Changelog

## irxforge (development version)

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
