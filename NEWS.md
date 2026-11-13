# irxforge (development version)

* `sample_covariates_mice_timevarying()`: fixed a crash ("Can't combine `..1$.SEX_lag`
  <integer> and `..2$.SEX_lag` <factor>") when categorical covariates are stored as
  integer codes and at least one covariate is time-varying. Observed and simulated
  rows are now coerced to factors with a shared level set before stacking (#18).
* `check_nm_dataset()`: fixed a crash when `TIME` is non-numeric (e.g. date/clock
  strings) and `ADDL`/`II` are present — the overlapping-dose check now also guards
  on `is.numeric(TIME)` so the whole check no longer aborts before returning results.
* `check_nm_dataset()`: the `unique_records` check now skips (returns `NA`) instead of
  emitting a false `FAIL` when the subject `ID` does not resolve or `TIME` is not
  numeric (a character clock-of-day TIME, or a key collapsed across subjects, produced
  spurious duplicates).
* `check_nm_dataset()`: failing `char_columns`, `reserved_names`, and
  `covariate_missing` checks now name the offending column(s) in their `description`.

* Initial CRAN submission.
