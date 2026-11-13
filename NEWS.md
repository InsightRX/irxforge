# irxforge (development version)

* `sample_covariates_mice_timevarying()`: fixed a crash ("Can't combine `..1$.SEX_lag`
  <integer> and `..2$.SEX_lag` <factor>") when categorical covariates are stored as
  integer codes and at least one covariate is time-varying. Observed and simulated
  rows are now coerced to factors with a shared level set before stacking (#18).
* `anonymize_dataset()`: `covariates` may now be `character(0)` for datasets without
  covariate columns. Covariate sampling is skipped; subjects are still renumbered
  `1:n` (with each observed design cloned onto a new subject in random order) and
  `DV` is still replaced by simulated concentrations (#20).
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
