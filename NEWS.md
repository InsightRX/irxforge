# irxforge (development version)

* `anonymize_dataset()`: new `design_cols` argument for study-design columns
  (e.g. `DVID`, `OCC`, L2 `PAIR` indices, carried dose history, model row filters)
  that are cloned verbatim from the matched source design instead of being dropped
  or resampled. They are present in the data handed to `ferx_simulate()`, so models
  that reference them can be simulated, and are kept in the returned dataset.
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
