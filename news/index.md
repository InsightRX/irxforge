# Changelog

## irxforge (development version)

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
