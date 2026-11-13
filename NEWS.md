# irxforge (development version)

* `anonymize_dataset()`: arguments passed through `...` are now validated up
  front against the time-varying sampler selected by `method` (and, for `"mice"`
  and `"lme"`, the internal MICE implementation). Unknown names (typos, or
  arguments added in a newer irxforge release) fail with "unused argument `x` ...
  not supported by this build of irxforge (<version>)", and arguments that
  `anonymize_dataset()` sets itself (e.g. `id_var`, `n_subjects`) are rejected,
  instead of surfacing deep in the call stack as "formal argument ... matched by
  multiple actual arguments" or being silently ignored (#29).
* `anonymize_dataset()`: `model_file` is no longer passed to FeRx verbatim. The
  simulation runs on a temporary copy adapted to the anonymized dataset: the
  `[data_selection]` and deprecated `[initial_values]` blocks are removed, `[data] path`
  is rewritten to the absolute path of the simulation dataset, and `[data]` column maps
  are kept only when they rename a surviving covariate (maps onto or from structural
  columns, e.g. `AMT = DOSE` or `DV = lndv`, are dropped because the anonymized data
  already uses the canonical names). Simultaneous renames such as `HTCM = WT` with
  `WT = HT` are preserved, a relative `[priors] from_fit` path is made absolute against
  the original model's directory, and `#` and `//` comments are handled as FeRx
  handles them (#23).
* `anonymize_dataset()`: covariates are now checked up front for observed values,
  before any sampling or design matching. Every covariate needs at least one
  non-missing value overall and at least one on the baseline rows (each subject's
  first record by time), which the samplers impute baselines from. All offending
  covariates are reported at once, and the error says "no observed values at
  baseline" when a covariate is observed only after baseline, instead of failing
  deep inside `mice_impute()` with "Variable `HT` has no observed values" (#25).
* `anonymize_dataset()`: `EVID == 0` records flagged `MDV == 1` (e.g. scheduled
  samples that were never taken) no longer cause "FeRx simulation row count does not
  match the number of observation records". Observations are now counted the way
  FeRx counts them (`EVID == 0 & (is.na(MDV) | MDV == 0)`); those records keep a
  missing `DV` and `MDV = 1` in the output. If the counts still disagree, the error
  reports how many rows FeRx returned, how many `EVID == 0` records there are, and how
  many of them are flagged `MDV == 1` (#24).
* `sample_covariates_mice_timevarying()`: fixed a crash ("Can't combine `..1$.SEX_lag`
  <integer> and `..2$.SEX_lag` <factor>") when categorical covariates are stored as
  integer codes and at least one covariate is time-varying. Observed and simulated
  rows are now coerced to factors with a shared level set before stacking (#18).
* `anonymize_dataset()`: the guard against time-reset records now tests what the
  time-varying samplers actually need — a non-decreasing `TIME` within every subject —
  instead of rejecting any dataset containing `EVID` 3 or 4. Reset records whose time
  scale keeps increasing are now accepted, and when the check fails the error names the
  offending subjects (#21).
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
