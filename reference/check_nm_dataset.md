# Run automated quality checks on a NONMEM dataset

Performs a comprehensive set of checks on a NONMEM-style dataset and
returns a summary table with the result of each check. No errors are
thrown; all issues are reported as rows in the output table.

## Usage

``` r
check_nm_dataset(
  data,
  dictionary = NULL,
  exclusion_flag = NULL,
  max_digits = 10,
  max_id_digits = 5,
  extra_reserved = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  a data.frame (or tibble) containing the NONMEM dataset to check.

- dictionary:

  an optional named list that maps standard NONMEM column names to the
  actual column names in `data`. For example,
  `list(ID = "NMID", TIME = "TAD")` means the column called `"NMID"` in
  `data` plays the role of `ID`, and `"TAD"` plays the role of `TIME`.
  Any standard name not listed in the dictionary is assumed to match the
  column name in `data` as-is. Supported keys: `ID`, `TIME`, `DV`,
  `MDV`, `EVID`, `AMT`, `CMT`, `RATE`, `SS`, `ADDL`, `II`.

- exclusion_flag:

  character name of the column used as an exclusion flag (for
  `$DATA ACCEPT` / `IGNORE` in NONMEM). If `NULL` (default), the
  exclusion-flag check is skipped.

- max_digits:

  integer; threshold for flagging numbers with too many significant
  digits (default `10`).

- max_id_digits:

  integer; preferred maximum number of characters for the ID variable
  (default `5`).

- extra_reserved:

  character vector of additional reserved NONMEM names to check for
  (e.g. PK parameters used in ADVAN/TRANS such as `"CL"`, `"V"`, `"V1"`,
  `"K12"`).

- verbose:

  logical; if `TRUE` (default), messages are printed for every check as
  it is evaluated.

## Value

A data.frame with columns `check` (short test name), `description`
(human-readable explanation), and `result` (`"PASS"`, `"FAIL"`, or
`"NA"` when a check is not applicable).
