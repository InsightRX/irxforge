# Download and cache NHANES data locally

Downloads all tables within the specified NHANES data groups for one or
more survey years, merges them into a single data frame per year, and
saves it as an RDS file. The resulting cache is used automatically by
[`sample_covariates_nhanes()`](https://insightrx.github.io/irxforge/reference/sample_covariates_nhanes.md).

## Usage

``` r
download_nhanes_cache(
  groups = c("DEMO", "LAB", "EXAM"),
  years = "2017-2018",
  path = nhanes_default_cache_dir(),
  overwrite = FALSE,
  ...
)
```

## Arguments

- groups:

  character vector of NHANES data groups to download. Valid values:
  `"DEMO"` (Demographics), `"LAB"` (Laboratory), `"EXAM"` (Examination),
  `"Q"` (Questionnaire), `"DIET"` (Dietary). Defaults to
  `c("DEMO", "LAB", "EXAM")`.

- years:

  character vector of NHANES survey cycles, e.g.
  `c("2015-2016", "2017-2018")`. Defaults to `"2017-2018"`.

- path:

  directory where merged RDS files will be saved. Created automatically
  if it does not exist. Defaults to the package-level cache directory
  returned by `nhanes_default_cache_dir()`.

- overwrite:

  logical. If `FALSE` (default), a year that already has a merged RDS in
  `path` is skipped. Set to `TRUE` to re-download and overwrite.

- ...:

  additional arguments (currently unused)

## Value

the `path` directory, invisibly.

## Details

Each survey year is saved as a single file `nhanes_<year>.rds` (e.g.
`nhanes_2017-2018.rds`) containing all variables from all downloaded
tables, merged on the SEQN respondent sequence number. Tables with
multiple rows per subject (e.g. dietary recall) are skipped
automatically.

Requires the `nhanesA` package.
