# Reformat NCA-type analysis-ready dataset (ARD) into a NONMEM-style modeling dataset

Reformat NCA-type analysis-ready dataset (ARD) into a NONMEM-style
modeling dataset

## Usage

``` r
reformat_data_nca_to_modeling(
  data,
  dictionary = list(subject_id = "ID", group = "GROUP", time = "TIME", dose = "AMT", conc
    = "DV"),
  dose_compartment = 1,
  obs_compartment = 1,
  covariates = NULL,
  repeat_doses = NULL,
  na = "."
)
```

## Arguments

- data:

  dataset formatted as NCA analysis-ready dataset

- dictionary:

  a data dictionary that maps expected variable names to variables in
  the data.

- dose_compartment:

  the compartment in which doses are entered

- obs_compartment:

  the observation compartment number

- covariates:

  a vector of covariate names that are to be extracted and added to the
  modeling dataset.

- repeat_doses:

  Optional list for repeated dosing (MAD studies). Must contain
  `interval` (dosing interval in TIME units). Optionally contains `n`
  (total number of doses). If `n` is omitted, it is inferred per
  subject/group as `ceiling(max(observation_time) / interval)`. Only
  applies to column-wise dose data. Default `NULL` preserves existing
  behavior (no ADDL/II columns). Examples: `list(interval = 12)` or
  `list(n = 5, interval = 12)`.

- na:

  what to set NA values to. E.g. ".", (default) or NA (keep NA), or NULL
  (do nothing).

## Value

data.frame with population PK input data in NONMEM-style format.
