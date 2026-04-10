# Reformat SDTM datasets into NONMEM-style modeling dataset

Reformat SDTM datasets into NONMEM-style modeling dataset

## Usage

``` r
reformat_data_sdtm_to_modeling(data, dictionary, na = ".")
```

## Arguments

- data:

  list containing data.frames with SDTM domains

- dictionary:

  a data dictionary that maps expected variable names to variables in
  the data.

- na:

  what to set NA values to. E.g. ".", (default) or NA (keep NA), or NULL
  (do nothing).

## Value

data.frame with population PK input data in NONMEM-style format. It will
also add the non-standard columns ROUTE ("oral", "iv") and FORM
(formulation: "tablet", "suspension", "patch", "infusion", etc.) with
values for each dose and NA for observations.
