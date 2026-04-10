# Reformat NCA-type analysis-ready dataset (ARD) into NONMEM-style modeling dataset

Reformat NCA-type analysis-ready dataset (ARD) into NONMEM-style
modeling dataset

## Usage

``` r
reformat_data_modeling_to_nca(data, dictionary = NULL, na = NA)
```

## Arguments

- data:

  dataset formatted as modeling-ready dataset

- dictionary:

  a data dictionary that maps expected variable names to variables in
  the data.

- na:

  what to set NA values to. E.g. ".", or NA (keep NA, default), or NULL
  (do nothing).

## Value

data.frame with population PK input data in NONMEM-style format.
