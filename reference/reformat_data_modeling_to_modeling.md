# Reformat modeling dataset into a properly checked and validated modeling dataset.

Reformat modeling dataset into a properly checked and validated modeling
dataset.

## Usage

``` r
reformat_data_modeling_to_modeling(data, dictionary = NULL, na = ".")
```

## Arguments

- data:

  dataset formatted as modeling-ready dataset

- dictionary:

  a data dictionary that maps expected variable names to variables in
  the data.

- na:

  what to set NA values to. E.g. ".", (default) or NA (keep NA), or NULL
  (do nothing).

## Value

data.frame with population PK input data in NONMEM-style format.
