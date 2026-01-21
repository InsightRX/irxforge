# Reformat modeling dataset into a properly checked and validated modeling dataset.

Reformat modeling dataset into a properly checked and validated modeling
dataset.

## Usage

``` r
reformat_data_modeling_to_modeling(data, dictionary = NULL)
```

## Arguments

- data:

  dataset formatted as modeling-ready dataset

- dictionary:

  a data dictionary that maps expected variable names to variables in
  the data.

## Value

data.frame with population PK input data in NONMEM-style format.
