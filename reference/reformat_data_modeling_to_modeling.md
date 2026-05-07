# Reformat modeling dataset into a properly checked and validated modeling dataset.

Reformat modeling dataset into a properly checked and validated modeling
dataset.

## Usage

``` r
reformat_data_modeling_to_modeling(
  data,
  dictionary = NULL,
  categorical_mapping = NULL,
  na = "."
)
```

## Arguments

- data:

  dataset formatted as modeling-ready dataset

- dictionary:

  a data dictionary that maps expected variable names to variables in
  the data.

- categorical_mapping:

  Either a character vector of column names to auto-encode (most common
  value gets 0, next gets 1, etc.), or a data.frame with columns
  `column`, `original_value`, `encoded_value` for explicit mappings. NA
  values are encoded as -99. The final mapping is attached as a
  `"categorical_mapping"` attribute on the returned data.frame. Default
  `NULL` skips encoding.

- na:

  what to set NA values to. E.g. ".", (default) or NA (keep NA), or NULL
  (do nothing).

## Value

data.frame with population PK input data in NONMEM-style format.
