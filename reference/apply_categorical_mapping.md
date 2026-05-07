# Apply categorical-to-numeric encoding for specified columns

Internal helper used by `reformat_data_*` functions to convert character
or factor columns to numeric values according to a user-supplied mapping
or automatic frequency-based encoding.

## Usage

``` r
apply_categorical_mapping(data, categorical_mapping = NULL)
```

## Arguments

- data:

  A data.frame.

- categorical_mapping:

  Either:

  - A character vector of column names to auto-encode (most common value
    gets 0, next most common gets 1, etc.).

  - A data.frame with columns `column`, `original_value`,
    `encoded_value` (case-insensitive) specifying explicit mappings.
    Values in the data not covered by the mapping receive continuation
    integers starting from `max(encoded_value) + 1`.

  - `NULL` (default) to skip encoding.

  NA values are always encoded as -99.

## Value

The input `data` with specified columns converted to numeric. A
`"categorical_mapping"` attribute is attached: a data.frame with columns
`column`, `original_value`, `encoded_value` describing the full mapping
used.
