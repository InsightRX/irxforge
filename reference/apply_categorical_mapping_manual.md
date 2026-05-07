# Build a mapping from a user-supplied data.frame, filling in unmapped classes

Build a mapping from a user-supplied data.frame, filling in unmapped
classes

## Usage

``` r
apply_categorical_mapping_manual(data, mapping_df)
```

## Arguments

- data:

  A data.frame

- mapping_df:

  A data.frame with columns column/original_value/encoded_value

## Value

A data.frame with columns `column`, `original_value`, `encoded_value`
