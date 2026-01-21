# Create a standardized table for demographics information

Create a standardized table for demographics information

## Usage

``` r
create_demographics_table(
  data,
  demographics = c("weight", "height", "sex", "age", "race", "ethnic", "arm"),
  group = NULL,
  path = NULL
)
```

## Arguments

- data:

  list of data.frames with SDTM dataset, including at least DM table.

- demographics:

  character vector specifying which demographics should be pulled and
  summarized. The function will check availability of the demographics
  and try to figure out whether the data is continuous or categorical.

- group:

  name of variable in dataset to group statistics by, e.g. `"ACTARM"`

- path:

  optional, path to filename to save output table to.

## Value

A data.frame or tibble.
