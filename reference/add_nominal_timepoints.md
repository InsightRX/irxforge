# Add nominal timepoints to a dataset

Add nominal timepoints to a dataset

## Usage

``` r
add_nominal_timepoints(
  data,
  time_var = "time",
  nominal_time_var = "NOMINAL_TIME",
  verbose = FALSE,
  ...
)
```

## Arguments

- data:

  `data.frame` or `tibble` with at least a time column

- time_var:

  variable name for time vector in dataset

- nominal_time_var:

  variable name for new nominal time vector

- verbose:

  Logical.

- ...:

  passed as argument to
  [`get_nominal_timepoints()`](https://insightrx.github.io/irxforge/reference/get_nominal_timepoints.md)
  and to [`stats::density()`](https://rdrr.io/r/stats/density.html).

## Value

data.frame or tibble

## See also

[`get_nominal_timepoints()`](https://insightrx.github.io/irxforge/reference/get_nominal_timepoints.md)
