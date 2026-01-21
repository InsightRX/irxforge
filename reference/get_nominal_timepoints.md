# Get nominal timepoints based on vector of timepoints

Get nominal timepoints based on vector of timepoints

## Usage

``` r
get_nominal_timepoints(t, adjust = 0.5, ...)
```

## Arguments

- t:

  vector of timepoints

- adjust:

  sensitivity for peaks in the density curve to detect nominal
  timepoints. A factor of 0.5 often works well for PK data.

- ...:

  optional arguments passed to
  [`stats::density`](https://rdrr.io/r/stats/density.html) function in
  addition to `adjust`, e.g `bw`. The easiest way to adjust the
  sensitivity to peaks in the data is to use `adjust`.

## Value

a vector of approximate nominal timepoints estimated from the data
