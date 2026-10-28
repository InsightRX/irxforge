# Predictive mean matching: select donors

For each missing value, finds the `donors` observed cases with the
closest predicted values and randomly selects one of them.

## Usage

``` r
mice_match_donors(yhat_obs, yhat_mis, donors = 5L)
```

## Arguments

- yhat_obs:

  predicted values for observed cases

- yhat_mis:

  predicted values for missing cases

- donors:

  number of donors to sample from in predictive mean matching.

## Value

integer vector of indices into the observed cases
