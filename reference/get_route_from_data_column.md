# Get the route of administration from a vector of routes for doses

It is assumed that all routes are the same for all patients and drug
administrations.

## Usage

``` r
get_route_from_data_column(x)
```

## Arguments

- x:

  vector of routes, e.g. from EXROUTE columns in EX dataset, or ROUTE
  column in output dataset from `get_data_for_modeling()`.

## Value

route (character), either "iv", "oral", "sc", or "im"
