# Evaluate an expression, emitting each distinct irxforge MICE warning once

The Gibbs sampler draws each variable's regression many times (per
iteration, replicate and time-varying transition step), so the same
warning would otherwise be repeated many times.

## Usage

``` r
dedupe_mice_warnings(expr)
```

## Arguments

- expr:

  expression to evaluate

## Value

value of `expr`
