# Left square root of a symmetric positive semi-definite matrix

Returns `L` such that `L %*% t(L)` equals `v`: the transposed Cholesky
factor when `v` is positive definite, otherwise a square root based on
the eigendecomposition (negative eigenvalues from rounding are set to
zero).

## Usage

``` r
mice_sqrt_psd(v)
```

## Arguments

- v:

  symmetric positive semi-definite matrix

## Value

numeric matrix
