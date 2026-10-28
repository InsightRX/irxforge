# Default imputation method per variable, based on variable type

Default imputation method per variable, based on variable type

## Usage

``` r
mice_default_method(data, method = NULL)
```

## Arguments

- data:

  data.frame with missing values coded as `NA`. Categorical variables
  must be factors.

- method:

  named character vector with the imputation method per variable.
  Variables not in `method` get a default method based on their type:
  `pmm` for numeric, `logreg` for factors with 2 levels, and `polyreg`
  for factors with more than 2 levels.

## Value

named character vector of methods for all variables in `data`
