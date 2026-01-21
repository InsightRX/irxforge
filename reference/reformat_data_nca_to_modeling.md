# Reformat NCA-type analysis-ready dataset (ARD) into a NONMEM-style modeling dataset

Reformat NCA-type analysis-ready dataset (ARD) into a NONMEM-style
modeling dataset

## Usage

``` r
reformat_data_nca_to_modeling(
  data,
  dictionary = list(subject_id = "ID", group = "GROUP", time = "TIME", dose = "AMT", conc
    = "DV"),
  dose_compartment = 1,
  obs_compartment = 1,
  covariates = NULL
)
```

## Arguments

- data:

  dataset formatted as NCA analysis-ready dataset

- dictionary:

  a data dictionary that maps expected variable names to variables in
  the data.

- dose_compartment:

  the compartment in which doses are entered

- obs_compartment:

  the observation compartment number

- covariates:

  a vector of covariate names that are to be extracted and added to the
  modeling dataset.

## Value

data.frame with population PK input data in NONMEM-style format.
