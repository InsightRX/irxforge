# Reformat data between various data set layouts

Reformat data between various data set layouts

## Usage

``` r
reformat_data(
  data,
  dictionary = NULL,
  input_type = c("auto", "nca", "modeling", "sdtm"),
  output_type = c("nca", "modeling"),
  ...
)
```

## Arguments

- data:

  A data set or list of data sets.

- dictionary:

  A data dictionary that maps expected variable names to variables in
  the data.

- input_type:

  The type of input data set, defined as follows:

  - `"nca"`: a data set with one row for every observed concentration
    measurement. Time, concentration value, and administered dose are
    required columns. The data set may contain any other variables,
    covariates, groupings etc in columns.

  - `"modeling"`: a data set with dose events (evid = 1), concentrations
    (evid = 0), and potentially other events (events = 2) in separate
    rows. Dataset requires columns for event type, time, dose amount,
    measured concentration. The dataset may contain any other variables,
    covariates, groupings etc in columns.

  - `"sdtm"`: a list of various data sets or "domains", such as `ADSL`
    `DM`, `EX`, and `DS`, following the SDTM structure and nomenclature.

- output_type:

  type of output dataset. Can be either `"nca"` or `"modeling"`.

- ...:

  passed onto specific reformatting functions:

  - `input_type = "nca"` and `output_type = "modeling"`:
    [`reformat_data_nca_to_modeling()`](https://insightrx.github.io/irxforge/reference/reformat_data_nca_to_modeling.md)

  - `input_type = "sdtm"` and `output_type = "modeling"`:
    [`reformat_data_sdtm_to_modeling()`](https://insightrx.github.io/irxforge/reference/reformat_data_sdtm_to_modeling.md)

  - `input_type = "modeling"` and `output_type = "modeling"`:
    [`reformat_data_modeling_to_modeling()`](https://insightrx.github.io/irxforge/reference/reformat_data_modeling_to_modeling.md)

  - `input_type = "modeling"` and `output_type = "nca"`:
    [`reformat_data_modeling_to_nca()`](https://insightrx.github.io/irxforge/reference/reformat_data_modeling_to_nca.md)

## Value

A data.frame in the format specified by `output_type`.
