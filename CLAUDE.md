# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Package Purpose

`irxforge` is an R package for generating, parsing, and converting
pharmacometric datasets. It handles: - Reformatting clinical trial data
between analysis formats (NCA ↔︎ NONMEM/modeling, SDTM → NONMEM) -
Sampling covariates for population PK simulations from multiple sources
(multivariate normal, bootstrap, MICE imputation, NHANES)

## Common Commands

``` r

# Run all tests
devtools::test()

# Run a single test file
devtools::test(filter = "sample_covariates_nhanes")

# Regenerate documentation after roxygen2 changes
devtools::document()

# Full package check
devtools::check()

# Install package locally
devtools::install()
```

## Development Rules

- **Version bump**: For any change that is made in the code, the package
  version should be updated in `DESCRIPTION`.
- **Documentation**: After making any change in function description,
  always update the package documentation by running
  `devtools::document()` to regenerate `man/` files.

## Architecture

### Data Reformatting (`R/reformat_data*.R`)

[`reformat_data()`](https://insightrx.github.io/irxforge/reference/reformat_data.md)
is the main dispatcher — it auto-detects input format and routes to
specialized functions: -
[`reformat_data_nca_to_modeling()`](https://insightrx.github.io/irxforge/reference/reformat_data_nca_to_modeling.md)
— NCA-ready → NONMEM (separates dose events EVID=1 from observations
EVID=0) -
[`reformat_data_modeling_to_modeling()`](https://insightrx.github.io/irxforge/reference/reformat_data_modeling_to_modeling.md)
— validation/cleanup of NONMEM datasets (uppercase columns, MDV
column) -
[`reformat_data_sdtm_to_modeling()`](https://insightrx.github.io/irxforge/reference/reformat_data_sdtm_to_modeling.md)
— SDTM domains (ADSL, DM, EX, PC, VS, LB) → NONMEM using the `admiral`
package; derives clinical covariates (BMI, BSA, CrCl, eGFR) -
[`reformat_data_modeling_to_nca()`](https://insightrx.github.io/irxforge/reference/reformat_data_modeling_to_nca.md)
— stub, not yet implemented

### Covariate Sampling (`R/sample_covariates*.R`)

[`sample_covariates()`](https://insightrx.github.io/irxforge/reference/sample_covariates.md)
dispatches on `method` argument to: -
[`sample_covariates_mvtnorm()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mvtnorm.md)
— fits multivariate normal to observed data, samples from it -
[`sample_covariates_bootstrap()`](https://insightrx.github.io/irxforge/reference/sample_covariates_bootstrap.md)
— nonparametric resampling with replacement -
[`sample_covariates_mice()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice.md)
— multiple imputation via chained equations (predictive mean matching),
using the internal dependency-free
[`mice_impute()`](https://insightrx.github.io/irxforge/reference/mice_impute.md)
(`R/mice_impute.R`) -
[`sample_covariates_nhanes()`](https://insightrx.github.io/irxforge/reference/sample_covariates_nhanes.md)
— downloads/caches NHANES survey data; supports probability-proportional
sampling via survey weights (`WTMEC2YR`) -
[`sample_covariates_copulas()`](https://insightrx.github.io/irxforge/reference/sample_covariates_copulas.md)
— fits a vine copula (Zwep et al. 2024, `rvinecopulib`) with
kernel-density marginals to the observed covariates and samples virtual
subjects, reproducing the full dependence structure (continuous
covariates only) -
[`sample_covariates_mice_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_mice_timevarying.md)
/
[`sample_covariates_lme_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_lme_timevarying.md)
/
[`sample_covariates_copulas_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_copulas_timevarying.md)
— time-varying (longitudinal) covariate samplers. MICE resamples
observed transitions via chained equations; LME fits per-covariate
mixed-effects transition models; copulas (Zwep et al. 2024,
`rvinecopulib`) fit a vine copula to per-subject polynomial-trajectory
coefficients. The two MICE/LME samplers share a `baseline_method`
(`"mice"`/`"bootstrap"`) and a `noise` (multiplicative log-normal
jitter) option for the baseline draw.

### NHANES Caching (`R/download_nhanes_cache.R`, `R/zzz.R`)

The `.onLoad()` hook auto-downloads NHANES 2017–2018 data into
`nhanes_cache/` on first package load.
[`download_nhanes_cache()`](https://insightrx.github.io/irxforge/reference/download_nhanes_cache.md)
supports years 1999–2020. Cache files are per-year RDS files merging
DEMO, LAB, EXAM, and Q tables.

### Shared Patterns

- **Dictionary-based column mapping**: All reformatting and sampling
  functions accept a `dictionary` argument
  (`list(subject_id = "ID", dose = "AMT", ...)`) to map expected column
  names to actual dataset column names.
- **Conditional filtering**: Sampling functions accept
  `conditional = list(AGE = c(18, 65), WT = c(50, 100))` to restrict
  sampling to specific covariate ranges.
- **Seed argument**: All sampling functions accept `seed` for
  reproducibility, passed through the dispatcher.
- **`na.rm` argument**: Bootstrap and NHANES functions drop NA rows
  before sampling (default `TRUE`).

## Key Dependencies

- `admiral` — SDTM data handling in
  [`reformat_data_sdtm_to_modeling()`](https://insightrx.github.io/irxforge/reference/reformat_data_sdtm_to_modeling.md)
- `mvtnorm` — multivariate normal sampling
- `nlme` — mixed-effects transition/trajectory models in the LME and
  copula time-varying samplers
- `rvinecopulib` (Suggests) — vine copula fitting in
  [`sample_covariates_copulas_timevarying()`](https://insightrx.github.io/irxforge/reference/sample_covariates_copulas_timevarying.md)
- `irxutils` — custom utility package (GitHub: InsightRX/irxutils);
  provides `%<=%`, `%>=%`, `is_continuous()`
- `nhanesA` (Suggests) — NHANES API access; only needed for
  [`sample_covariates_nhanes()`](https://insightrx.github.io/irxforge/reference/sample_covariates_nhanes.md)
  and
  [`download_nhanes_cache()`](https://insightrx.github.io/irxforge/reference/download_nhanes_cache.md)
