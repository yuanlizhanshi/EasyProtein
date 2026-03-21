# Impute missing values in raw intensity using low-percentile sampling or geometric mean

Missing values within each condition are imputed either from the
low-intensity distribution or via geometric mean when the majority of
values are missing.

## Usage

``` r
impute_low1pct_or_median_raw(
  df,
  id_col = "Protein.Ids",
  sample_col = "sample",
  value_col = "raw_value",
  condition_col = "condition",
  low_prob = 0.01,
  return_log2 = FALSE,
  seed = 1
)
```

## Arguments

- df:

  Long-format data.frame containing raw intensities.

- id_col:

  Protein ID column.

- sample_col:

  Sample name column.

- value_col:

  Value column for raw intensity.

- condition_col:

  Condition or group column.

- low_prob:

  Low percentile used for imputation (default 0.01).

- return_log2:

  Return log2 values instead of raw (default FALSE).

- seed:

  Random seed.

## Value

Long-format data.frame with imputed values.
