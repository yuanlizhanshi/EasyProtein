# Category table summarizer

Produce a table summarizing how many values fall within specified cut
categories, optionally cumulative.

## Usage

``` r
catable(
  data,
  categories = c(quantile(data, c(0.01, 0.1, 0.5, 0.9, 0.99), na.rm = TRUE)),
  cumulative = FALSE,
  na.rm = TRUE,
  digits = 3
)
```

## Arguments

- data:

  Numeric vector.

- categories:

  Numeric vector of category boundaries.

- cumulative:

  Logical, whether cumulative counts should be returned.

- na.rm:

  Logical, remove NAs before computation.

- digits:

  Number of digits to round results.

## Value

A 2-row matrix summarizing counts and proportions.
