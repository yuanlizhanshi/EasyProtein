# Plot missing values per sample

Plot missing values per sample

## Usage

``` r
plotSE_missing_value(se, return_table = FALSE)
```

## Arguments

- se:

  A SummarizedExperiment object.

- return_table:

  Logical. If `TRUE`, return the plotting data.frame instead of a ggplot
  object. Default is `FALSE`.

## Value

A ggplot object summarizing missing count per sample, or a data.frame
when `return_table = TRUE`.
