# Plot CV distribution across conditions

Plot CV distribution across conditions

## Usage

``` r
plotCV_density(se, return_table = FALSE)
```

## Arguments

- se:

  A SummarizedExperiment object.

- return_table:

  Logical. If `TRUE`, return the plotting data.frame instead of a ggplot
  object. Default is `FALSE`.

## Value

A ggplot object, a data.frame when `return_table = TRUE`, or `NULL` if
no replicates.
