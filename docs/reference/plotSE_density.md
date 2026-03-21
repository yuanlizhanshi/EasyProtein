# Plot intensity distribution for each sample

This function draws violin + boxplot of log2 intensity for each sample
from a SummarizedExperiment object.

## Usage

``` r
plotSE_density(se, return_table = FALSE)
```

## Arguments

- se:

  A SummarizedExperiment object.

- return_table:

  Logical. If `TRUE`, return the plotting data.frame instead of a ggplot
  object. Default is `FALSE`.

## Value

A ggplot violin plot, or a data.frame when `return_table = TRUE`.
