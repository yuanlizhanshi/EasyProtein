# Dot plot for GO enrichment (style 2)

This visualization computes GeneRatio and displays enriched GO terms
using a colored dot plot.

## Usage

``` r
plot_GO_dot2(GO_df, topn = 10, label_format = 30)
```

## Arguments

- GO_df:

  A GO enrichment result data frame.

- topn:

  Number of GO terms.

- label_format:

  Width for wrapped labels.

## Value

A ggplot object.
