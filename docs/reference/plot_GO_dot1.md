# Dot plot for GO enrichment (style 1)

This function takes a GO enrichment result (e.g. clusterProfiler output)
and visualizes the top enriched categories using -log10(p.adjust).

## Usage

``` r
plot_GO_dot1(GO_df, topn = 10, label_format = 30)
```

## Arguments

- GO_df:

  A GO enrichment result data frame.

- topn:

  Number of categories to show.

- label_format:

  Width for wrapped text labels.

## Value

A ggplot object.
