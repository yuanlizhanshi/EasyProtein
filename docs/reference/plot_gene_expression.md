# Plot gene expression across samples or conditions

This function extracts a given gene from a SummarizedExperiment object,
merges its expression values with sample metadata, and visualizes the
distribution using boxplots (grouped mode) or barplots (single-sample
mode).

## Usage

``` r
plot_gene_expression(se, gene, by = "condition")
```

## Arguments

- se:

  A SummarizedExperiment object.

- gene:

  Gene name to plot.

- by:

  Column in colData(se) used for grouping (default: "condition").

## Value

A ggplot object.
