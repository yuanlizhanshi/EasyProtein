# Visualize DEG dynamic trends by Mfuzz cluster

Visualizes dynamic expression trends of differentially expressed genes
(DEGs) grouped by Mfuzz clustering results stored in \`rowData(se)\`.
The function summarizes gene expression across conditions, optionally
scales expression per gene (z-score), and displays cluster-level average
trends with optional single-gene trajectories.

## Usage

``` r
plot_deg_trends(
  se,
  assay_name = "conc",
  group_by = "condition",
  scale_by_gene = TRUE,
  show_all_genes = TRUE,
  show_ci = FALSE,
  ci_level = 0.95
)
```

## Arguments

- se:

  A `SummarizedExperiment` object. Must contain:

  - An assay specified by `assay_name`

  - A column in `colData(se)` specified by `group_by`

  - Columns `gene`, `gene_group`, and `type` in `rowData(se)`

- assay_name:

  Character. Name of assay to visualize. Default is `"conc"`.

- group_by:

  Character. Column name in `colData(se)` used to define condition or
  time ordering. Default is `"condition"`.

- scale_by_gene:

  Logical. If `TRUE`, expression values are scaled per gene (z-score
  across conditions). Recommended for dynamic pattern visualization.
  Default is `TRUE`.

- show_all_genes:

  Logical. If `TRUE`, individual gene trajectories are shown as
  semi-transparent grey lines. Default is `TRUE`.

- show_ci:

  Logical. If `TRUE`, add 95% confidence intervals for the cluster-level
  mean trend at each condition. Default is `FALSE`.

- ci_level:

  Numeric. Confidence level used when `show_ci = TRUE`. Default is
  `0.95`.

## Value

A `ggplot2` object showing dynamic expression trends for each DEG
cluster.

## Details

The function performs the following steps:

1.  Filters genes labeled as `type == "DEGs"` in `rowData(se)`.

2.  Computes mean expression per gene per condition.

3.  Optionally standardizes expression per gene.

4.  Computes cluster-level mean trends and optional confidence
    intervals.

5.  Generates a faceted line plot by `gene_group`.

This function assumes that Mfuzz clustering results have already been
computed and stored in `rowData(se)$gene_group`.

## Examples

``` r
if (FALSE) { # \dontrun{
  p <- plot_deg_trends(se)
  print(p)
} # }
```
