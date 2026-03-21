# Plot multi-round logFC trends for monotonic gene groups

This function visualizes log fold-change (logFC) trajectories of genes
classified by multi-round monotonic time-course patterns (e.g. `Up1`,
`Down2`). Individual gene trajectories are shown as semi-transparent
lines, while the group-level mean trajectory is overlaid for each
pattern.

## Usage

``` r
plot_FC_trend(se, group_by = "condition")
```

## Arguments

- se:

  A SummarizedExperiment object containing concentration-based
  expression data and log fold-change values.

- group_by:

  Character string specifying the column in `colData(se)` used to define
  experimental conditions. Default is `"condition"`.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object
showing logFC trajectories faceted by gene trend group.

## Details

Genes are grouped according to their assigned trend group, and facets
are ordered by round index, with `Up*` and `Down*` displayed as paired
groups within each round.

The function performs the following steps:

1.  Convert the `SummarizedExperiment` object to a gene-level data frame
    using [`se2conc()`](se2conc.md).

2.  Retain only genes classified as `Up` or `Down`.

3.  Reshape logFC values into long format for plotting.

4.  Reorder gene groups as paired rounds (`Up1`, `Down1`, `Up2`,
    `Down2`, ...).

5.  Plot individual gene trajectories and overlay group-level mean
    trends.

Facet labels include the number of genes (`n`) in each group.

## See also

[`classify_timecourse_by_round`](classify_timecourse_by_round.md),
[`se2conc`](se2conc.md),
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)

## Examples

``` r
if (FALSE) { # \dontrun{
p <- plot_FC_trend(se, group_by = "condition")
print(p)
} # }
```
