# Assign genes into dynamic groups based on expression, correlation, and CV

This function identifies gene behavior patterns (Up, Down, Stable,
fluctuated, Low expression) based on: - time-course correlation -
differential expression - mean expression - coefficient of variation
(CV)

## Usage

``` r
se2gene_group(
  se,
  assay_name = "conc",
  group_by = "condition",
  min_expresion_threhold = 1,
  CV_with_time_threhold = 0.1,
  padj_threhold = 0.05,
  k_cluster = 10
)
```

## Arguments

- se:

  A SummarizedExperiment object.

- assay_name:

  Assay name to use (default "conc").

- group_by:

  Column in colData(se) defining sample ordering.

- min_expresion_threhold:

  Minimum mean expression.

- CV_with_time_threhold:

  CV threshold used to define stable genes.

- padj_threhold:

  Adjusted p-value threshold for DEGs.

- k_cluster:

  Number of clusters for mufzz

## Value

Updated SummarizedExperiment with gene grouping added to rowData.
