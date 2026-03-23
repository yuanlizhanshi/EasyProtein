# Calculate mean expression per gene per condition

Calculate mean expression per gene per condition

## Usage

``` r
calc_gene_mean_by_condition(
  se,
  assay_name = "conc",
  method = c("mean", "median"),
  condition_col = "condition"
)
```

## Arguments

- se:

  SummarizedExperiment object.

- assay_name:

  Assay used for mean or median calculation.

- method:

  for calculate mean or median.

- condition_col:

  Column in colData(se) defining conditions.

## Value

A numeric matrix of gene-by-condition mean expression.
