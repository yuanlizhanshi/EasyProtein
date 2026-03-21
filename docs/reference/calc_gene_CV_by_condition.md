# Calculate coefficient of variation (CV) for each gene per condition

Calculate coefficient of variation (CV) for each gene per condition

## Usage

``` r
calc_gene_CV_by_condition(se, assay_name = "conc", condition_col = "condition")
```

## Arguments

- se:

  SummarizedExperiment object.

- assay_name:

  Assay used for CV calculation (default "conc").

- condition_col:

  Column in colData(se) defining conditions.

## Value

A data.frame containing CV values for each gene per condition.
