# Convert SummarizedExperiment to concentration (CPM) data.frame

Convert SummarizedExperiment to concentration (CPM) data.frame

## Usage

``` r
se2conc(se, group_by = "condition")
```

## Arguments

- se:

  A SummarizedExperiment object.

- group_by:

  Column in colData(se) used for grouping (default: "condition").

## Value

A data.frame containing rowData and conc assay.
