# Summarize assay values by groups in colData(se)

Summarize assay values by groups in colData(se)

## Usage

``` r
summarize_se_by_coldata(
  se,
  group_by,
  assay_name = "conc",
  method = c("mean", "median"),
  na.rm = TRUE
)
```

## Arguments

- se:

  SummarizedExperiment object.

- group_by:

  Column defining groups.

- assay_name:

  Assay to summarize.

- method:

  "mean" or "median".

- na.rm:

  Logical.

## Value

A matrix of summarized expression.
