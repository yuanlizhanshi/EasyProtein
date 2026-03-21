# Extract DEG results from SummarizedExperiment

This function performs differential expression analysis using
limma-based methods (limma_protein_DE or limma_protein_DE_pair), and
returns a data.frame containing: - rowData(se) - expression matrix - DEG
statistics - group median expression values

## Usage

``` r
se2DEGs(
  se,
  compare_col = "condition",
  ref = NULL,
  cmp = NULL,
  pair_col = NULL,
  logFC_cutoff = 1,
  adj_p_cutoff = 0.05
)
```

## Arguments

- se:

  A SummarizedExperiment object.

- compare_col:

  Column in colData(se) used to define groups.

- ref:

  Reference group (optional).

- cmp:

  Comparison group (optional).

- pair_col:

  Optional column for paired analysis.

- logFC_cutoff:

  Absolute logFC threshold for DEG classification.

- adj_p_cutoff:

  Adjusted p-value threshold for DEG classification.

## Value

A data.frame with DEG statistics and annotations.
