# Limma-based differential expression analysis for proteomics (unpaired)

Limma-based differential expression analysis for proteomics (unpaired)

## Usage

``` r
limma_protein_DE(
  expr,
  group,
  offset = NULL,
  contrast_str = NULL,
  trend = TRUE,
  robust = TRUE
)
```

## Arguments

- expr:

  Expression matrix.

- group:

  Factor defining two groups to compare.

- offset:

  Offset added before log2 transformation.

- contrast_str:

  Contrast string for limma (e.g., "B-A").

- trend:

  Whether to apply trend eBayes.

- robust:

  Whether to apply robust eBayes.

## Value

A data.frame containing logFC, P-values, adjusted P-values, mean log2
expression per group, and delta mean.
