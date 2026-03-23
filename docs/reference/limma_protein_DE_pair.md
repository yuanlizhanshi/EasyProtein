# Paired differential expression analysis using limma

Paired differential expression analysis using limma

## Usage

``` r
limma_protein_DE_pair(
  expr,
  group,
  donor = NULL,
  offset = NULL,
  trend = TRUE,
  robust = TRUE,
  contrast_str = NULL
)
```

## Arguments

- expr:

  Expression matrix.

- group:

  Factor defining two comparison groups.

- donor:

  Pairing variable (e.g., donor ID).

- offset:

  Offset added before log2 transformation.

- trend:

  Apply trend eBayes.

- robust:

  Apply robust eBayes.

- contrast_str:

  Contrast specification for limma.

## Value

A data.frame containing limma statistics for paired tests.
