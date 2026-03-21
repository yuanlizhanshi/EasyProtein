# Run GSEA (Gene Set Enrichment Analysis)

Performs GO or KEGG GSEA using ranked gene statistics. This function
dynamically loads clusterProfiler and annotation packages only when
needed, making them optional dependencies.

## Usage

``` r
run_gsea(
  df,
  gene_col = "gene",
  stat_col = "log2FC",
  db = c("GO", "KEGG"),
  ont = "ALL",
  species = c("human", "mouse"),
  keyType = "SYMBOL",
  qvalue_cutoff = 0.05,
  minGSSize = 10,
  maxGSSize = 5000,
  pAdjustMethod = "BH",
  eps = 1e-10,
  use_internal_data = FALSE,
  seed = 123
)
```

## Arguments

- df:

  A data frame containing ranking statistics and gene IDs.

- gene_col:

  Column storing gene IDs.

- stat_col:

  Column storing ranking values (e.g. log2FC).

- db:

  Database for GSEA: `"GO"` or `"KEGG"`.

- ont:

  GO ontology ("BP","CC","MF","ALL").

- species:

  Species: `"human"` or `"mouse"`.

- keyType:

  Gene ID type (default SYMBOL).

- qvalue_cutoff:

  Q-value filter.

- minGSSize:

  Minimum gene set size.

- maxGSSize:

  Maximum gene set size.

- pAdjustMethod:

  Multiple testing correction.

- eps:

  GSEA numeric convergence threshold.

- use_internal_data:

  whether using internal KEGG data

- seed:

  Random seed.

## Value

A `gseaResult` object.
