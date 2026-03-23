# Calculate gene-wise correlation to target gene profile

Computes the correlation between every gene in a
\codeSummarizedExperiment object and a target gene profile across all
samples. When multiple target genes are provided, their sample-wise
median expression is used as the reference profile.

The target genes can be matched either against row names of the assay
matrix or, when available, against \coderowData(se)\$gene.

## Usage

``` r
correlate_genes_to_target(se, gene, assay_name = "conc", method = "spearman")
```

## Arguments

- se:

  A \codeSummarizedExperiment object.

- gene:

  A character vector of one or more target genes.

- assay_name:

  Character. Assay name used for correlation calculation. Default is
  \code"conc".

- method:

  Character. Correlation method passed to \codestats::cor. Default is
  \code"pearson".

## Value

A data.frame with one row per gene and correlation values. The returned
columns include: \describe \itemfeature_idRow names of the assay matrix.
\itemgeneGene label. Uses \coderowData(se)\$gene when available,
otherwise row names. \itemcorrelationCorrelation between each gene and
the target profile.
