# Perform fuzzy clustering on gene-level time-course matrix using Mfuzz

Applies fuzzy c-means clustering (via Mfuzz) to a gene-by-sample numeric
matrix (e.g., log2FC, expression, or kinetic values). Each gene is
assigned to the cluster with the highest membership score.

This function is typically used to classify dynamic genes (e.g., DEGs)
into temporal response patterns.

## Usage

``` r
run_mfuzz_clustering(mat, k = 10, min_sd = 0)
```

## Arguments

- mat:

  A numeric matrix or data.frame with genes in rows and samples/time
  points in columns. Row names must contain gene IDs.

- k:

  Integer. Number of clusters (default = 10).

- min_sd:

  Numeric. Minimum standard deviation threshold for filtering
  low-variance genes prior to clustering (default = 0).

## Value

A tibble with three columns:

- gene:

  Gene identifier (rownames of input matrix).

- gene_group:

  Cluster assignment label (e.g., "C1", "C2", ...).

- type:

  Character label, default set to "DEGs".

## Details

The workflow includes:

1.  Filtering genes by row-wise standard deviation.

2.  Z-score standardization using
    [`Mfuzz::standardise()`](https://rdrr.io/pkg/Mfuzz/man/standardise.html).

3.  Automatic estimation of fuzzifier parameter `m`.

4.  Fuzzy c-means clustering.

5.  Hard assignment based on maximum membership.

## See also

[`mfuzz`](https://rdrr.io/pkg/Mfuzz/man/mfuzz.html),
[`mestimate`](https://rdrr.io/pkg/Mfuzz/man/mestimate.html),
[`standardise`](https://rdrr.io/pkg/Mfuzz/man/standardise.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example: clustering a log2FC matrix
set.seed(1)
mat <- matrix(rnorm(1000), nrow = 100)
rownames(mat) <- paste0("Gene", 1:100)
colnames(mat) <- paste0("T", 1:10)

res <- run_mfuzz_clustering(mat, k = 5)
head(res)
} # }
```
