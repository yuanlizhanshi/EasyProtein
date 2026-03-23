# PCA-based clustering using DynamicTreeCut

Perform PCA-based feature or sample clustering using DynamicTreeCut.

## Usage

``` r
auto_cluster_matrix_pca_one(
  mtx,
  mode = c("row", "col"),
  n_pcs = 20,
  method_hclust = "ward.D2",
  deepSplit = 0,
  minClusterSize = 10,
  pamStage = TRUE
)
```

## Arguments

- mtx:

  Numeric matrix. Rows or columns will be clustered.

- mode:

  "row" or "col". Determines clustering direction.

- n_pcs:

  Number of principal components to retain for distance calculation.

- method_hclust:

  Agglomeration method for hierarchical clustering.

- deepSplit:

  DynamicTreeCut parameter controlling cluster granularity.

- minClusterSize:

  Minimum cluster size in DynamicTreeCut.

- pamStage:

  Whether to apply PAM stage refinement in DynamicTreeCut.

## Value

A data.frame containing \`protein_group\` and assigned cluster label.
