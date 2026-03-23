# Plot PCA embedding

This function visualizes PCA components stored in pca.df and pca.res.
Labels can be optionally added using ggrepel.

## Usage

``` r
plot_pca(pca.df, pca.res, colorby, label)
```

## Arguments

- pca.df:

  A data frame containing Dim.1 and Dim.2 columns.

- pca.res:

  PCA result object with eigenvalues (eig slot).

- colorby:

  Column name used for coloring.

- label:

  Column name used for labeling points.

## Value

A ggplot object.
