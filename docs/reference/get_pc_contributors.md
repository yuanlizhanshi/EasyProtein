# Extract positive and negative contributors for a given PCA component

This function extracts the top contributing features (e.g.
proteins/genes) for a specified principal component (PC) from a
[`FactoMineR::PCA`](https://rdrr.io/pkg/FactoMineR/man/PCA.html) result.
It supports both signed loadings (coordinates) for direction-aware
interpretation and contribution values for importance ranking, and can
optionally merge feature annotations from a `SummarizedExperiment`
object.

## Usage

``` r
get_pc_contributors(
  pca.res,
  se_obj = NULL,
  pc = 1,
  n = 20,
  use = c("coord", "contrib")
)
```

## Arguments

- pca.res:

  A `PCA` object returned by
  [`FactoMineR::PCA`](https://rdrr.io/pkg/FactoMineR/man/PCA.html).

- se_obj:

  A `SummarizedExperiment` object. If provided, row annotations (from
  `rowData(se_obj)`) will be joined to PCA variables using
  `Protein.Ids`. Default is `NULL`.

- pc:

  Integer. The principal component to extract (e.g. `1` for PC1).
  Default is `1`.

- n:

  Integer. Number of top features to return for each direction (positive
  and negative). Default is `20`.

- use:

  Character. Which PCA metric to use:

  `"coord"`

  :   Use signed coordinates (loadings). This preserves directionality
      and is suitable for identifying positive vs negative contributors
      along the PC.

  `"contrib"`

  :   Use contribution values. This ranks features by importance to the
      PC but does not encode direction.

  Default is `"coord"`.

## Value

A list with the following components:

- PC:

  Character string indicating the selected PC (e.g. `"Dim.1"`).

- metric:

  Which metric was used: `"coord"` or `"contrib"`.

- positive:

  A data frame of the top `n` positively contributing features (only for
  `"coord"`).

- negative:

  A data frame of the top `n` negatively contributing features (only for
  `"coord"`).

## Details

The function operates on `pca.res$var[[use]]`, where:

- `coord` represents the signed loadings (projection of each feature
  onto the PC axis), allowing interpretation of positive vs negative
  directions.

- `contrib` represents the percentage contribution of each feature to
  the variance of the PC, without directional information.

When `se_obj` is provided, feature-level annotations are merged from
`rowData(se_obj)` by `Protein.Ids`. Gene symbols are extracted from the
`Genes` column (taking the first symbol before a semicolon).

If multiple protein entries map to the same gene, consider collapsing
results at the gene level downstream.

## Examples

``` r
if (FALSE) { # \dontrun{
library(FactoMineR)

# Run PCA
pca.res <- PCA(t(expr_matrix), scale.unit = TRUE, graph = FALSE)

# Extract top contributors on PC1 using signed coordinates
res_pc1 <- get_pc_contributors(pca.res, pc = 1, n = 30, use = "coord")

res_pc1$positive
res_pc1$negative

# Using contribution values instead of signed loadings
res_pc1_contrib <- get_pc_contributors(pca.res, pc = 1, n = 30, use = "contrib")
} # }
```
