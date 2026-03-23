# Plot heatmap with row-wise summary line using ComplexHeatmap

This function draws a heatmap using ComplexHeatmap and adds a custom
row-side panel that summarizes each row cluster (defined by `row_split`)
with a line plot (mean or median across columns or column groups). It is
particularly useful for visualizing module-level trends or cluster-level
expression profiles in omics data.

## Usage

``` r
plot_heatmap_withline(
  mat = NULL,
  row_split = NULL,
  column_split = NULL,
  top_annotation = NULL,
  show_column_names = FALSE,
  ht.col.list = list(col_range = c(-2, 0, 2), col_color = c("#08519C", "white",
    "#A50F15")),
  border = TRUE,
  line.size = 0.1,
  line.col = "grey90",
  mline.size = 2,
  mline.col = "#CC3333",
  set.md = "mean",
  textbox.pos = c(0.5, 0.8),
  textbox.size = 8,
  panel.arg = c(2, 0.25, 4, "grey90", NA),
  ...
)
```

## Arguments

- mat:

  A numeric matrix. Rows typically represent genes/features and columns
  represent samples/conditions. The matrix will be internally coerced to
  `matrix` if needed.

- row_split:

  A vector or factor of length `nrow(mat)` used to split rows into
  clusters or groups. Each unique level defines one row group.

- column_split:

  A vector or factor of length `ncol(mat)` used to split columns into
  groups. If provided, summary lines are calculated at the group level;
  otherwise, summaries are calculated across all columns.

- top_annotation:

  A `HeatmapAnnotation` object (from ComplexHeatmap) added to the top of
  the heatmap. Default is `NULL`.

- show_column_names:

  Logical. Whether to display column names on the heatmap. Default is
  `FALSE`.

- ht.col.list:

  A list controlling the color mapping of the heatmap.

  col_range

  :   Numeric vector of length 3 specifying breakpoints for the color
      scale.

  col_color

  :   Character vector of colors corresponding to `col_range`.

- border:

  Logical. Whether to draw borders around heatmap cells. Passed to
  [`ComplexHeatmap::Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html).
  Default is `TRUE`.

- line.size:

  Numeric. Line width for grid lines in the heatmap body. Default is
  `0.1`.

- line.col:

  Character. Color of grid lines in the heatmap body. Default is
  `"grey90"`.

- mline.size:

  Numeric. Line width of the summary line drawn in the right annotation
  panel. Default is `2`.

- mline.col:

  Character. Color of the summary line drawn in the right annotation
  panel. Default is `"#CC3333"`.

- set.md:

  Character. Method for summarizing values within each cluster. Either
  `"mean"` or `"median"`. Default is `"mean"`.

- textbox.pos:

  Numeric vector of length 2 giving the relative position (in NPC units)
  of the annotation text box inside the panel. Default is `c(0.5, 0.8)`.

- textbox.size:

  Numeric. Font size of the annotation text shown in the panel. Default
  is `8`.

- panel.arg:

  A vector of length 5 controlling the layout and appearance of the
  right-side summary panel created by `anno_link`:

  \[1\]

  :   Height of the panel (in cm).

  \[2\]

  :   Gap between heatmap and panel (in cm).

  \[3\]

  :   Width of the panel (in cm).

  \[4\]

  :   Fill color of the panel background.

  \[5\]

  :   Border color of the panel. Use `NA` to remove the border.

  Default is `c(2, 0.25, 4, "grey90", NA)`.

- ...:

  Additional arguments passed to
  [`ComplexHeatmap::Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html).

## Value

A `Heatmap` object from ComplexHeatmap. The object can be further
modified, combined with other heatmaps, or drawn using
[`draw()`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-dispatch.html).

## Details

The function performs the following steps:

1.  Split rows according to `row_split`.

2.  Assign random colors to each row group and display them as block
    annotations on the right side.

3.  For each row group, compute either the mean or median expression
    across columns (or across column groups if `column_split` is
    provided).

4.  Plot the summarized values as a line within a custom panel aligned
    to the corresponding row cluster.

This visualization is particularly suitable for showing module-level or
cluster-level trends, such as temporal patterns or condition-specific
shifts in expression.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ComplexHeatmap)

# Example matrix
mat <- matrix(rnorm(200), nrow = 20)
rownames(mat) <- paste0("Gene", 1:20)
colnames(mat) <- paste0("Sample", 1:10)

# Row and column grouping
row_grp <- rep(LETTERS[1:4], each = 5)
col_grp <- rep(c("Ctrl", "Treat"), each = 5)

# Draw heatmap with summary lines
ht <- plot_heatmap_withline(
  mat = mat,
  row_split = row_grp,
  column_split = col_grp,
  set.md = "mean"
)

draw(ht)
} # }
```
