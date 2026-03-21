# Interactive or static volcano plot

This function creates a volcano plot with optional interactivity via
ggiraph. Code from https://github.com/YuLab-SMU/ivolcano

## Usage

``` r
ivolcano(
  data,
  logFC_col = "logFC",
  pval_col = "adj.P.Val",
  gene_col = "gene",
  title = "",
  interactive = TRUE,
  onclick_fun = NULL,
  pval_cutoff = 0.05,
  logFC_cutoff = 1,
  top_n = 10,
  label_mode = "separate",
  fontface = "italic",
  label_sig_only = TRUE,
  threshold_line = list(color = "black", linetype = "dashed", linewidth = 0.5),
  sig_colors = c(Up = "red", Down = "blue", Not_Significant = "grey70"),
  size_by = "none"
)
```

## Arguments

- data:

  Input data frame.

- logFC_col:

  Column name storing log fold change.

- pval_col:

  Column storing adjusted P value.

- gene_col:

  Column storing gene names.

- title:

  Plot title.

- interactive:

  Whether to use ggiraph.

- onclick_fun:

  Optional function for onclick events.

- pval_cutoff:

  FDR threshold.

- logFC_cutoff:

  LogFC threshold.

- top_n:

  Number of labels to draw.

- label_mode:

  Label mode.

- fontface:

  Label font face.

- label_sig_only:

  Whether to only label significant genes.

- threshold_line:

  Line settings for cutoff.

- sig_colors:

  Colors for Up/Down/Not_Significant.

- size_by:

  Optional scaling variable.

## Value

A list with static ggplot object and interactive girafe object.
