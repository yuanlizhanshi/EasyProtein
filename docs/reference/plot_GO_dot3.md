# Dot plot for GO/Pathway output (style 3, generic)

A flexible dot plot for enrichment tables containing terms such as:
"term description", "enrichment score", "false discovery rate".

## Usage

``` r
plot_GO_dot3(GO_df, topn = 10, label_format = 30)
```

## Arguments

- GO_df:

  A generic enrichment result table.

- topn:

  Number of terms.

- label_format:

  Width for wrapping labels.

## Value

A ggplot object.
