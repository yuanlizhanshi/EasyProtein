# Classify genes by multi-round monotonic time-course patterns

This function classifies genes based on monotonic temporal trends using
an iterative Spearman correlation strategy across progressively trimmed
time windows. At each round, genes exhibiting significant positive or
negative monotonicity are assigned to Up or Down groups corresponding to
the round at which the trend first becomes detectable.

## Usage

``` r
classify_timecourse_by_round(
  mat,
  rho_cut = 0.75,
  min_len = 4,
  trim_side = c("best", "head", "tail")
)
```

## Arguments

- mat:

  A numeric matrix with genes in rows and time-ordered samples in
  columns. Row names must correspond to gene identifiers.

- rho_cut:

  Numeric scalar specifying the absolute Spearman correlation threshold
  for calling a monotonic trend. Default is 0.75.

- min_len:

  Integer specifying the minimum number of samples required to perform
  trend classification. Default is 4.

- trim_side:

  Character string specifying how to trim the time window at each
  iteration. One of:

## Value

A data.frame with one row per gene and the following
columns:gene,gene_group,type

## Details

The time window is shortened by removing one boundary sample (head,
tail, or automatically selected) after each round, until fewer than
min_len samples remain.
