# Run pairwise DEG comparisons for all conditions except a reference

Run pairwise DEG comparisons for all conditions except a reference

## Usage

``` r
get_batch_DEGs(se, compare_col = "condition", start_col = 1)
```

## Arguments

- se:

  A SummarizedExperiment object.

- compare_col:

  Column in colData(se) representing conditions.

- start_col:

  Index specifying which condition to use as reference.

## Value

A named list of DEG data.frames.
