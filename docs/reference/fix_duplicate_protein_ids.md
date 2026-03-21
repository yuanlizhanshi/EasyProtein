# Fix duplicated protein IDs

Detect and automatically rename duplicated IDs in a proteomics matrix.

## Usage

``` r
fix_duplicate_protein_ids(df, id_col = "Protein.Ids")
```

## Arguments

- df:

  A data.frame containing protein information.

- id_col:

  Column name containing protein identifiers.

## Value

A data.frame with fixed unique IDs.
