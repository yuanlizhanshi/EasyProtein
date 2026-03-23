# Functional enrichment analysis (GO / KEGG)

This function performs GO or KEGG enrichment analysis using
clusterProfiler. Dependencies are loaded dynamically via
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html), so the
function will only require clusterProfiler and annotation packages when
executed.

## Usage

``` r
enrichment_analysis(
  gene,
  db = c("GO", "KEGG"),
  species = c("human", "mouse"),
  keyType = "SYMBOL",
  qvalue_cutoff = 0.05,
  use_internal_data = FALSE
)
```

## Arguments

- gene:

  A character vector of gene symbols.

- db:

  Database to use: `"GO"` or `"KEGG"`.

- species:

  Species: `"human"` or `"mouse"`.

- keyType:

  Gene ID type used in `gene`, e.g. `"SYMBOL"`.

- qvalue_cutoff:

  Q-value threshold for filtering.

- use_internal_data:

  whether using internal KEGG data

## Value

A data frame containing enriched GO/KEGG terms.

## Details

KEGG analysis automatically converts gene IDs to ENTREZID using
[`clusterProfiler::bitr()`](https://rdrr.io/pkg/clusterProfiler/man/bitr.html).
Human and mouse annotation packages (`org.Hs.eg.db` and `org.Mm.eg.db`)
are used depending on species.

## Examples

``` r
if (FALSE) { # \dontrun{
enrichment_analysis(c("TP53","MDM2"), db="GO", species="human")
} # }
```
