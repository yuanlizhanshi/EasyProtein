---
title: "EasyProtein"
---

# EasyProtein

EasyProtein is an R package for streamlined proteomics data analysis and visualization.

## Features

- data import and preprocessing
- quality control
- PCA and clustering
- differential expression analysis
- pathway enrichment
- publication-ready plotting

## Get started


EasyProtein requires a recent version of R (≥ 4.1) and several CRAN and Bioconductor packages.


Please install the required package managers first:

``` r
install.packages(c("devtools", "BiocManager"))

BiocManager::install(c(
  "ComplexHeatmap",
  "edgeR",
  "limma",
  "S4Vectors",
  "Mfuzz",
  "SummarizedExperiment"
))
```
Then you can install the development version of EasyProtein:

```r
devtools::install_github("yuanlizhanshi/EasyProtein")
```
