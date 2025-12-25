# EasyProtein: an interactive and reproducible platform for comprehensive proteomics data analysis



## Installation


EasyProtein requires a recent version of R (≥ 4.1) and several CRAN and Bioconductor packages.


Please install the required package managers first:
You can install the development version of EasyProtein

``` r
# install.packages(c("devtools", "BiocManager"))

BiocManager::install(c(
  "ComplexHeatmap",
  "edgeR",
  "limma",
  "S4Vectors",
  "SummarizedExperiment"
))

devtools::install_github("yuanlizhanshi/EasyProtein")
```


## Tutorial
### Launch the interactive Application

After installing EasyProtein and its dependencies, load the package and launch the Shiny app:

``` r
library(EasyProtein)
launch_EasyProtein()
```

## Citation



## Help

If you have any suggestions and questions, please contact <kongyunhui1@gmail.com>