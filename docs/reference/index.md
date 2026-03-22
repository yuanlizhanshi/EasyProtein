# Package index

## Data import and preprocessing

- [`fix_duplicate_protein_ids()`](https://yuanlizhanshi.github.io/EasyProtein/reference/fix_duplicate_protein_ids.md)
  : Fix duplicated protein IDs
- [`rawdata2se()`](https://yuanlizhanshi.github.io/EasyProtein/reference/rawdata2se.md)
  : Construct a SummarizedExperiment object from raw expression table
- [`impute_low1pct_or_median_raw()`](https://yuanlizhanshi.github.io/EasyProtein/reference/impute_low1pct_or_median_raw.md)
  : Impute missing values in raw intensity using low-percentile sampling
  or geometric mean
- [`calculate_cv()`](https://yuanlizhanshi.github.io/EasyProtein/reference/calculate_cv.md)
  : Calculate coefficient of variation
- [`calc_gene_CV_by_condition()`](https://yuanlizhanshi.github.io/EasyProtein/reference/calc_gene_CV_by_condition.md)
  : Calculate coefficient of variation (CV) for each gene per condition
- [`calc_gene_mean_by_condition()`](https://yuanlizhanshi.github.io/EasyProtein/reference/calc_gene_mean_by_condition.md)
  : Calculate mean expression per gene per condition
- [`summarize_se_by_coldata()`](https://yuanlizhanshi.github.io/EasyProtein/reference/summarize_se_by_coldata.md)
  : Summarize assay values by groups in colData(se)
- [`se2raw()`](https://yuanlizhanshi.github.io/EasyProtein/reference/se2raw.md)
  : Convert SummarizedExperiment to raw intensity data.frame
- [`se2intensity()`](https://yuanlizhanshi.github.io/EasyProtein/reference/se2intensity.md)
  : Convert SummarizedExperiment to intensity data.frame
- [`se2conc()`](https://yuanlizhanshi.github.io/EasyProtein/reference/se2conc.md)
  : Convert SummarizedExperiment to concentration (CPM) data.frame
- [`se2DEGs()`](https://yuanlizhanshi.github.io/EasyProtein/reference/se2DEGs.md)
  : Extract DEG results from SummarizedExperiment
- [`se2gene_group()`](https://yuanlizhanshi.github.io/EasyProtein/reference/se2gene_group.md)
  : Assign genes into dynamic groups based on expression, correlation,
  and CV
- [`se2scale()`](https://yuanlizhanshi.github.io/EasyProtein/reference/se2scale.md)
  : Convert SummarizedExperiment to z-scaled CPM data.frame
- [`scale_mtx()`](https://yuanlizhanshi.github.io/EasyProtein/reference/scale_mtx.md)
  : Scale matrix by row

## Analysis

- [`limma_protein_DE()`](https://yuanlizhanshi.github.io/EasyProtein/reference/limma_protein_DE.md)
  : Limma-based differential expression analysis for proteomics
  (unpaired)
- [`limma_protein_DE_pair()`](https://yuanlizhanshi.github.io/EasyProtein/reference/limma_protein_DE_pair.md)
  : Paired differential expression analysis using limma
- [`enrichment_analysis()`](https://yuanlizhanshi.github.io/EasyProtein/reference/enrichment_analysis.md)
  : Functional enrichment analysis (GO / KEGG)
- [`run_gsea()`](https://yuanlizhanshi.github.io/EasyProtein/reference/run_gsea.md)
  : Run GSEA (Gene Set Enrichment Analysis)
- [`run_mfuzz_clustering()`](https://yuanlizhanshi.github.io/EasyProtein/reference/run_mfuzz_clustering.md)
  : Perform fuzzy clustering on gene-level time-course matrix using
  Mfuzz
- [`get_batch_DEGs()`](https://yuanlizhanshi.github.io/EasyProtein/reference/get_batch_DEGs.md)
  : Run pairwise DEG comparisons for all conditions except a reference
- [`correlate_genes_to_target()`](https://yuanlizhanshi.github.io/EasyProtein/reference/correlate_genes_to_target.md)
  : Calculate gene-wise correlation to target gene profile
- [`classify_timecourse_by_round()`](https://yuanlizhanshi.github.io/EasyProtein/reference/classify_timecourse_by_round.md)
  : Classify genes by multi-round monotonic time-course patterns
- [`get_turning_point()`](https://yuanlizhanshi.github.io/EasyProtein/reference/get_turning_point.md)
  : Count turning points in vector
- [`get_pc_contributors()`](https://yuanlizhanshi.github.io/EasyProtein/reference/get_pc_contributors.md)
  : Extract positive and negative contributors for a given PCA component

## Visualization

- [`ivolcano()`](https://yuanlizhanshi.github.io/EasyProtein/reference/ivolcano.md)
  : Interactive or static volcano plot
- [`saveplot()`](https://yuanlizhanshi.github.io/EasyProtein/reference/saveplot.md)
  : Save plots in multiple formats with standardized resolution
- [`plot_pca()`](https://yuanlizhanshi.github.io/EasyProtein/reference/plot_pca.md)
  : Plot PCA embedding
- [`plotSE_density()`](https://yuanlizhanshi.github.io/EasyProtein/reference/plotSE_density.md)
  : Plot intensity distribution for each sample
- [`plotCV_density()`](https://yuanlizhanshi.github.io/EasyProtein/reference/plotCV_density.md)
  : Plot CV distribution across conditions
- [`plotSE_missing_value()`](https://yuanlizhanshi.github.io/EasyProtein/reference/plotSE_missing_value.md)
  : Plot missing values per sample
- [`plotSE_protein_number()`](https://yuanlizhanshi.github.io/EasyProtein/reference/plotSE_protein_number.md)
  : Plot number of detected proteins per sample
- [`plot_gene_expression()`](https://yuanlizhanshi.github.io/EasyProtein/reference/plot_gene_expression.md)
  : Plot gene expression across samples or conditions
- [`plot_GO_dot1()`](https://yuanlizhanshi.github.io/EasyProtein/reference/plot_GO_dot1.md)
  : Dot plot for GO enrichment (style 1)
- [`plot_GO_dot2()`](https://yuanlizhanshi.github.io/EasyProtein/reference/plot_GO_dot2.md)
  : Dot plot for GO enrichment (style 2)
- [`plot_GO_dot3()`](https://yuanlizhanshi.github.io/EasyProtein/reference/plot_GO_dot3.md)
  : Dot plot for GO/Pathway output (style 3, generic)
- [`plot_FC_trend()`](https://yuanlizhanshi.github.io/EasyProtein/reference/plot_FC_trend.md)
  : Plot multi-round logFC trends for monotonic gene groups
- [`plot_deg_trends()`](https://yuanlizhanshi.github.io/EasyProtein/reference/plot_deg_trends.md)
  : Visualize DEG dynamic trends by Mfuzz cluster
- [`plot_heatmap_withline()`](https://yuanlizhanshi.github.io/EasyProtein/reference/plot_heatmap_withline.md)
  : Plot heatmap with row-wise summary line using ComplexHeatmap
- [`catable()`](https://yuanlizhanshi.github.io/EasyProtein/reference/catable.md)
  : Category table summarizer
- [`geom_mean()`](https://yuanlizhanshi.github.io/EasyProtein/reference/geom_mean.md)
  : Geometric mean
- [`auto_cluster_matrix_pca_one()`](https://yuanlizhanshi.github.io/EasyProtein/reference/auto_cluster_matrix_pca_one.md)
  : PCA-based clustering using DynamicTreeCut

## App and package utilities

- [`EasyProtein-package`](https://yuanlizhanshi.github.io/EasyProtein/reference/EasyProtein.md)
  [`EasyProtein`](https://yuanlizhanshi.github.io/EasyProtein/reference/EasyProtein.md)
  : EasyProtein: A comprehensive proteomics analysis toolkit
- [`launch_EasyProtein()`](https://yuanlizhanshi.github.io/EasyProtein/reference/launch_EasyProtein.md)
  : Launch EasyProtein Shiny App
- [`detect_species_from_symbol()`](https://yuanlizhanshi.github.io/EasyProtein/reference/detect_species_from_symbol.md)
  : Detect species (human or mouse) from gene symbols
