# Codex 工作指南：EasyProtein Case Study 1

## 1. 任务目标

基于仓库中已经准备好的公共定量蛋白质组数据、EasyProtein 函数和现有 vignettes，完成文章 **Case Study 1** 的全部分析、绘图和结果整理。

本案例的目标不是提出新的生物学机制，而是通过一个真实公共数据集展示 EasyProtein 的常规端到端分析能力：

> raw quantitative protein matrix → structured `SummarizedExperiment` → preprocessing → quality control → sample-structure visualization → differential analysis → functional interpretation → publication-ready figures and exportable tables.

最终需要生成：

1. Case Study 1 主图 Figure 2 的所有 panel；
2. 对应的 Supplementary Figures；
3. Supplementary Tables；
4. 完整、可重复执行的 R 分析脚本；
5. 用于 manuscript 的结果数字和简短结果摘要。

---

## 2. 总体原则

### 2.1 必须复用 EasyProtein

分析优先调用 EasyProtein 已有函数，不要重新实现已有功能。需要重点复用：

- `rawdata2se()`
- `plotSE_density()`
- `plotSE_missing_value()`
- `plotSE_protein_number()`
- `plot_pca()`
- `se2DEGs()`
- `enrichment_analysis()`
- `plot_gene_expression()`
- 其他仓库中已经存在、且适用于本案例的 QC、heatmap、volcano、normalization、filtering 和 export 函数

如现有函数不能直接满足 publication-level 图形要求，可以：

1. 先使用 EasyProtein 函数完成数据计算；
2. 再基于函数返回结果单独编写绘图代码；
3. 不要复制或重写核心统计逻辑。

### 2.2 不修改原始数据

- 原始数据文件只读；
- 所有中间对象保存到新的 analysis/output 目录；
- 不要覆盖 package 自带数据、vignettes 或原始矩阵。

### 2.3 所有结果必须可重复

- 所有随机过程设置 `set.seed(123)`；
- 所有过滤阈值、比较方向和富集参数必须写入配置区；
- 所有 figure 必须由脚本自动生成，禁止手工修改数值或图形元素；
- 保存 `sessionInfo()`；
- 脚本必须从干净 R session 中可以顺序运行。

### 2.4 不安装任何额外软件或 R 包

如果发现缺少依赖：

- 不要自动安装；
- 在日志中列出缺少的包；
- 优先使用项目当前环境中已有包和 EasyProtein 已声明依赖；
- 将缺失依赖及其用途写入 `missing_dependencies.md`。

---

## 3. 需要先阅读的文件

开始工作前，按顺序阅读：

1. `manuscript/section1_overview.md` 或名称最接近的 overview 文件；
2. `vignettes/quick_start.Rmd`；
3. `vignettes/multiple_sample_analysis.Rmd`；
4. `DESCRIPTION`；
5. `NAMESPACE`；
6. `R/` 下与以下功能相关的脚本：
   - data import
   - preprocessing
   - missing-value handling
   - normalization/scaling
   - quality control
   - PCA
   - differential analysis
   - volcano plot
   - heatmap
   - enrichment analysis
   - expression visualization
7. `inst/extdata/` 下公共示例数据及相关 metadata；
8. 仓库中已有的 manuscript figure theme、配色和保存函数。

不要只根据函数名称猜测行为。必须确认：

- 输入 assay 名称；
- 输出对象结构；
- log transform 是否已执行；
- missing-value filtering 规则；
- normalization 方法；
- differential comparison 的方向定义；
- `logFC > 0` 对应哪一组更高。

---

## 4. 数据集和分析设计

当前 vignettes 使用的公共数据文件为：

```r
system.file(
  "extdata",
  "mouse_multi-organ_DIA_report.pg_matrix.tsv.gz",
  package = "EasyProtein"
)
```

如项目中已有同一数据的本地分析版本，优先使用项目内明确指定的数据路径，但需要记录实际路径。

### 4.1 全数据集用途

使用全部样本完成：

- 数据导入；
- metadata 构建；
- 全局 QC；
- missing-value assessment；
- detected protein summary；
- sample correlation；
- PCA；
- 可选 UMAP；
- dataset composition summary。

### 4.2 差异分析子集

使用 vignette 中的标准比较：

- tissue: Liver
- sex: M
- reference group: 1w
- comparison group: 8w
- comparison direction: `8w versus 1w`

必须先检查 metadata 中实际值和大小写，再统一标准化：

```r
se$tissue <- stringr::str_to_title(se$tissue)
se$sex <- stringr::str_to_upper(se$sex)
se$age <- factor(se$age, levels = c("1w", "4w", "8w"))
```

重点检查并修正现有 vignette 中可能存在的：

- `live` 与 `Liver` 不一致；
- `heart` 与 `Heart` 不一致；
- `M` 与 `m` 不一致。

任何修改都必须在 case-study 脚本中显式完成，不要静默依赖模糊匹配。

---

## 5. 建议目录结构

在仓库中建立：

```text
case_study1/
├── README.md
├── config.R
├── scripts/
│   ├── 00_check_environment.R
│   ├── 01_import_and_metadata.R
│   ├── 02_preprocessing_and_qc.R
│   ├── 03_sample_structure.R
│   ├── 04_differential_analysis.R
│   ├── 05_functional_enrichment.R
│   ├── 06_main_figure.R
│   ├── 07_supplementary_figures.R
│   ├── 08_export_tables.R
│   └── 09_generate_summary.R
├── R/
│   ├── plot_theme.R
│   ├── figure_helpers.R
│   └── validation_helpers.R
├── output/
│   ├── objects/
│   ├── tables/
│   ├── figures/
│   │   ├── main/
│   │   └── supplementary/
│   ├── logs/
│   └── manuscript_values/
└── run_all.R
```

如果仓库已有统一的 manuscript 或 analysis 目录规范，则遵循现有规范，但保持模块化脚本结构。

---

## 6. 配置文件

在 `config.R` 中集中定义：

```r
seed <- 123

comparison <- list(
  tissue = "Liver",
  sex = "M",
  ref = "1w",
  cmp = "8w",
  label = "Male liver: 8 weeks versus 1 week"
)

deg_threshold <- list(
  logFC = 1,
  adj_p = 0.05
)

enrichment_threshold <- list(
  qvalue = 0.05,
  top_n = 8
)

figure_size <- list(
  single_width = 4,
  single_height = 3.5,
  main_width = 14,
  main_height = 10
)
```

不要把阈值散落在各个脚本中。

---

## 7. 分析步骤

# Step 1. 环境和函数检查

脚本：`00_check_environment.R`

完成：

1. 加载 EasyProtein；
2. 检查所需函数是否存在；
3. 检查数据文件是否存在；
4. 检查必需 R 包；
5. 输出 package version；
6. 记录 `sessionInfo()`；
7. 建立输出目录。

输出：

- `output/logs/environment_check.txt`
- `output/logs/sessionInfo.txt`

如发现关键函数或数据缺失，使用明确错误信息停止，不要继续生成空图。

---

# Step 2. 数据导入和 metadata 构建

脚本：`01_import_and_metadata.R`

完成：

1. 使用 `rawdata2se()` 导入数据；
2. 提取 `se_obj$se`；
3. 检查 assays、rowData 和 colData；
4. 从 condition 或 sample name 中解析：
   - tissue
   - age
   - sex
   - replicate
5. 标准化 metadata 大小写和 factor 顺序；
6. 检查重复 sample name；
7. 检查每个 tissue × age × sex 的样本数；
8. 检查 expression matrix 的维度和 NA 数量。

输出：

- `output/objects/se_imported.rds`
- `output/tables/sample_metadata.csv`
- `output/tables/dataset_composition.csv`
- `output/tables/import_summary.csv`

`import_summary.csv` 至少包含：

- number of proteins before filtering
- number of samples
- number of tissues
- number of age groups
- number of male/female samples
- total missing-value fraction
- assay names

---

# Step 3. 预处理和 QC

脚本：`02_preprocessing_and_qc.R`

必须首先阅读 `rawdata2se()` 及相关函数源码，确认哪些步骤已经自动执行，避免二次 log transform 或二次 normalization。

完成：

1. 保存可用的 raw/processed/normalized/concentration assays；
2. 计算每个样本：
   - total/median intensity
   - missing fraction
   - detected protein number
3. 计算每个蛋白：
   - missing fraction
   - mean abundance
   - CV，若适用
4. 根据 EasyProtein 默认逻辑执行 filtering；
5. 记录 filtering 前后保留蛋白数；
6. 使用 EasyProtein QC 函数生成基础图；
7. 同时生成 publication-ready 版本。

输出表：

- `sample_qc_metrics.csv`
- `protein_qc_metrics.csv`
- `filtering_summary.csv`

输出图：

- raw or initial intensity density
- processed intensity density
- sample missing fraction
- detected protein count
- protein missingness distribution
- CV distribution

注意：

- 如果数据对象不存在真正意义上的“处理前 assay”，不要伪造 before/after 图；
- 此时主图只展示 processed intensity distribution，Supplement 中展示 filtering summary；
- 在日志中说明哪些 preprocessing 已由 `rawdata2se()` 自动完成。

---

# Step 4. 样本结构分析

脚本：`03_sample_structure.R`

完成：

1. 选用 EasyProtein 推荐的 processed/concentration assay；
2. 删除全 NA 或零方差蛋白；
3. 计算 sample–sample Spearman correlation；
4. 绘制 correlation heatmap；
5. 运行 PCA；
6. 输出 variance explained；
7. PCA 分别按以下变量着色：
   - tissue
   - age
   - sex
8. 可选运行 UMAP，仅用于 Supplementary Figure；
9. 检查每个 biological group 的 replicate dispersion；
10. 标记潜在 outlier，但不要自动删除。

输出：

- `sample_correlation_matrix.csv`
- `pca_coordinates.csv`
- `pca_variance_explained.csv`
- `potential_outliers.csv`

图形要求：

- 主图 PCA：color = tissue；
- shape 可使用 age 或 sex，但不能导致图例过度拥挤；
- 不给所有点添加长文本标签；
- 只标注需要解释的离群点或组织中心；
- correlation heatmap 添加 tissue、age、sex annotation。

---

# Step 5. 差异蛋白分析

脚本：`04_differential_analysis.R`

分析子集：

```r
se_liver_male <- se[, se$tissue == "Liver" & se$sex == "M"]
se_de <- se_liver_male[, se_liver_male$age %in% c("1w", "8w")]
```

运行：

```r
deg_res <- se2DEGs(
  se = se_de,
  compare_col = "age",
  ref = "1w",
  cmp = "8w",
  logFC_cutoff = 1,
  adj_p_cutoff = 0.05
)
```

必须验证：

- `logFC > 0` 是否表示 8w 高于 1w；
- ref/cmp 方向是否与标题一致；
- 每组样本数；
- design matrix 是否满秩；
- 是否存在缺失 group；
- 是否存在异常 P value；
- 是否存在 duplicated gene symbols。

生成：

1. full DEG table；
2. UP/DOWN/NS counts；
3. volcano plot；
4. MA plot；
5. P-value distribution；
6. top differential protein heatmap；
7. representative protein expression plots。

主图 heatmap：

- 选择 top 30–50 proteins；
- 首先筛选 `adj.P.Val <= 0.05`；
- 再按 `abs(logFC)` 和 `adj.P.Val` 排序；
- row z-score；
- columns 按 age 和 replicate 固定顺序；
- rows 可 split 为 UP 和 DOWN；
- 不允许使用只基于选定样本结果的 circular clustering 解释。

代表性蛋白：

- 选择 2–4 个；
- 至少一个 UP、一个 DOWN；
- 优先来自主富集通路；
- 展示所有 biological replicates；
- 使用 boxplot/violin + jitter；
- 不只展示均值和误差条。

输出：

- `DEG_Liver_M_8w_vs_1w_full.csv`
- `DEG_Liver_M_8w_vs_1w_significant.csv`
- `DEG_summary.csv`
- `top_heatmap_proteins.csv`
- `representative_proteins.csv`

---

# Step 6. 功能富集

脚本：`05_functional_enrichment.R`

UP 和 DOWN 必须分开分析。

分别运行：

- GO Biological Process
- KEGG
- 若现有 EasyProtein 已支持且依赖完整，可增加 GSEA

标准：

```r
adj.P.Val <= 0.05
abs(logFC) >= 1
```

对于 ORA：

- UP genes 单独输入；
- DOWN genes 单独输入；
- 背景基因应优先使用实际进入差异分析的可检测蛋白，而不是全部基因组；
- 若 EasyProtein 当前函数不支持自定义 universe，记录这一限制，不要私自更换统计实现。

对于 GSEA：

- 使用完整 ranked list；
- 明确 ranking metric；
- 记录重复 gene symbol 的处理方式。

生成：

- GO_UP.csv
- GO_DOWN.csv
- KEGG_UP.csv
- KEGG_DOWN.csv
- optional GSEA result tables
- publication-ready enrichment dot plots

主图 enrichment plot：

- UP 与 DOWN 分面或左右排列；
- 每个方向 top 5–8 terms；
- x = GeneRatio 或 enrichment ratio；
- point size = Count；
- color = adjusted P value；
- pathway label 不得被截断；
- 不展示高度冗余的多个近义 GO term。

如需去冗余，可基于 semantic similarity 或简单关键词人工规则，但规则必须写入脚本并输出被移除 term 列表。

---

## 8. Main Figure 2

脚本：`06_main_figure.R`

标题建议：

> **Figure 2 | EasyProtein provides an end-to-end workflow for routine quantitative proteomics analysis.**

主图由以下 panel 构成。

### Figure 2A. Dataset and analytical workflow

内容：

- public mouse multi-organ DIA proteomics dataset；
- protein-group quantitative matrix；
- `rawdata2se()`；
- `SummarizedExperiment`；
- assays / rowData / colData；
- preprocessing → QC → differential analysis → enrichment → export。

要求：

- 画成矢量 schematic；
- 不要使用网页截图；
- 不要与 Figure 1 软件总览完全重复；
- 突出本案例中的实际数据流。

### Figure 2B. Protein intensity distribution

优先展示 processed sample intensity density。

如果存在可靠 before/after assays，则展示 before versus after；否则只展示 processed distribution，并在标题中准确描述。

### Figure 2C. Data completeness

组合两个小图：

- missing-value fraction per sample；
- detected proteins per sample。

按 tissue 排序或分组，避免给每个样本使用独立颜色。

### Figure 2D. Sample–sample correlation heatmap

- Spearman correlation；
- annotation = tissue, age, sex；
- publication-level ComplexHeatmap 或等价实现。

### Figure 2E. PCA

- full dataset；
- color = tissue；
- biological replicates 应可识别；
- 轴标题写明 variance explained。

### Figure 2F. Volcano plot

比较：

> Male liver: 8 weeks versus 1 week

- x = log2 fold change；
- y = `-log10(P.Value)` 或 `-log10(adj.P.Val)`，但图注必须准确；
- 标记 cutoff；
- 标注 6–10 个代表性蛋白；
- 标注不能严重重叠。

### Figure 2G. Differential protein heatmap

- top 30–50 differential proteins；
- row z-score；
- sample annotation；
- UP/DOWN row split。

### Figure 2H. Functional interpretation

组合：

- GO/KEGG enrichment dot plot；
- 2–4 个 representative protein expression plots。

最终主图输出：

- `Figure2_case_study1.pdf`
- `Figure2_case_study1.svg`
- `Figure2_case_study1.png`，600 dpi

并保存每个独立 panel：

- `Figure2A.pdf` ... `Figure2H.pdf`

---

## 9. Supplementary Figures

脚本：`07_supplementary_figures.R`

### Supplementary Figure S1. Dataset composition and object construction

建议 panel：

- S1A tissue × age × sex sample composition；
- S1B sample-name/condition parsing schematic；
- S1C sample metadata preview；
- S1D `SummarizedExperiment` structure；
- S1E assay dimensions and missingness summary。

### Supplementary Figure S2. Comprehensive quality control

建议 panel：

- S2A all-sample intensity density；
- S2B intensity boxplots；
- S2C sample missing fraction；
- S2D detected protein count；
- S2E protein-level missingness distribution；
- S2F within-group CV distribution。

### Supplementary Figure S3. Sample relationships

建议 panel：

- S3A PCA colored by tissue；
- S3B PCA colored by age；
- S3C PCA colored by sex；
- S3D UMAP colored by tissue；
- S3E hierarchical clustering dendrogram；
- S3F within-group replicate correlation summary。

UMAP 仅作为补充，不替代 PCA。

### Supplementary Figure S4. Preprocessing and filtering assessment

根据 EasyProtein 实际支持能力选择：

- S4A filtering 前后的蛋白数量；
- S4B protein missingness versus abundance；
- S4C retained proteins under different missingness cutoffs；
- S4D processed matrix distribution；
- S4E normalization/scaling impact；
- S4F filtering flow summary。

不要为了凑 panel 比较 package 当前不支持的 imputation 方法。

### Supplementary Figure S5. Differential-analysis diagnostics

建议 panel：

- S5A MA plot；
- S5B P-value distribution；
- S5C UP/DOWN/NS counts；
- S5D logFC distribution；
- S5E–H representative proteins；
- S5I top-hit effect size and group median comparison。

### Supplementary Figure S6. Functional interpretation and interface consistency

建议 panel：

- S6A GO enrichment, UP；
- S6B GO enrichment, DOWN；
- S6C KEGG enrichment, UP；
- S6D KEGG enrichment, DOWN；
- S6E optional GSEA curves；
- S6F optional STRING/network output；
- S6G R package versus Shiny logFC concordance；
- S6H R package versus Shiny DE classification concordance。

只有在可以从同一输入和相同参数可靠获得 Shiny 输出时才制作 S6G–H。不能使用模拟数据或手工复制结果。

所有 Supplementary Figure 输出：

- PDF
- SVG
- 600-dpi PNG
- 独立 panel 文件

---

## 10. Supplementary Tables

脚本：`08_export_tables.R`

生成一个 Excel 文件：

`CaseStudy1_Supplementary_Tables.xlsx`

至少包含：

### Table S1. Sample metadata

- sample
- condition
- tissue
- age
- sex
- replicate

### Table S2. Data-processing summary

- original protein count
- duplicated features removed
- invalid features removed
- proteins removed by missingness
- retained protein count
- sample count
- assay used for downstream analysis

### Table S3. Full differential-analysis results

- gene/protein ID
- gene symbol
- logFC
- AveExpr, if available
- P.Value
- adj.P.Val
- DEGs_types
- median_1w
- median_8w

### Table S4. Significant differential proteins

### Table S5. GO enrichment results

UP 和 DOWN 分 sheet。

### Table S6. KEGG enrichment results

UP 和 DOWN 分 sheet。

### Table S7. Functions and parameters used

至少包含：

- analysis step
- EasyProtein function
- input assay/object
- key parameters
- output
- corresponding figure panel

Excel sheet 名称不能超过 31 个字符。

---

## 11. Manuscript 数值和结果摘要

脚本：`09_generate_summary.R`

自动生成：

`output/manuscript_values/case_study1_key_numbers.md`

包括：

- total samples；
- total proteins before and after filtering；
- number of tissues；
- overall missing fraction；
- liver male 1w and 8w sample numbers；
- number of UP proteins；
- number of DOWN proteins；
- top differential proteins；
- top enriched GO/KEGG terms；
- PCA variance explained；
- minimum within-group replicate correlation；
- potential outlier summary。

同时生成：

`output/manuscript_values/case_study1_result_draft.md`

写成 3–4 个简短段落：

1. data import and object construction；
2. QC and sample structure；
3. differential analysis；
4. functional interpretation。

只报告由脚本实际计算得到的数字，不要编造结论。

---

## 12. 绘图统一规范

### 12.1 基本主题

建立统一 theme，例如：

```r
theme_case_study <- function(base_size = 8) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      plot.title = element_text(face = "bold", hjust = 0),
      axis.title = element_text(face = "plain"),
      legend.title = element_text(face = "bold"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      panel.border = element_rect(fill = NA, linewidth = 0.4)
    )
}
```

可根据仓库已有主题调整，不要破坏全文视觉一致性。

### 12.2 配色

- tissue 使用固定离散色板；
- age 使用有顺序的色板；
- UP/DOWN/NS 全文保持一致；
- annotation colors 在所有 heatmap 中保持一致；
- 不使用彩虹色板；
- 保证色盲友好和灰度可区分性。

建议：

- UP：暖色；
- DOWN：冷色；
- NS：浅灰；
- 缺失值：浅灰或白色。

### 12.3 字体与输出

- 优先使用通用 sans-serif 字体；
- PDF 中嵌入字体；
- panel label 为粗体 A–H；
- 主图最终文字在单栏/双栏缩放后仍可阅读；
- raster 图 600 dpi；
- heatmap 优先矢量 PDF，但注意文件大小。

### 12.4 图形真实性

- 不删除不理想的样本以改善图形；
- 不对 PCA 坐标进行非线性人工移动；
- 不手动改变 P value；
- 不只挑选支持预期结论的代表性蛋白；
- representative protein 的选择规则必须输出到表格。

---

## 13. 质量控制和自动验证

在 `validation_helpers.R` 中加入自动检查。

至少检查：

```r
stopifnot(!anyDuplicated(colnames(assay(se))))
stopifnot(all(colnames(assay(se)) %in% rownames(colData(se))))
stopifnot(all(c("Liver", "1w", "8w", "M") %in% expected_metadata_values))
stopifnot(ncol(se_de) >= 4)
stopifnot(all(is.finite(deg_res$logFC[!is.na(deg_res$logFC)])))
stopifnot(all(deg_res$adj.P.Val >= 0 & deg_res$adj.P.Val <= 1, na.rm = TRUE))
```

还要检查：

- comparison direction；
- figure input row count；
- heatmap 至少有足够的显著蛋白；
- enrichment 输入基因数量；
- 输出文件是否实际生成；
- 所有 CSV 可重新读入；
- Excel 中所有 sheet 非空或明确标记 no significant result。

当显著蛋白少于预期时，不要放宽阈值来凑图。应：

1. 使用全部显著蛋白；
2. 在图注和结果中准确报告数量；
3. heatmap 可按最显著蛋白展示，但明确标记 selection rule。

---

## 14. `run_all.R`

建立总入口：

```r
source("case_study1/config.R")
source("case_study1/scripts/00_check_environment.R")
source("case_study1/scripts/01_import_and_metadata.R")
source("case_study1/scripts/02_preprocessing_and_qc.R")
source("case_study1/scripts/03_sample_structure.R")
source("case_study1/scripts/04_differential_analysis.R")
source("case_study1/scripts/05_functional_enrichment.R")
source("case_study1/scripts/06_main_figure.R")
source("case_study1/scripts/07_supplementary_figures.R")
source("case_study1/scripts/08_export_tables.R")
source("case_study1/scripts/09_generate_summary.R")
```

要求：

- 每一步打印清楚进度；
- 每一步写日志；
- 已有中间结果可选择安全复用，但默认从头运行；
- 任何关键错误必须停止，并指出具体文件和步骤。

---

## 15. README 内容

`case_study1/README.md` 必须说明：

1. 本案例目标；
2. 数据来源和实际文件路径；
3. 运行环境；
4. 一键运行方式；
5. 主要输出；
6. Figure 2 每个 panel 对应哪个脚本和文件；
7. Supplementary Figure 对应关系；
8. 已知限制；
9. Shiny consistency 是否完成。

---

## 16. 工作顺序

严格按以下顺序执行：

1. 阅读源码和 vignettes；
2. 确认数据对象及 assay；
3. 确认 metadata 值；
4. 完成可重复的数据导入；
5. 完成 QC 和 sample structure；
6. 验证 differential direction；
7. 完成 enrichment；
8. 先保存所有单独 panel；
9. 再拼接主图和补图；
10. 输出表格；
11. 生成 key numbers 和 result draft；
12. 从干净 session 运行 `run_all.R`；
13. 检查所有输出是否存在且非空。

不要一开始就拼主图。先确保每个分析结果和单独 panel 正确。

---

## 17. 最终交付清单

完成后必须提供以下文件：

### 代码

- [ ] `case_study1/run_all.R`
- [ ] `case_study1/config.R`
- [ ] 所有模块化分析脚本
- [ ] figure helper 和 validation helper

### 主图

- [ ] Figure 2 完整 PDF
- [ ] Figure 2 完整 SVG
- [ ] Figure 2 完整 600-dpi PNG
- [ ] Figure 2A–H 独立 panel

### 补充图

- [ ] Supplementary Figures S1–S6
- [ ] 每张图的 PDF、SVG 和 PNG
- [ ] 所有独立 panel

### 表格

- [ ] sample metadata
- [ ] QC metrics
- [ ] filtering summary
- [ ] full differential results
- [ ] significant differential results
- [ ] GO/KEGG/GSEA results
- [ ] combined supplementary Excel

### Manuscript 辅助文件

- [ ] `case_study1_key_numbers.md`
- [ ] `case_study1_result_draft.md`
- [ ] figure-to-script mapping
- [ ] sessionInfo
- [ ] analysis log
- [ ] missing dependencies，若存在

---

## 18. 验收标准

只有满足以下条件才算完成：

1. 从原始公共数据到最终 Figure 2 可通过一个入口脚本重复生成；
2. 所有主图数值均可追溯到输出表；
3. comparison direction 明确且经过验证；
4. metadata 大小写和 factor 顺序一致；
5. 没有二次 log transformation 或重复 normalization；
6. UP 和 DOWN 分开富集；
7. 主图不包含与 Figure 1 重复的 Shiny 截图；
8. UMAP 只放补图；
9. 所有 figure 达到 publication-ready 水平；
10. 所有结果均来自真实数据和实际函数输出；
11. 不存在空 panel、临时文件、绝对个人路径或不可复现的手工步骤；
12. `run_all.R` 在干净 session 中成功运行。

---

## 19. 禁止事项

- 不要修改或覆盖原始公共数据；
- 不要自动安装新软件或 R 包；
- 不要更换差异分析方法，除非现有 `se2DEGs()` 无法运行，并且先清楚记录原因；
- 不要将 UP 和 DOWN 混在一起做主富集分析；
- 不要为了得到更多显著结果随意降低阈值；
- 不要伪造 preprocessing before/after 对比；
- 不要把 UMAP 当作主要质量证据；
- 不要只生成图而不保存底层表格；
- 不要在脚本中使用用户个人电脑的绝对路径；
- 不要声称获得某个生物学机制，除非数据和富集结果明确支持。

---

## 20. 最终总结格式

任务完成后，在 pull request 或工作总结中按以下格式报告：

```text
1. Data used
2. EasyProtein functions reused
3. Metadata corrections made
4. Assay selected for downstream analysis
5. QC summary
6. Differential comparison and direction
7. Number of UP/DOWN proteins
8. Main enriched processes
9. Figures generated
10. Tables generated
11. Missing dependencies or unresolved issues
12. Exact command used to reproduce all results
```

不要只写“分析已完成”。必须列出实际输出路径和关键结果数字。
