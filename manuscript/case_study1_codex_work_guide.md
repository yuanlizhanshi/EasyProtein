# Codex 最终工作指南：EasyProtein Case Study 1 / Figure 2

## 1. 最终任务

基于 EasyProtein 仓库中已经准备好的公共多组织定量蛋白质组数据，完成文章 **Case Study 1** 的最终 Figure 2 和 Supplementary Figures。

Figure 2 的内容已经确定，不再重新设计版式或增加新的分析模块。你的工作是：

1. 使用 EasyProtein 已有函数和 vignette 中已有代码完成分析；
2. 生成 publication-ready 图形；
3. 将 Main Figure 的全部代码放在一个 R 文件中；
4. 将 Supplementary Figures 的全部代码放在另一个 R 文件中；
5. 最终只需要交付 PDF 图；
6. 图形保存优先使用 EasyProtein 的 `saveplot()` 函数。

不要修改原始数据，不要自动安装新的 R 包，不要为了让结果更好看而手工调整统计结果或删除样本。

---

## 2. 开始前必须阅读

按以下顺序阅读并确认现有代码、对象和函数接口：

1. `manuscript/section1_overview.md` 或对应 overview 文件；
2. `vignettes/quick_start.Rmd`；
3. `vignettes/multiple_sample_analysis.Rmd`；
4. `vignettes/complexheatmap_visualization.Rmd`；
5. `R/visualization.R`；
6. 与以下功能有关的 R 源码：
   - `rawdata2se()`
   - PCA
   - ComplexHeatmap
   - tissue-specific gene/protein clustering
   - differential analysis
   - volcano plot
   - gene expression plot
   - GO/KEGG enrichment
   - 两种 enrichment visualization
   - `saveplot()`
7. 仓库中已经生成 `Supplementary_Figure_S2.pdf` 和 `Supplementary_Figure_S3.pdf` 的旧代码、对象和输出文件。

注意：当前 GitHub 已提交版本中可能暂时找不到 `vignettes/complexheatmap_visualization.Rmd`。如果本地工作目录中存在该文件，以本地文件为准；必须先定位并阅读它，不要凭函数名猜测热图和聚类逻辑。

---

## 3. 数据和 metadata

默认使用 vignette 中的公共数据：

```r
system.file(
  "extdata",
  "mouse_multi-organ_DIA_report.pg_matrix.tsv.gz",
  package = "EasyProtein"
)
```

使用 `rawdata2se()` 构建 `SummarizedExperiment`。

必须检查并统一 metadata：

```r
se$tissue <- stringr::str_to_title(se$tissue)
se$sex <- stringr::str_to_upper(se$sex)
se$age <- factor(se$age, levels = c("1w", "4w", "8w"))
```

检查并修正可能存在的：

- `live` / `liver` / `Liver`；
- `heart` / `Heart`；
- `M` / `m`；
- 样本顺序与 `colData(se)` 不一致；
- 重复 sample name；
- 无法解析 tissue、age、sex 或 replicate 的样本。

不要重复执行已经由 `rawdata2se()` 完成的 log transformation、normalization 或 imputation。必须先读源码确认 assay 含义，再选择用于 PCA、heatmap 和差异分析的 assay。

---

# 4. 代码文件结构

最终只保留两个主分析脚本：

```text
case_study1/
├── Figure2_main.R
├── Figure2_supplementary.R
├── output/
│   ├── Figure2.pdf
│   ├── Supplementary_Figure_S2.pdf
│   ├── Supplementary_Figure_S3.pdf
│   ├── Supplementary_Figure_S4.pdf
│   └── Supplementary_Figure_S5.pdf
└── intermediate/
    ├── se_processed.rds
    ├── tissue_specific_genes.csv
    ├── tissue_specific_GO.csv
    ├── tissue_specific_KEGG.csv
    └── tissue_DEG_summary.csv
```

允许保存必要的中间对象和结果表用于复现，但最终交付重点是上述 PDF。

- `Figure2_main.R`：只负责 Main Figure 2A–D；
- `Figure2_supplementary.R`：负责所有补充图；
- 不要拆成大量零散脚本；
- 两个文件都应能从项目根目录直接运行。

---

# 5. Main Figure 2 的最终结构

Figure 2 只包含四个 panel：A–D。

## Figure 2A. EasyProtein workflow

这一部分已经完成。

要求：

1. 优先直接复用已经生成的 workflow panel；
2. 不重新设计内容；
3. 检查字体、尺寸、panel label 和最终拼图是否协调；
4. 如果 workflow 是 PDF/SVG，尽量以矢量形式导入；
5. 不要把 workflow 截成低分辨率位图。

该 panel 表达：

```text
protein quantitative matrix
→ SummarizedExperiment
→ preprocessing / QC
→ visualization
→ clustering
→ enrichment
→ differential analysis
```

---

## Figure 2B. PCA of all samples colored by tissue

### 分析要求

使用全部样本进行 PCA：

- 输入使用 EasyProtein 推荐的 processed/concentration assay；
- 删除全 NA 和零方差 feature；
- PCA 基于样本，即对 expression matrix 转置；
- color by `tissue`；
- 不按 age 或 sex 分面；
- 不需要为所有样本添加文字标签；
- PC1 和 PC2 标注 variance explained；
- 使用 EasyProtein 内置 `plot_pca()`，如需 publication-level 微调，可在其返回对象上继续加 theme。

### 图形目标

清楚显示：

- 不同组织在蛋白质组层面形成明显分离；
- 同一组织样本总体聚集；
- 图例中的 tissue 顺序和颜色固定。

不要删除 PCA 中位置不理想的样本。

---

## Figure 2C. Heatmap of all samples and tissue-specific protein clusters

参考：

```text
vignettes/complexheatmap_visualization.Rmd
```

### 核心目标

使用全部样本绘制表达热图，并通过聚类识别每个组织的特异性高表达蛋白/基因模块。

### 必须完成的步骤

1. 使用与 vignette 一致的 assay 和 scaling 方法；
2. 根据需要先计算每个 tissue 的平均表达，或按照 vignette 的既定方式处理全部样本；
3. 对 feature 进行 row-wise z-score；
4. 对 rows 开启 clustering；
5. 对 columns 进行合理排序或 clustering；
6. column annotation 使用 tissue；
7. 每个 tissue 使用与 PCA 完全一致的颜色；
8. 从聚类结果中提取 tissue-specific gene/protein clusters；
9. 将每个 cluster 分配给其最特异的 tissue；
10. 输出每个 tissue 对应的特异性 gene/protein list。

### tissue-specific cluster 的定义

优先完全复用 `complexheatmap_visualization.Rmd` 中已经使用的方法。

如果 vignette 只绘图但没有显式输出 cluster，则采用以下可重复规则：

1. 在 tissue-level mean expression matrix 上进行 row z-score；
2. 对蛋白进行层次聚类或 vignette 中使用的聚类；
3. 将蛋白切分为若干 cluster；
4. 对每个 cluster 计算各 tissue 的平均 z-score；
5. 将 cluster 指派给平均 z-score 最高的 tissue；
6. 只有当该 tissue 的平均 z-score 明显高于其他 tissue 时，才称为 tissue-specific；
7. 输出 cluster size 和 specificity score。

不要为每个 tissue 强行制造一个 cluster。若某个组织没有稳定特异模块，应如实记录。

### Figure 2C 的表现形式

建议：

- columns：全部样本；
- column split 或顶部 annotation：tissue；
- rows：选定的 tissue-specific proteins；
- row split：对应 tissue-specific cluster；
- heatmap color：z-score；
- 显示每个 cluster 对应的 tissue 名称；
- 不显示全部 row names，避免拥挤；
- 可在每个 cluster 旁标注 feature 数量。

必须保存：

```text
case_study1/intermediate/tissue_specific_genes.csv
```

至少包含：

- gene/protein identifier
- gene symbol
- cluster
- assigned tissue
- tissue mean expression
- tissue mean z-score
- specificity score

---

## Figure 2D. GO enrichment of tissue-specific clusters

对 Figure 2C 中识别的每个 tissue-specific gene/protein set 分别做 GO enrichment。

### 分析要求

1. 每个 tissue 单独富集；
2. Main Figure 只展示 GO Biological Process；
3. species 使用 mouse；
4. key type 根据实际 gene identifier 设置；
5. 使用 EasyProtein 内置 `enrichment_analysis()` 或 package 中对应函数；
6. background/universe 如函数支持，应使用进入 heatmap/clustering 的可检测蛋白；
7. 不将所有 tissue-specific genes 混合后统一富集；
8. 无显著 term 的 tissue 如实标记，不要放宽阈值凑结果。

建议阈值：

```r
p.adjust <= 0.05
qvalue <= 0.05
```

每个 tissue 保留 top 3–5 个非冗余 GO terms。

### 两种 enrichment visualization

EasyProtein 包中存在两种富集结果可视化代码。必须：

1. 找到这两种可视化函数或 vignette 示例；
2. 对同一 GO enrichment result 分别画两版；
3. 比较哪一种更适合 Main Figure；
4. 最终 Figure 2D 选择信息密度更高、组织间更容易比较的一版；
5. 另一版可以作为测试输出保留，但不必进入最终 Supplementary Figure，除非版面合适。

优先考虑的最终形式：

- tissue × GO term dot plot；或
- 每个 tissue 的 enrichment bar/dot plot 组合。

必须确保：

- tissue 顺序与 Figure 2B/C 一致；
- GO term 标签可读；
- point size、颜色和横轴含义明确；
- 不显示大量高度重复 term。

保存：

```text
case_study1/intermediate/tissue_specific_GO.csv
```

---

# 6. Main Figure 拼图

在 `Figure2_main.R` 中完成：

1. 读取/生成 Figure 2A；
2. 生成 Figure 2B；
3. 生成 Figure 2C；
4. 生成 Figure 2D 的两种候选版本；
5. 选择最终 D；
6. 拼接 A–D；
7. 添加统一 panel labels；
8. 使用 `saveplot()` 保存最终图。

推荐布局：

```text
A ─────────────────────────
B ────────── C ─────────────
D ─────────────────────────
```

或者根据实际图形比例采用：

```text
A        B
C        D
```

热图通常需要较大空间，因此优先确保 Figure 2C 可读，不要为了整齐强行压缩。

最终文件名必须为：

```text
Figure2.pdf
```

使用：

```r
saveplot(
  object = Figure2,
  filenames = "Figure2",
  width = ..., 
  height = ...,
  dpi = 600
)
```

`saveplot()` 会自动写入 `Fig/PDF/`。运行后将最终 PDF 复制到：

```text
case_study1/output/Figure2.pdf
```

用户最终只需要 PDF，不需要整理或提交 PNG/TIFF。

---

# 7. Supplementary Figures

所有补充图代码写在：

```text
case_study1/Figure2_supplementary.R
```

最终补充图编号如下。

---

## Supplementary Figure S2

仓库中已经做过：

```text
Supplementary_Figure_S2.pdf
```

任务：重新运行并完整复现一次。

要求：

1. 找到生成该图的原始代码；
2. 不改变该图的分析内容；
3. 使用当前数据对象和当前 package 版本重新生成；
4. 检查 panel、字体、图例、尺寸和输出是否完整；
5. 使用 `saveplot()` 保存；
6. 最终文件名保持：

```text
Supplementary_Figure_S2.pdf
```

不要重新设计 S2，除非原代码无法运行；如需修复，只修复错误和兼容性问题。

---

## Supplementary Figure S3

仓库中已经做过：

```text
Supplementary_Figure_S3.pdf
```

任务与 S2 相同：定位原代码并重新运行复现。

要求：

- 不改变原分析逻辑；
- 使用当前数据和当前 EasyProtein 版本；
- 使用 `saveplot()`；
- 最终输出：

```text
Supplementary_Figure_S3.pdf
```

---

## Supplementary Figure S4. Differential proteomics across tissues: 1w versus 8w

### S4A. Number of differential proteins in each tissue

对每个组织分别比较：

```text
8w versus 1w
```

尽可能保持 sex 一致。优先方案：

- 若每个 tissue 的 male 样本数量足够，固定 `sex == "M"`；
- 如果数据设计更适合合并 sex，必须在模型中考虑 sex，或明确说明合并规则；
- 不允许在不同 tissue 中使用不同的比较设计。

优先使用统一简单阈值：

```r
adj.P.Val <= 0.05
abs(logFC) >= 1
```

如果由于样本数或数据特征导致绝大多数 tissue 没有 DEG，可以使用：

```r
P.Value <= 0.05
abs(logFC) >= 1
```

但只能统一更改一次，并必须在图注和代码中明确记录。不要为每个 tissue 单独调整阈值。

使用 EasyProtein 内置 differential analysis 函数，例如：

```r
se2DEGs(
  se = se_tissue,
  compare_col = "age",
  ref = "1w",
  cmp = "8w",
  logFC_cutoff = 1,
  adj_p_cutoff = 0.05
)
```

生成一个统计图比较每个 tissue 的 DEG 数量：

- x = tissue；
- y = number of differential proteins；
- UP 和 DOWN 分开显示；
- tissue 顺序与 Main Figure 一致；
- 可使用 stacked bar 或 grouped bar；
- 图中标注每个组织的总 DEG 数量。

输出完整汇总：

```text
case_study1/intermediate/tissue_DEG_summary.csv
```

至少包含：

- tissue
- n_samples_1w
- n_samples_8w
- n_UP
- n_DOWN
- n_total_DEG
- threshold

### S4B. Representative volcano plot

从所有 tissue 中选择一个代表组织绘制 volcano plot。

选择规则：

1. 样本数充足；
2. DEG 数量适中；
3. UP 和 DOWN 均存在；
4. 图形信息量高；
5. 不要仅因为某个组织最符合预期就挑选。

优先选择 DEG 总数较多且统计结构正常的 tissue。

必须使用 EasyProtein 内置 volcano plot 函数。开始前找到实际函数名和参数，不要自己重新写 volcano plot 替代 package 功能。

标题必须明确：

```text
<Tissue>: 8 weeks versus 1 week
```

图中标注少量 top proteins，避免标签拥挤。

### S4C. Representative gene/protein expression plot

从 S4B 的 volcano plot 中选择一个代表性差异蛋白：

- adjusted P value 显著；
- absolute logFC 较大；
- 在多数样本中有可靠定量值；
- 尽量避免主要由单个 outlier 驱动。

使用 EasyProtein 内置：

```r
plot_gene_expression()
```

在同一个 tissue 中展示该 gene/protein 在 1w 和 8w 的表达量。

目的：验证 volcano plot 中的差异方向与原始样本表达一致。

图中必须展示 biological replicates；如内置函数只画 boxplot，可在其返回对象上增加 jitter，但不要重新计算表达量。

### S4 最终布局

```text
A. DEG counts across tissues
B. Representative volcano plot
C. Representative gene expression
```

最终输出：

```text
Supplementary_Figure_S4.pdf
```

---

## Supplementary Figure S5. KEGG enrichment of tissue-specific clusters

使用 Main Figure 2C 中同一批 tissue-specific gene/protein sets。

Main Figure 2D 已经完成 GO enrichment，因此 Supplementary Figure S5 只做 KEGG enrichment。

要求：

1. 每个 tissue 单独运行 KEGG；
2. gene set 与 Main Figure 2D 完全一致；
3. 不重新聚类或重新筛选一套 tissue-specific genes；
4. species = mouse；
5. 使用 EasyProtein 内置 enrichment function；
6. 使用 package 中适合跨组织比较的 enrichment visualization；
7. 每个 tissue 展示 top 3–5 pathways；
8. 没有显著 KEGG pathway 的 tissue 如实标记；
9. 不放宽阈值来强行填满所有 tissue。

建议阈值：

```r
p.adjust <= 0.05
qvalue <= 0.05
```

若 KEGG 结果普遍较少，可以保留 nominally significant pathways 作为 exploratory result，但必须：

- 使用统一阈值；
- 在标题或图注中明确；
- 不与 FDR-significant 结果混淆。

最终输出：

```text
Supplementary_Figure_S5.pdf
```

同时保存：

```text
case_study1/intermediate/tissue_specific_KEGG.csv
```

---

# 8. 两个 R 文件的具体责任

## `Figure2_main.R`

该文件必须完整包含或调用：

1. package loading；
2. data loading；
3. metadata correction；
4. assay selection；
5. Figure 2A workflow loading；
6. all-sample PCA；
7. all-sample ComplexHeatmap；
8. tissue-specific cluster extraction；
9. GO enrichment；
10. 两种 GO visualization；
11. Figure 2 assembly；
12. `saveplot()`；
13. 将最终 PDF 复制到 `case_study1/output/Figure2.pdf`。

## `Figure2_supplementary.R`

该文件必须完整包含或调用：

1. 读取 Main Figure 产生的 `se_processed.rds`；
2. 复现 Supplementary Figure S2；
3. 复现 Supplementary Figure S3；
4. 每个 tissue 的 1w versus 8w differential analysis；
5. DEG count summary；
6. 内置 volcano plot；
7. 内置 gene expression plot；
8. tissue-specific KEGG enrichment；
9. S4 和 S5 的拼图；
10. 使用 `saveplot()` 保存全部 Supplementary PDFs。

两个文件中不要复制一大段相同的数据预处理代码。Supplementary 脚本优先读取 Main 脚本保存的标准对象和 tissue-specific gene list。

---

# 9. `saveplot()` 使用要求

EasyProtein 的 `saveplot()` 当前调用形式为：

```r
saveplot(
  object = plot_object,
  filenames = "Figure_name",
  width = 6,
  height = 4,
  dpi = 600
)
```

该函数会自动创建：

```text
Fig/PDF/
Fig/TIFF/
Fig/PNG/
```

本任务最终只整理和交付 PDF：

```text
Fig/PDF/Figure2.pdf
Fig/PDF/Supplementary_Figure_S2.pdf
Fig/PDF/Supplementary_Figure_S3.pdf
Fig/PDF/Supplementary_Figure_S4.pdf
Fig/PDF/Supplementary_Figure_S5.pdf
```

运行完成后，将 PDF 复制到：

```text
case_study1/output/
```

不要手工在 Illustrator 中修改统计图。若版式需要调整，回到 R 代码中修改并重新导出。

注意 `saveplot()` 内部会将 PDF width 和 height 乘以 2，因此传入尺寸时先用小规模测试图确认最终页面比例，避免输出过大或字体过小。

---

# 10. 统一绘图规范

## Tissue colors

PCA、heatmap annotation、GO、KEGG 和 DEG count 中，同一 tissue 必须使用完全相同颜色。

建立一个命名向量：

```r
tissue_colors <- c(
  "Tissue1" = "...",
  "Tissue2" = "..."
)
```

不要在不同 panel 中重新自动生成颜色。

## DEG colors

全文统一：

- UP：红/暖色；
- DOWN：蓝/冷色；
- NS：灰色。

## Text and labels

- 使用通用 sans-serif 字体；
- panel labels 为粗体大写；
- pathway 名称不能被裁切；
- 不在 PCA 中标全部样本；
- 不在 heatmap 中显示数千个 row names；
- volcano 只标注少量 top proteins。

## Statistical transparency

所有 panel 的代码附近必须注释：

- assay；
- transformation/scaling；
- comparison direction；
- DEG threshold；
- enrichment threshold；
- clustering method；
- tissue-specific assignment rule。

---

# 11. 必须进行的验证

在生成图之前执行：

```r
stopifnot(!anyDuplicated(colnames(SummarizedExperiment::assay(se))))
stopifnot(all(colnames(SummarizedExperiment::assay(se)) == rownames(SummarizedExperiment::colData(se))))
stopifnot(!anyNA(se$tissue))
stopifnot(all(c("1w", "8w") %in% as.character(unique(se$age))))
```

此外检查：

1. PCA 输入无 Inf；
2. heatmap 输入无全 NA row；
3. row z-score 后无 NaN；
4. 每个 tissue-specific cluster 有明确 gene list；
5. GO/KEGG 输入 ID 类型正确；
6. `8w versus 1w` 中 `logFC > 0` 的方向经过一个代表 gene 的原始表达验证；
7. volcano plot 使用的 DEG table 与 DEG count 使用同一阈值；
8. Supplementary expression plot 的 gene 与 volcano 中方向一致；
9. S5 使用的 gene sets 与 Main Figure D 完全一致；
10. 所有 PDF 均存在且文件大小大于 0。

不要在代码中静默跳过错误。如果某个 tissue 样本数不足，应在 DEG summary 中标为 `not_tested`，而不是返回空结果后继续当作 0 DEG。

---

# 12. 最终交付文件

最终只需要整理以下 PDF：

```text
case_study1/output/Figure2.pdf
case_study1/output/Supplementary_Figure_S2.pdf
case_study1/output/Supplementary_Figure_S3.pdf
case_study1/output/Supplementary_Figure_S4.pdf
case_study1/output/Supplementary_Figure_S5.pdf
```

以及两个可重复运行的代码文件：

```text
case_study1/Figure2_main.R
case_study1/Figure2_supplementary.R
```

同时保留下列中间结果以便检查：

```text
case_study1/intermediate/se_processed.rds
case_study1/intermediate/tissue_specific_genes.csv
case_study1/intermediate/tissue_specific_GO.csv
case_study1/intermediate/tissue_specific_KEGG.csv
case_study1/intermediate/tissue_DEG_summary.csv
```

---

# 13. 完成标准

只有满足以下条件才算完成：

1. Figure 2 严格包含 A workflow、B PCA、C tissue-specific heatmap、D GO enrichment；
2. PCA 使用全部样本并按 tissue 着色；
3. heatmap 使用全部样本，开启聚类，并输出每个 tissue 的特异性 gene/protein；
4. Figure 2D 对 C 中每个 tissue-specific set 分别做 GO enrichment；
5. EasyProtein 的两种 enrichment visualization 都实际生成并比较；
6. S2 和 S3 从已有代码重新完整复现；
7. S4 比较所有可分析 tissue 的 1w versus 8w DEG 数量；
8. S4 包含一个 EasyProtein 内置 volcano plot；
9. S4 包含一个 EasyProtein 内置 gene expression plot验证 volcano 结果；
10. S5 对 Main Figure 的同一组 tissue-specific genes 做 KEGG enrichment；
11. Main Figure 代码只有一个文件；
12. Supplementary Figure 代码只有一个文件；
13. 最终所有图均为 PDF；
14. 所有最终 PDF 由 R 代码和 `saveplot()` 可重复生成；
15. 没有手工修改图形、伪造结果、按组织调整不同阈值或删除不理想样本。

---

# 14. 完成后的汇报格式

完成后只需报告：

```text
1. Figure2_main.R path
2. Figure2_supplementary.R path
3. Figure2.pdf path
4. Supplementary Figure S2–S5 PDF paths
5. Assay used
6. Tissue-specific clustering method
7. Number of tissue-specific genes per tissue
8. DEG threshold used for S4
9. Representative tissue and gene used in S4
10. GO and KEGG enrichment thresholds
11. Any tissue that could not be tested and the reason
12. Any unresolved error or missing dependency
```

不要只回复“已经完成”。必须给出实际文件路径和关键分析设置。