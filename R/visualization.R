#' Plot gene expression across samples or conditions
#'
#' This function extracts a given gene from a SummarizedExperiment object,
#' merges its expression values with sample metadata, and visualizes the
#' distribution using boxplots (grouped mode) or barplots (single-sample mode).


#' @param se A SummarizedExperiment object.
#' @param gene Gene name to plot.
#' @param by Column in colData(se) used for grouping (default: "condition").
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_col labs theme element_text ggtitle
#' @importFrom dplyr filter left_join select
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_detect
#'
#' @export
plot_gene_expression <- function(se, gene, by = "condition") {

  conc_mean_df <- se2conc(se) %>%
    dplyr::select(-Genes) %>%
    tidyr::pivot_longer(cols = -Protein.Ids,
                        names_to = "sample",
                        values_to = "conc") %>%
    dplyr::left_join(as.data.frame(se@colData), by = "sample") %>%
    dplyr::left_join(as.data.frame(rowData(se)), by = "Protein.Ids")

  gene_name <- paste0("^", gene, "$")

  target_gene <- conc_mean_df %>%
    dplyr::filter(stringr::str_detect(Genes, gene_name))

  if (!by %in% colnames(se@colData)) {
    stop(paste0("Column '", by, "' not found in colData(se)."))
  }

  target_gene[[by]] <- factor(
    target_gene[[by]],
    levels = unique(se@colData[[by]])
  )

  # grouped by condition
  if (length(unique(se@colData[[by]])) < ncol(se)) {
    p1 <- ggplot(target_gene, aes(x = .data[[by]], y = conc)) +
      geom_boxplot(outlier.size = 1, width = 0.3) +
      labs(x = NULL, y = "Expression") +
      ggtitle(gene) +
      theme_test() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold")
      )
  } else {
    # per-sample barplot
    p1 <- ggplot(target_gene, aes(x = sample, y = conc)) +
      geom_col() +
      labs(x = NULL, y = "Expression") +
      ggtitle(gene) +
      theme_test() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold")
      )
  }
  return(p1)
}

#' Plot PCA embedding
#'
#' This function visualizes PCA components stored in pca.df and pca.res.
#' Labels can be optionally added using ggrepel.
#'
#' @param pca.df A data frame containing Dim.1 and Dim.2 columns.
#' @param pca.res PCA result object with eigenvalues (eig slot).
#' @param colorby Column name used for coloring.
#' @param label Column name used for labeling points.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_point labs theme geom_point
#' @importFrom ggrepel geom_text_repel
#'
#' @export
plot_pca <- function(pca.df, pca.res, colorby, label) {

  base_plot <- ggplot(pca.df, aes(x = Dim.1, y = Dim.2)) +
    geom_point(aes_string(color = colorby), size = rel(2.5)) +
    labs(
      x = paste0("PC1 ", round(pca.res$eig[1, 2], 2), "%"),
      y = paste0("PC2 ", round(pca.res$eig[2, 2], 2), "%"),
      color = NULL
    ) +
    theme_test() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold")
    )

  if (label == "NULL") {
    return(base_plot)
  } else {
    return(
      base_plot +
        ggrepel::geom_text_repel(aes_string(label = label))
    )
  }
}

#' Plot intensity distribution for each sample
#'
#' This function draws violin + boxplot of log2 intensity for each sample
#' from a SummarizedExperiment object.
#'
#' @param se A SummarizedExperiment object.
#'
#' @return A ggplot violin plot.
#'
#' @importFrom ggplot2 ggplot aes geom_violin geom_boxplot scale_x_log10 labs theme
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter mutate
#'
#' @export
plotSE_density <- function(se){
  rawdata <- se2raw(se)
  rawdata_df <- rawdata %>%
    pivot_longer(cols = 3:ncol(rawdata),
                 names_to = 'Sample',
                 values_to = 'intersity') %>%
    dplyr::filter(!is.na(intersity)) %>%
    mutate(log2intersity = log2(intersity))

  rawdata_df$Sample <- factor(rawdata_df$Sample, levels = unique(colnames(se)))

  ggplot(rawdata_df, aes(x = log2intersity, y = Sample, fill = Sample)) +
    geom_violin() +
    geom_boxplot(width = 0.2, outlier.size = 0.5) +
    labs(x = "log2 intensity", y = NULL) +
    guides(fill = "none") +
    theme_test() +
    theme(
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold")
    )
}


#' Plot missing values per sample
#'
#' @param se A SummarizedExperiment object.
#'
#' @return A ggplot object summarizing missing count per sample.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_segment geom_text labs theme
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter group_by mutate select distinct
#'
#' @export
plotSE_missing_value <- function(se){
  rawdata <- se2raw(se)

  missing_df <- rawdata %>%
    pivot_longer(cols = 3:ncol(rawdata),
                 names_to = 'Sample',
                 values_to = 'intersity') %>%
    dplyr::filter(is.na(intersity)) %>%
    group_by(Sample) %>%
    mutate(missing_number = n()) %>%
    dplyr::select(Sample, missing_number) %>%
    distinct()

  missing_df$Sample <- factor(missing_df$Sample, levels = unique(colnames(se)))

  ggplot(missing_df, aes(x = missing_number, y = Sample)) +
    geom_segment(aes(x = 0, xend = missing_number), color = "grey60") +
    geom_point(size = 4) +
    geom_text(aes(label = missing_number, hjust = -0.5)) +
    labs(x = "Number of missing values", y = NULL) +
    theme_test() +
    theme(
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold")
    )
}


#' Plot missing values per sample
#'
#' @param se A SummarizedExperiment object.
#'
#' @return A ggplot object summarizing missing count per sample.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_segment geom_text labs theme
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter group_by mutate select distinct
#'
#' @export
plotSE_missing_value <- function(se){
  rawdata <- se2raw(se)

  missing_df <- rawdata %>%
    pivot_longer(cols = 3:ncol(rawdata),
                 names_to = 'Sample',
                 values_to = 'intersity') %>%
    dplyr::filter(is.na(intersity)) %>%
    group_by(Sample) %>%
    mutate(missing_number = n()) %>%
    dplyr::select(Sample, missing_number) %>%
    distinct()

  missing_df$Sample <- factor(missing_df$Sample, levels = unique(colnames(se)))

  ggplot(missing_df, aes(x = missing_number, y = Sample)) +
    geom_segment(aes(x = 0, xend = missing_number), color = "grey60") +
    geom_point(size = 4) +
    geom_text(aes(label = missing_number, hjust = -0.5)) +
    labs(x = "Number of missing values", y = NULL) +
    theme_test() +
    theme(
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold")
    )
}

#' Plot number of detected proteins per sample
#'
#' @param se A SummarizedExperiment object.
#'
#' @return A ggplot object.
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter group_by mutate select distinct
#'
#' @export
plotSE_protein_number <- function(se){
  rawdata <- se2raw(se)

  df <- rawdata %>%
    pivot_longer(cols = 3:ncol(rawdata),
                 names_to = "Sample",
                 values_to = "intersity") %>%
    dplyr::filter(!is.na(intersity)) %>%
    group_by(Sample) %>%
    mutate(protein_number = n()) %>%
    dplyr::select(Sample, protein_number) %>%
    distinct()

  df$Sample <- factor(df$Sample, levels = unique(colnames(se)))

  ggplot(df, aes(x = protein_number, y = Sample)) +
    geom_segment(aes(x = 0, xend = protein_number), color = "grey60") +
    geom_point(size = 4) +
    geom_text(aes(label = protein_number, hjust = -0.5)) +
    labs(x = "Number of detected proteins", y = NULL) +
    theme_test()
}


#' Plot CV distribution across conditions
#'
#' @param se A SummarizedExperiment object.
#'
#' @return A ggplot object or NULL if no replicates.
#'
#' @importFrom ggplot2 ggplot aes geom_violin geom_boxplot scale_x_log10 labs theme
#' @importFrom dplyr filter mutate
#'
#' @export
plotCV_density <- function(se){
  cv_df <- calc_gene_CV_by_condition(se)

  if (is.null(cv_df)) {
    message("No replicates found.")
    return(NULL)
  }

  cv_df$condition <- factor(cv_df$condition, levels = unique(cv_df$condition))

  ggplot(cv_df, aes(CV, condition, fill = condition)) +
    geom_violin() +
    geom_boxplot(width = 0.2, outlier.size = 0.5) +
    scale_x_log10() +
    labs(x = "CV", y = NULL) +
    theme_test()
}



#' Interactive or static volcano plot
#'
#' This function creates a volcano plot with optional interactivity via ggiraph.
#' Code from https://github.com/YuLab-SMU/ivolcano
#'
#' @param data Input data frame.
#' @param logFC_col Column name storing log fold change.
#' @param pval_col Column storing adjusted P value.
#' @param gene_col Column storing gene names.
#' @param title Plot title.
#' @param interactive Whether to use ggiraph.
#' @param onclick_fun Optional function for onclick events.
#' @param pval_cutoff FDR threshold.
#' @param logFC_cutoff LogFC threshold.
#' @param top_n Number of labels to draw.
#' @param label_mode Label mode.
#' @param fontface Label font face.
#' @param label_sig_only Whether to only label significant genes.
#' @param threshold_line Line settings for cutoff.
#' @param sig_colors Colors for Up/Down/Not_Significant.
#' @param size_by Optional scaling variable.
#'
#' @return A list with static ggplot object and interactive girafe object.
#'
#' @importFrom ggplot2 ggplot aes geom_point labs theme geom_hline geom_vline scale_color_manual
#' @importFrom dplyr mutate case_when filter slice_min group_by
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggiraph geom_point_interactive girafe
#'
#' @export
ivolcano <- function(
    data,
    logFC_col = "logFC",
    pval_col = "adj.P.Val",
    gene_col = "gene",
    title = "",
    interactive = TRUE,
    onclick_fun = NULL,
    pval_cutoff = 0.05,
    logFC_cutoff = 1,
    top_n = 10,
    label_mode = "separate",
    fontface = "italic",
    label_sig_only = TRUE,
    threshold_line = list(color = "black", linetype = "dashed", linewidth = 0.5),
    sig_colors = c(Up = "red", Down = "blue", Not_Significant = "grey70"),
    size_by = "none"
) {
  stopifnot(all(c(logFC_col, pval_col, gene_col) %in% colnames(data)))
  label_mode <- match.arg(label_mode, c("all", "separate"))
  size_by <- match.arg(size_by, c("none", "negLogP", "absLogFC"))

  # prepare data
  df <- data[!is.na(data[[logFC_col]]) & !is.na(data[[pval_col]]), ]

  df <- df |>
    dplyr::mutate(
      negLogP = -log10(!!sym(pval_col)),
      sig = dplyr::case_when(
        !!sym(pval_col) < pval_cutoff & !!sym(logFC_col) > logFC_cutoff ~ "Up",
        !!sym(pval_col) < pval_cutoff & !!sym(logFC_col) < -logFC_cutoff ~
          "Down",
        TRUE ~ "Not_Significant"
      )
    )
  df$sig <- factor(df$sig, levels = c("Up", "Down", "Not_Significant"))
  df$tooltip <- sprintf(
    "Gene: %s\nlogFC: %s\nadj.P.val: %s",
    df[[gene_col]],
    signif(df[[logFC_col]], 3),
    signif(df[[pval_col]], 3)
  )

  # onclick
  if (!is.null(onclick_fun)) {
    if (is.function(onclick_fun)) {
      df$onclick <- vapply(
        df[[gene_col]],
        function(g) {
          x <- onclick_fun(g)
          if (is.na(x) || !nzchar(x)) "" else as.character(x)
        },
        character(1)
      )
    } else {
      stop("onclick_fun must be a function or NULL")
    }
  } else {
    df$onclick <- ""
  }
  aes_args <- aes(
    x = !!sym(logFC_col),
    y = !!sym("negLogP"),
    color = !!sym("sig"),
    tooltip = paste0(
      gene_col,
      ": ",
      !!sym(gene_col),
      "\nlogFC: ",
      round(!!sym(logFC_col), 2),
      "\nFDR: ",
      signif(!!sym(pval_col), 3)
    ),
    data_id = !!sym(gene_col),
    onclick = !!sym("onclick")
  )
  if (!is.null(size_by) && size_by %in% names(df)) {
    aes_args$size <- df[[size_by]]
  }


  sig_counts <- table(df$sig)

  for (nm in names(sig_colors)) {
    if (!nm %in% names(sig_counts)) sig_counts[nm] <- 0
  }
  sig_counts <- sig_counts[names(sig_colors)]  # 保证顺序一致
  sig_labels <- sprintf("%s (n=%d)", names(sig_colors), sig_counts)

  p <- ggplot(df, aes_args) +
    {
      if (interactive) {
        ggiraph::geom_point_interactive(alpha = 0.7)
      } else {
        geom_point(alpha = 0.7)
      }
    } +
    scale_color_manual(
      values = sig_colors,
      labels = sig_labels,     # ✅ 图例标签带数量
      name = "Significance"    # 图例标题
    ) +
    labs(title = title, x = "log2 Fold Change", y = "-log10(FDR)") +
    theme_minimal()

  # threshold line
  p <- p +
    geom_hline(
      yintercept = -log10(pval_cutoff),
      color = threshold_line$color,
      linetype = threshold_line$linetype,
      linewidth = threshold_line$linewidth
    ) +
    geom_vline(
      xintercept = c(-logFC_cutoff, logFC_cutoff),
      color = threshold_line$color,
      linetype = threshold_line$linetype,
      linewidth = threshold_line$linewidth
    )

  # label topN genes
  if (top_n > 0) {
    df_label <- df
    if (label_sig_only) {
      df_label <- dplyr::filter(df, .data$sig != "Not_Significant")
    }
    if (label_mode == "separate") {
      df_label <- dplyr::group_by(df_label, sign(!!sym(logFC_col)))
    }
    df_label <- dplyr::slice_min(
      df_label,
      order_by = !!sym(pval_col),
      n = top_n
    )

    df_label$label <- df_label[[gene_col]]

    p <- p +
      ggrepel::geom_text_repel(
        data = df_label,
        aes(label = !!sym("label"), fontface = fontface),
        box.padding = 0.3,
        max.overlaps = 20
      )
  }
  return(
    list(
      ggiraph_obj =   ggiraph::girafe(ggobj = p, options = list(ggiraph::opts_hover(css = "fill:black;r:6"))),
      ggobj =  p
    )

  )

}

#' Dot plot for GO enrichment (style 1)
#'
#' This function takes a GO enrichment result (e.g. clusterProfiler output)
#' and visualizes the top enriched categories using -log10(p.adjust).
#'
#' @param GO_df A GO enrichment result data frame.
#' @param topn Number of categories to show.
#' @param label_format Width for wrapped text labels.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 labs theme element_text scale_x_discrete
#' @importFrom dplyr arrange slice_head mutate
#' @importFrom stringr str_wrap
#' @importFrom ggpubr ggdotchart
#'
#' @export
plot_GO_dot1 <- function(GO_df, topn = 10, label_format = 30){
  go_res <- GO_df %>%
    arrange(p.adjust) %>%
    slice_head(n = topn) %>%
    mutate(neg10_p = -log10(p.adjust))

  ggdotchart(
    go_res,
    x = "Description",
    y = "neg10_p",
    color = "Description",
    sorting = "descending",
    palette = if (exists("cols")) cols else NULL,
    add = "segments",
    rotate = TRUE,
    dot.size = 3,
    ggtheme = theme_test()
  ) +
    labs(x = NULL, y = "-log10 adjusted p value") +
    guides(color = "none") +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = label_format)) +
    theme(
      axis.text.x  = element_text(size = 12, face = "bold"),
      axis.text.y  = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold")
    )
}


#' Dot plot for GO enrichment (style 2)
#'
#' This visualization computes GeneRatio and displays enriched GO terms
#' using a colored dot plot.
#'
#' @param GO_df A GO enrichment result data frame.
#' @param topn Number of GO terms.
#' @param label_format Width for wrapped labels.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_point labs scale_color_gradient theme
#' @importFrom stringr str_extract str_wrap
#' @importFrom dplyr arrange slice_head mutate
#'
#' @export
plot_GO_dot2 <- function(GO_df, topn = 10, label_format = 30){
  go_res2 <- GO_df %>%
    arrange(p.adjust) %>%
    slice_head(n = topn) %>%
    mutate(
      neg10_p = -log10(p.adjust),
      gene_ratio =
        as.numeric(stringr::str_extract(GeneRatio, "\\d+")) /
        as.numeric(stringr::str_extract(GeneRatio, "\\d+$"))
    )

  ggplot(
    go_res2,
    aes(
      x = gene_ratio,
      y = reorder(Description, gene_ratio),
      color = p.adjust, size = Count
    )
  ) +
    geom_point() +
    labs(x = "GeneRatio", y = NULL) +
    scale_color_gradient(low = "blue", high = "red", name = "p.adjust") +
    theme_test() +
    guides(size = guide_legend(order = 2),
           color = guide_colorbar(order = 1)) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = label_format)) +
    theme(
      axis.text.x  = element_text(size = 12, face = "bold"),
      axis.text.y  = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold")
    )
}


#' Dot plot for GO/Pathway output (style 3, generic)
#'
#' A flexible dot plot for enrichment tables containing terms such as:
#' "term description", "enrichment score", "false discovery rate".
#'
#' @param GO_df A generic enrichment result table.
#' @param topn Number of terms.
#' @param label_format Width for wrapping labels.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_point labs scale_color_gradient theme
#' @importFrom dplyr arrange slice_head mutate
#' @importFrom stringr str_wrap
#'
#' @export
plot_GO_dot3 <- function(GO_df, topn = 10, label_format = 30){
  go_res2 <- GO_df %>%
    mutate(FDR = -log10(`false discovery rate`)) %>%
    arrange(desc(FDR)) %>%
    slice_head(n = topn) %>%
    mutate(Count = `genes mapped`)

  ggplot(
    go_res2,
    aes(
      x = `enrichment score`,
      y = reorder(`term description`, `enrichment score`),
      color = FDR, size = Count
    )
  ) +
    geom_point() +
    labs(x = "GeneRatio", y = NULL) +
    scale_color_gradient(low = "blue", high = "red", name = "-log10(FDR)") +
    theme_test() +
    guides(size  = guide_legend(order = 2),
           color = guide_colorbar(order = 1)) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = label_format)) +
    theme(
      axis.text.x  = element_text(size = 12, face = "bold"),
      axis.text.y  = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold")
    )
}





#' Heatmap with mean/median trend line and block annotation
#'
#' This wrapper around ComplexHeatmap visualizes a matrix with row clusters.
#' A trend line (mean or median) is added per column group.
#'
#' @param mat Numeric matrix.
#' @param row_split Vector defining row groups.
#' @param column_split Vector defining column groups.
#' @param top_annotation ComplexHeatmap top annotation.
#' @param show_column_names Logical.
#' @param ht.col.list List controlling heatmap colors.
#' @param border Logical.
#' @param line.size Size of grid lines.
#' @param line.col Color of grid lines.
#' @param mline.size Size of trend line.
#' @param mline.col Color of trend line.
#' @param set.md "mean" or "median".
#' @param textbox.pos Position of group text.
#' @param textbox.size Font size of group text.
#' @param panel.arg Parameters for ComplexHeatmap::anno_link.
#' @param ... Passed to ComplexHeatmap::Heatmap.
#'
#' @return A Heatmap object.
#'
#' @importFrom ComplexHeatmap Heatmap anno_block anno_link rowAnnotation ht_opt grid.textbox
#' @importFrom scales rescale
#' @importFrom grid grid.rect grid.text pushViewport popViewport viewport gpar unit
#'
#' @export

plot_heatmap_withline <- function(
    mat = NULL,
    row_split = NULL,
    column_split = NULL,
    top_annotation = NULL,
    show_column_names = FALSE,
    ht.col.list = list(col_range = c(-2, 0, 2),
                       col_color = c("#08519C", "white", "#A50F15")),
    border = TRUE,
    line.size = 0.1,
    line.col = "grey90",
    mline.size = 2,
    mline.col = "#CC3333",
    set.md = "mean",
    textbox.pos = c(0.5, 0.8),
    textbox.size = 8,
    panel.arg = c(2, 0.25, 4, "grey90", NA),
    ...
) {

  ComplexHeatmap::ht_opt(message = FALSE)

  subgroup <- factor(row_split)
  cluster.num <- nlevels(subgroup)

  # Random colors for cluster blocks
  clustcol <- c(
    "OrangeRed", "SlateBlue3", "DarkOrange", "GreenYellow", "Purple",
    "DarkSlateGray", "Gold", "DarkGreen", "DeepPink2", "Red4",
    "#4682B4", "#FFDAB9", "#708090", "#836FFF", "#CDC673", "#CD9B1D"
  )
  colanno <- sample(clustcol, cluster.num)
  names(colanno) <- levels(subgroup)

  # Row block annotation
  anno.block <- ComplexHeatmap::anno_block(
    align_to = split(1:nrow(mat), subgroup),
    panel_fun = function(index, nm) {
      npos = as.numeric(nm)
      grid::grid.rect(gp = grid::gpar(fill = colanno[npos], col = NA))
      grid::grid.text(
        label = paste0("n:", length(index)),
        rot = 90,
        gp = grid::gpar(col = "white", fontsize = 8)
      )
    },
    which = "row"
  )

  # -----------------------------
  # panel drawing function
  # -----------------------------
  panel_fun <- function(index, nm) {
    grid::pushViewport(grid::viewport(
      xscale = c(0, 1), yscale = c(0, 1), clip = "on"
    ))

    tmpmat <- mat[index, , drop = FALSE]

    # -------------------------------------------------
    # draw mean/median trend line
    # -------------------------------------------------
    if (!is.null(column_split)) {
      col_groups <- factor(column_split)
      group_levels <- levels(col_groups)

      means <- sapply(group_levels, function(g) {
        cols <- which(col_groups == g)
        vals <- as.numeric(tmpmat[, cols])
        if (set.md == "mean") mean(vals, na.rm = TRUE) else median(vals, na.rm = TRUE)
      })

      x_coords <- seq_along(means)

      # scaling
      from_range <- range(means, na.rm = TRUE)
      clip <- function(x) pmin(pmax(x, 0.05), 0.95)

      y_scaled <- clip(scales::rescale(means, to = c(0.05, 0.95), from = from_range))
      x_scaled <- clip(scales::rescale(x_coords, to = c(0.05, 0.95)))

      grid::grid.lines(
        x = grid::unit(x_scaled, "npc"),
        y = grid::unit(y_scaled, "npc"),
        gp = grid::gpar(lwd = mline.size, col = mline.col)
      )

    } else {
      mdia <- if (set.md == "mean") colMeans(tmpmat, na.rm = TRUE)
      else apply(tmpmat, 2, median, na.rm = TRUE)

      y_scaled <- scales::rescale(mdia, to = c(0.05, 0.95))
      x_scaled <- scales::rescale(seq_along(mdia), to = c(0.05, 0.95))

      grid::grid.lines(
        x = grid::unit(x_scaled, "npc"),
        y = grid::unit(y_scaled, "npc"),
        gp = grid::gpar(lwd = mline.size, col = mline.col)
      )
    }

    # -------------------------------------------------
    # add text box (using ComplexHeatmap::grid.textbox)
    # -------------------------------------------------
    text <- paste0("Gene number: ", length(index))

    ComplexHeatmap::grid.textbox(
      text,
      x = textbox.pos[1],
      y = textbox.pos[2],
      gp = grid::gpar(fontsize = textbox.size, fontface = "italic"),
      background_gp = grid::gpar(col = NA, fill = "transparent")
    )

    grid::popViewport()
  }

  # row annotation
  right_annotation <- ComplexHeatmap::rowAnnotation(
    cluster = anno.block,
    line = ComplexHeatmap::anno_link(
      align_to = subgroup,
      which = "row",
      panel_fun = panel_fun,
      size = grid::unit(as.numeric(panel.arg[1]), "cm"),
      gap = grid::unit(as.numeric(panel.arg[2]), "cm"),
      width = grid::unit(as.numeric(panel.arg[3]), "cm"),
      side = "right",
      link_gp = grid::gpar(fill = panel.arg[4], col = panel.arg[5])
    )
  )

  # final heatmap
  ht_final <- ComplexHeatmap::Heatmap(
    as.matrix(mat),
    name = "Z-score",
    cluster_rows = TRUE,
    cluster_columns = FALSE,
    show_row_names = FALSE,
    show_column_names = show_column_names,
    top_annotation = top_annotation,
    column_split = column_split,
    row_split = row_split,
    right_annotation = right_annotation,
    ...
  )

  return(ht_final)
}

