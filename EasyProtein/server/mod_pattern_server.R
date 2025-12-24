# ======================================================
# Module: Pattern Server
# 功能：加载 SummarizedExperiment -> 参数选择 -> 聚类分析 -> 热图 -> 导出
# 保持 UI ID 和外部接口不变
# ======================================================

mod_pattern_server <- function(input, output, session) {


  rv <- reactiveValues(
    se     = NULL,
    se_sub = NULL
  )

  mat_data <- reactiveVal(NULL)
  row_info <- reactiveVal(NULL)
  col_info <- reactiveVal(NULL)
  pdf_path <- reactiveVal(NULL)

  # =============================
  # 1. 上传 SE
  # =============================
  observeEvent(input$matrix_file, {
    req(input$matrix_file)
    se_obj <- readRDS(input$matrix_file$datapath)
    rv$se <- se_obj
    show_heatmap_param_modal(rv$se)
  })

  # 重置参数（重新弹窗）
  observeEvent(input$reset_params, {
    req(rv$se)
    removeModal()
    show_heatmap_param_modal(rv$se)
  })

  # =============================
  # 2. colData 动态更新 + 分组选择器
  # =============================
  observe({
    req(rv$se)
    cd <- colData(rv$se)
    updateSelectInput(session, "coldata_col_selector", choices = colnames(cd))
  })

  output$coldata_col_selector <- renderUI({
    req(rv$se)
    all_cols <- as.data.frame(colData(rv$se))

    # 初始化 grouped select 的 server 逻辑
    make_grouped_select(id = "selected_cols",df= all_cols,label = 'Select coldata columns', default_all = TRUE)$server(input, output, session)

    # 真正显示在 UI 里的 coldata 分组选择下拉框
    selectInput(
      "coldata_col_selector",
      "Select grouping column from colData",
      choices  = colnames(colData(rv$se)),
      selected = "condition"
    )
  })
  observe({
    req(rv$se)
    rd <- rowData(rv$se)
    updateSelectInput(session, "coldata_row_selector", choices = colnames(rd))
  })

  output$coldata_row_selector <- renderUI({
    req(rv$se)
    all_rows <- as.data.frame(rowData(rv$se))

    # 初始化 grouped select 的 server 逻辑
    make_grouped_select(id = "selected_rows", df = all_rows, label = 'Select rowdata columns',default_all = TRUE)$server(input, output, session)

    # 真正显示在 UI 里的 coldata 分组选择下拉框
    selectInput(
      "coldata_row_selector",
      "Select grouping column from rowData",
      choices  = colnames(rowData(rv$se)),
      selected = colnames(rowData(rv$se))[1]
    )
  })


  # =============================
  # 3. 主逻辑：确认参数 -> 子集 -> 聚类 -> 热图
  # =============================
  observeEvent(input$confirm_params, {
    req(rv$se)
    removeModal()

    se <- rv$se
    ##col select
    sel <- input$selected_cols
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Please select at least one sample group", type = "error")
      return()
    }
    col_name <- sub("\\|\\|.*$", "", sel[1])
    vals     <- sub("^.*\\|\\|", "", sel)

    col_df <- as.data.frame(colData(se))
    sample_names <- rownames(col_df)
    if (is.null(sample_names)) {
      # 如果 colData 没有 rownames，则用列名当样本名
      sample_names <- colnames(se)
    }

    matched_samples <- sample_names[col_df[[col_name]] %in% vals]
    if (length(matched_samples) == 0) {
      showNotification("No samples matched your selection", type = "error")
      return()
    }

    #row
    sel_row <- input$selected_rows
    row_df <- as.data.frame(rowData(se))
    gene_names <- rownames(row_df)

    if (is.null(gene_names)) {
      showNotification("rowData has no rownames (gene names missing)", type = "error")
      return()
    }

    if (is.null(sel_row) || length(sel_row) == 0) {
      matched_genes <- gene_names
    } else {
      row_name <- sub("\\|\\|.*$", "", sel_row[1])
      vals_row <- sub("^.*\\|\\|", "", sel_row)

      if (!row_name %in% colnames(row_df)) {
        showNotification(paste("Invalid row annotation:", row_name), type = "error")
        return()
      }

      matched_genes <- gene_names[row_df[[row_name]] %in% vals_row]

      if (length(matched_genes) == 0) {
        showNotification("No genes matched your selection", type = "error")
        return()
      }
    }


    #subset here
    se_sub <- se[matched_genes, matched_samples, drop = FALSE]


    intersity_mtx <- as.matrix(assay(se_sub, "conc"))
    thr    <- input$expr_min
    cv_thr <- input$cv_min

    row_index <- intersect(
      which(rowMeans(intersity_mtx) > thr),
      which(apply(intersity_mtx, 1, calculate_cv) > cv_thr)
    )

    if (length(row_index) == 0) {
      showNotification("No features passed expression/CV filters", type = "error")
      return()
    }

    intersity_mtx <- intersity_mtx[row_index, , drop = FALSE]
    se_sub        <- se_sub[row_index, ]

    intersity_scale <- scale_mtx(intersity_mtx)
    mat_data(intersity_scale)

    # 加载弹窗
    showModal(modalDialog(
      title = NULL,
      "Loading... Please wait a second",
      footer = NULL,
      easyClose = FALSE
    ))

    # =============================
    # 3.3 行聚类
    # =============================
    # 返回 data.frame: protein_group + km_cluster

    if (identical(input$row_k, "AUTO")) {
      row_cluster_df <- auto_cluster_matrix_pca_one(intersity_scale, mode = "row")
    } else {
      rk <- kmeans(intersity_scale, centers = input$row_k, nstart = 1)
      row_cluster_df <- tibble::tibble(
        protein_group = rownames(intersity_scale),
        km_cluster    = paste0('km',rk$cluster)
      )
    }

    # 对齐 se_sub 行顺序，写入 rowData
    row_cluster_vec <- row_cluster_df$km_cluster[
      match(rownames(se_sub), row_cluster_df$protein_group)
    ]

    if (any(is.na(row_cluster_vec))) {
      stop("Row clustering result does not match rownames of se_sub.")
    }

    existing_cols <- colnames(rowData(se_sub))
    km_cols <- grep("^km_cluster[0-9]*$", existing_cols, value = TRUE)
    new_col_name <- if (length(km_cols) == 0) {
      "km_cluster"
    } else {
      paste0("km_cluster", length(km_cols) + 1)
    }

    rowData(se_sub)[[new_col_name]] <- as.character(row_cluster_vec)

    # =============================
    # 3.4 列聚类
    # =============================
    if (input$col_cluster_mode == "kmeans") {
      if (identical(input$col_k, "AUTO")) {
        col_cluster_df <- auto_cluster_matrix_pca_one(intersity_scale, mode = "col")
      } else {
        ck <- kmeans(t(intersity_scale), centers = input$col_k, nstart = 1)
        col_cluster_df <- tibble::tibble(
          protein_group = colnames(intersity_scale),
          km_cluster    = paste0('km',ck$cluster)
        )
      }
      col_cluster_vec <- col_cluster_df$km_cluster

    } else {
      group_vec <- as.character(colData(se_sub)[[input$coldata_col_selector]])
      col_cluster_df <- tibble::tibble(
        protein_group = colnames(intersity_scale),
        group         = factor(group_vec, levels = unique(group_vec))
      )
      col_cluster_vec <- col_cluster_df$group
    }

    if (!"col_cluster" %in% colnames(colData(se_sub))) {
      colData(se_sub)$col_cluster <- as.character(col_cluster_vec)
    }

    # 缓存给下游使用
    rv$se_sub <- se_sub

    # =============================
    # 3.5 构造 row_info（导出用）
    # =============================
    mean_expr <- calc_gene_mean_by_condition(se_sub, condition_col = "col_cluster") %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Protein.Ids")

    row_info_df <- row_cluster_df %>%
      dplyr::left_join(mean_expr, by = c("protein_group" = "Protein.Ids")) %>%
      dplyr::left_join(
        se2conc(se_sub),
        by = c("protein_group" = "Protein.Ids")
      )

    row_info(row_info_df)
    col_info(col_cluster_df)

    # =============================
    # 3.6 画热图 PDF
    # =============================
    out_pdf <- file.path("www", "heatmap.pdf")

    cairo_pdf(out_pdf, width = input$pdf_width, height = input$pdf_height, fallback_resolution = 300)

    # 顶部注释
    top_anno <- NULL
    if (input$top_annotaion_legend != "NULL") {
      group_col <- input$top_annotaion_legend
      df_anno   <- as.data.frame(colData(se_sub))

      if (group_col %in% colnames(df_anno)) {
        vec <- df_anno[[group_col]]
        if (is.character(vec)) vec <- factor(vec, levels = unique(vec))
        top_anno <- ComplexHeatmap::HeatmapAnnotation(group = vec)
      }
    }

    # 行、列 split 向量（严格对齐）
    row_split_vec <- rowData(se_sub)[[new_col_name]]
    col_split_vec <- colData(se_sub)$col_cluster

    stopifnot(length(row_split_vec) == nrow(intersity_scale))
    stopifnot(length(col_split_vec) == ncol(intersity_scale))

    if (input$col_cluster_mode == "kmeans") {
      ht <- ComplexHeatmap::Heatmap(
        intersity_scale,
        cluster_columns   = TRUE,
        cluster_rows      = input$enable_row_cluster,
        show_row_names    = input$show_row_names,
        show_column_names = input$show_col_names,
        row_split         = row_split_vec,
        column_split      = col_split_vec,
        top_annotation    = top_anno,
        name              = "Z-score"
      )
    } else {
      if (isTRUE(input$enable_col_cluster)) {
        ht <- ComplexHeatmap::Heatmap(
          intersity_scale,
          cluster_columns   = TRUE,
          cluster_rows      = input$enable_row_cluster,
          show_row_names    = input$show_row_names,
          show_column_names = input$show_col_names,
          column_title_gp   = grid::gpar(fontsize = input$column_title_size),
          row_split         = row_split_vec,
          column_split      = col_split_vec,
          top_annotation    = top_anno,
          name              = "Z-score"
        )
      } else {
        ht <- plot_heatmap_withline(
          mat              = intersity_scale,
          row_split        = row_split_vec,
          column_split     = col_split_vec,
          show_column_names = input$show_col_names,
          top_annotation   = ComplexHeatmap::HeatmapAnnotation(
            group = col_split_vec
          )
        )
      }
    }

    ComplexHeatmap::draw(ht)
    grDevices::dev.off()

    pdf_path(out_pdf)
    removeModal()

    # =============================
    # 3.7 输出 UI：PDF 预览 + 按钮
    # =============================
    output$heatmap_pdf <- renderUI({
      tags$iframe(
        src   = "heatmap.pdf",
        style = "width:100%; height:700px;"
      )
    })

    output$reset_params_ui <- renderUI({
      req(row_info())
      actionButton("reset_params", "Reset parameters", class = "btn-info")
    })

    output$download_ui <- renderUI({
      req(row_info())
      downloadButton("download_clusters", "Download clusters results")
    })
  })

  # =============================
  # 4. 下载聚类结果
  # =============================
  output$download_clusters <- downloadHandler(
    filename = function() {
      paste0(
        tools::file_path_sans_ext(basename(input$matrix_file$name)),
        "_cluster_results.xlsx"
      )
    },
    content = function(file) {
      require(openxlsx)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Row_Clusters")
      openxlsx::addWorksheet(wb, "Col_Clusters")
      openxlsx::writeData(wb, "Col_Clusters", col_info())
      openxlsx::writeData(wb, "Row_Clusters", row_info())
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # =============================
  # 5. 下载带聚类信息的 SE
  # =============================
  output$download_pattern_se <- make_download_time_se_zip(
    se_reactive   = reactive(rv$se_sub),
    input         = input,
    file_input_id = "matrix_file",
    suffix        = "_clustering"
  )

  output$download_pattern_se_UI <- renderUI({
    req(rv$se_sub)
    downloadButton("download_pattern_se", "Download SE")
  })
}
