# ======================================================
# Module: Filter Server
# 功能：读取 se 文件 → 移除样本 / 筛选基因 → PCA + QC 绘图 → 导出 zip
# 适配原 UI，输出 ID 不改。
# ======================================================

mod_QC_server <- function(input, output, session) {
  
  # -----------------------------
  # 1️⃣ 数据读取
  # -----------------------------
  se_data <- reactiveVal(NULL)
  
  observeEvent(input$se_file, {
    se <- tryCatch(readRDS(input$se_file$datapath), error = function(e) NULL)
    validate(need(!is.null(se), "Failed to read Rds, please check file format."))
    se_data(se)
  })
  
  # -----------------------------
  # 2️⃣ 移除样本逻辑
  # -----------------------------
  remove_samples <- reactiveVal(character(0))
  
  output$edit_samples_UI <- renderUI({
    req(se_filtered())
    actionButton("edit_samples", "Edit SE (Remove samples)")
  })
  
  observeEvent(input$edit_samples, {
    req(se_data())
    all_cols <- if (!is.null(se_data())) as.data.frame(colData(se_data())) else character(0)
    make_grouped_select("modal_remove_samples", all_cols)$server(input, output, session)
    showModal(
      modalDialog(
        title = "Choose sample to remove",

        make_grouped_select("modal_remove_samples", all_cols)$ui,
        # shinyWidgets::pickerInput(
        #   inputId = "modal_remove_samples",
        #   label   = "Select sample to remove ",
        #   choices = colnames(se_data()),
        #   selected = remove_samples(),
        #   multiple = TRUE,
        #   options = list(
        #     `actions-box` = TRUE,       # 全选/全不选按钮
        #     `live-search` = TRUE,       # 搜索功能
        #     size = 10                   # 下拉显示行数
        #   )
        # ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("apply_remove", "Apply")
        ),
        size = "l",
        easyClose = TRUE
      )
    )
  })
  
  observeEvent(input$apply_remove, {
    req(se_data())
    
    col_df <- as.data.frame(colData(se_data()))
    sel <- input$modal_remove_samples %||% character(0)
    if (length(sel) == 0) {
      removeModal()
      return()
    }
    col_name <- sub("\\|\\|.*$", "", sel[1])     # 所选列名（只有一个）
    vals     <- sub("^.*\\|\\|", "", sel)        # 多个取值
    samples_to_remove <- rownames(col_df)[col_df[[col_name]] %in% vals]
    remove_samples(samples_to_remove)
    
    removeModal()
  })
  
  
  # -----------------------------
  # 3️⃣ 基因筛选逻辑
  # -----------------------------
  gene_keep <- reactiveVal(NULL)
  uploaded_gene_df <- reactiveVal(NULL)
  
  output$edit_genes_UI <- renderUI({
    req(se_filtered())
    actionButton("edit_genes", "Edit SE (Subset genes)")
  })
  
  observeEvent(input$edit_genes, {
    showModal(
      modalDialog(
        title = "Upload Excel file to keep specific genes",
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("apply_keep_genes", "Apply", class = "btn btn-primary")
        ),
        fluidRow(
          column(
            width = 12,
            fileInput(
              "gene_excel_upload",
              "Upload Excel file containing gene list",
              multiple = FALSE,
              accept = c(".xls", ".xlsx")
            ),
            helpText("The first column should contain gene symbols to keep.")
          )
        )
      )
    )
  })
  
  observeEvent(input$gene_excel_upload, {
    req(input$gene_excel_upload)
    df <- tryCatch({
      readxl::read_excel(input$gene_excel_upload$datapath)
    }, error = function(e) {
      showNotification("Failed to read Excel file", type = "error")
      return(NULL)
    })
    if (is.null(df) || nrow(df) == 0) {
      showNotification("Empty or invalid Excel file.", type = "error")
      return(NULL)
    }
    uploaded_gene_df(df)
    
    if (ncol(df) == 1) {
      genes <- unique(na.omit(df[[1]]))
      gene_keep(genes)
      showNotification(paste0("✅ Loaded ", length(genes), " genes from the only column."), type = "message")
    } else {
      show_gene_selection_modal(df)  # 保留原逻辑
    }
  })
  
  observeEvent(input$confirm_gene_selection, {
    req(uploaded_gene_df())
    df <- uploaded_gene_df()
    
    gene_col  <- input$gene_col
    group_col <- input$group_col
    if (!is.null(group_col) && group_col != "None") {
      req(input$group_value)
      df <- df[df[[group_col]] == input$group_value, , drop = FALSE]
    }
    
    genes <- df[[gene_col]]
    genes <- unique(na.omit(trimws(genes)))
    genes <- genes[genes != ""]
    if (length(genes) == 0) {
      showNotification("❌ No valid genes found.", type = "error")
      return(NULL)
    }
    
    gene_keep(genes)
    showNotification(
      paste0("✅ Loaded ", length(genes), " genes (", gene_col,
             if (!is.null(group_col) && group_col != "None") paste0(", group=", input$group_value) else "",
             ")."),
      type = "message"
    )
    removeModal()
  })
  
  # -----------------------------
  # 4️⃣ 过滤数据（样本 + 基因）
  # -----------------------------
  se_filtered <- reactive({
    req(se_data())
    se <- se_data()
    
    rmv <- remove_samples()
    if (!is.null(rmv) && length(rmv) > 0) {
      se <- se[, !(colnames(se) %in% rmv), drop = FALSE]
    }
    
    keep_genes <- gene_keep()
    if (!is.null(keep_genes) && length(keep_genes) > 0) {
      keep_rows <- intersect(rownames(se), keep_genes)
      se <- se[keep_rows, , drop = FALSE]
    }
    
    se
  })
  
  # -----------------------------
  # 5️⃣ PCA 分析
  # -----------------------------
  pca_results <- reactive({
    req(se_filtered())
    se <- se_filtered()
    validate(
      need(ncol(se) >= 2, "至少需要保留 2 个样本才能计算 PCA"),
      need("intensity" %in% assayNames(se),
           "未找到 assay 'intensity'，请检查对象或更换 assay 名称")
    )
    intersity_mtx <- as.matrix(assay(se, "intensity"))
    FactoMineR::PCA(t(intersity_mtx), scale.unit = TRUE, graph = FALSE)
  })
  
  pca_df <- reactive({
    req(se_filtered())
    se <- se_filtered()
    pca.res <- pca_results()
    df <- as.data.frame(pca.res$ind$coord[, 1:2])
    for (i in colnames(colData(se))) {
      df[[i]] <- factor(colData(se)[[i]],levels = unique(colData(se)[[i]]))
    }
    df
  })
  
  # -----------------------------
  # 6️⃣ QC / PCA 绘图
  # -----------------------------
  output$color_selector <- renderUI({
    req(se_filtered())
    selectInput(
      "colorby", "Color by",
      choices = colnames(colData(se_filtered())),
      selected = if ("condition" %in% colnames(colData(se_filtered())))
        "condition" else colnames(colData(se_filtered()))[1]
    )
  })
  
  output$label_selector <- renderUI({
    req(se_filtered())
    selectInput(
      "labelby", "Label by",
      choices = c(colnames(colData(se_filtered())), "NULL"),
      selected = colnames(colData(se_filtered()))[1]
    )
  })
  
  output$included_info <- renderText({
    req(se_data())
    total <- ncol(se_data())
    kept  <- ncol(se_filtered())
    paste0("Sample info：", kept, " / ", total)
  })
  
  output$intensity_density <- renderPlot({
    plotSE_density(se_filtered())
  }, height = function() input$plot_height, width = function() input$plot_width)
  
  output$proteins_number <- renderPlot({
    plotSE_protein_number(se_filtered())
  }, height = function() input$plot_height, width = function() input$plot_width)
  
  output$cv_density <- renderPlot({
    plotCV_density(se_filtered())
  }, height = function() input$plot_height, width = function() input$plot_width)
  
  output$Miss_value_statistics <- renderPlot({
    plotSE_missing_value(se_filtered())
  }, height = function() input$plot_height, width = function() input$plot_width)
  
  output$pca_plot <- renderPlot({
    req(pca_df(), pca_results(), input$colorby, input$labelby)
    plot_pca(pca_df(), pca_results(), input$colorby, input$labelby)
  }, height = function() input$plot_height, width = function() input$plot_width)
  
  # -----------------------------
  # 7️⃣ 导出 Zip
  # -----------------------------
  output$download_pca_pdf <- make_download_pdf(
    plot_expr   = function() plot_pca(pca_df(), pca_results(), input$colorby, input$labelby),
    input       = input,
    suffix      = "PCA",
    width       = function() input$plot_width / 100,
    height      = function() input$plot_height / 100,
    input_field = "se_file"
  )
  
  output$download_protein_density_pdf <- make_download_pdf(
    plot_expr   = function() plotSE_density(se_filtered()),
    input       = input,
    suffix      = "intensity_density",
    width       = function() input$plot_width / 100,
    height      = function() input$plot_height / 100,
    input_field = "se_file"
  )
  
  output$download_protein_num_pdf <- make_download_pdf(
    plot_expr   = function() plotSE_protein_number(se_filtered()),
    input       = input,
    suffix      = "proteins_number",
    width       = function() input$plot_width / 100,
    height      = function() input$plot_height / 100,
    input_field = "se_file"
  )
  
  output$download_na_pdf <- make_download_pdf(
    plot_expr   = function() plotSE_missing_value(se_filtered()),
    input       = input,
    suffix      = "miss_value_statistics",
    width       = function() input$plot_width / 100,
    height      = function() input$plot_height / 100,
    input_field = "se_file"
  )
  
  output$download_cv_density_pdf <- make_download_pdf(
    plot_expr   = function() plotCV_density(se_filtered()),
    input       = input,
    suffix      = "CV_density",
    width       = function() input$plot_width / 100,
    height      = function() input$plot_height / 100,
    input_field = "se_file"
  )
  
  output$download_se2 <- make_download_se_zip(
    se_reactive   = se_filtered,   # 传 reactiveExpr（不要加 ()）
    input         = input,
    file_input_id = "se_file",     # 上传文件 ID
    suffix        = "_filtered"    # 可选
  )
  
  output$download_edit_sample <- renderUI({
    req(se_filtered())
    downloadButton("download_se2", "Download edit samples")
  })
}
