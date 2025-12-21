# ======================================================
# Module: Visualization Server
# 功能：差异分析结果可视化（Volcano plot + GO dotplot）
# 保持所有 UI 输出 ID 和逻辑完全不变
# ======================================================

mod_visualization_server <- function(input, output, session) {
  
  # =====================================================
  # 1️⃣ Volcano plot 部分
  # =====================================================
  DEGs_rv2 <- reactiveVal(NULL)
  
  observeEvent(input$Upload_DEG_table1, {
    req(input$Upload_DEG_table1)
    deg_path <- input$Upload_DEG_table1$datapath
    DEGs_df <- readxl::read_excel(deg_path)
    DEGs_rv2(DEGs_df)
  })
  
  make_vol_plot <- reactive({
    req(DEGs_rv2())
    df <- DEGs_rv2()
    
    ivolcano(
      data          = df,
      logFC_col     = "logFC",
      pval_col      = "adj.P.Val",
      gene_col      = "Genes",
      pval_cutoff   = input$Volcano_FDR,
      logFC_cutoff  = input$Volcano_log2FC,
      top_n         = input$Volcano_topN,
    )
  })
  
  output$volcano <- ggiraph::renderGirafe({
    req(DEGs_rv2())
    make_vol_plot()$ggiraph_obj
  })
  
  output$download_vol_pdf <- downloadHandler(
    filename = function() {
      base_full <- tools::file_path_sans_ext(basename(input$Upload_DEG_table1$name))
      paste0(base_full, "_volcano.pdf")
    },
    content = function(file) {
      p <- make_vol_plot()$ggobj
      ggplot2::ggsave(file, plot = p, device = cairo_pdf, width = 8, height = 6, dpi = 600)
    }
  )
  
  
  
  # =====================================================
  # 2️⃣ GO enrichment dotplot 部分
  # =====================================================
  go_df <- reactive({
    req(input$go_file)
    ext <- tools::file_ext(input$go_file$name)
    df <- switch(tolower(ext),
                 "xlsx" = readxl::read_xlsx(input$go_file$datapath),
                 "xls"  = readxl::read_xls(input$go_file$datapath),
                 stop("Only .xls/.xlsx are supported")
    )
    
    need_cols <- c("Description", "p.adjust", "GeneRatio", "Count")
    validate(
      need(all(need_cols %in% names(df)),
           paste("Missing columns in file:", paste(setdiff(need_cols, names(df)), collapse = ", "))
      )
    )
    df
  })
  

  output$GO_enrich_plot1 <- renderPlot({
    req(go_df())
    plot_GO_dot1(go_df(), topn = input$go_topn, label_format = input$go_label_width)
  },
  height = function() input$go_plot_height,
  width = function() input$go_plot_width)
  
  output$GO_enrich_plot2 <- renderPlot({
    req(go_df())
    plot_GO_dot2(go_df(), topn = input$go_topn, label_format = input$go_label_width)
  },
  height = function() input$go_plot_height,
  width = function() input$go_plot_width)
  
 
  output$down_GO_pdf_style1 <- renderUI({
    req(go_df())
    downloadButton("dl_go_style1", "Download PDF")
  })
  
  output$down_GO_pdf_style2 <- renderUI({
    req(go_df())
    downloadButton("dl_go_style2", "Download PDF")
  })
  
 
  output$dl_go_style1 <- make_download_pdf(
    plot_expr   = function() plot_GO_dot1(go_df(), topn = input$go_topn, label_format = input$go_label_width),
    input       = input,
    suffix      = "GO_style1",
    width       = function() input$go_plot_width  / 100,
    height      = function() input$go_plot_height / 100,
    input_field = "go_file"
  )
  
  output$dl_go_style2 <- make_download_pdf(
    plot_expr   = function() plot_GO_dot2(go_df(), topn = input$go_topn, label_format = input$go_label_width),
    input       = input,
    suffix      = "GO_style2",
    width       = function() input$go_plot_width  / 100,
    height      = function() input$go_plot_height / 100,
    input_field = "go_file"
  )
  #string
  string_go_df <- reactive({
    req(input$string_go_file)
    ext <- tools::file_ext(input$string_go_file$name)
    df <- switch(tolower(ext),
                 "xlsx" = readxl::read_xlsx(input$string_go_file$datapath),
                 "xls"  = readxl::read_xls(input$string_go_file$datapath),
                 "tsv"  = data.table::fread(input$string_go_file$datapath),
                 stop("Only .tsv/.xls/.xlsx are supported")
    )
    
    need_cols <- c("false discovery rate", "genes mapped", "term description", "enrichment score")
    validate(
      need(all(need_cols %in% names(df)),
           paste("Missing columns in file:", paste(setdiff(need_cols, names(df)), collapse = ", "))
      )
    )
    df
  })
  output$GO_enrich_plot3 <- renderPlot({
    req(string_go_df())
    plot_GO_dot3(string_go_df(), topn = input$string_go_go_topn, label_format = input$string_go_go_label_width)
  },
  height = function() input$string_go_plot_height,
  width = function() input$string_go_width)
  
  
  output$down_GO_pdf_style3 <- renderUI({
    req(string_go_df())
    downloadButton("dl_go_style3", "Download PDF")
  })
  output$dl_go_style3 <- make_download_pdf(
    plot_expr   = function() plot_GO_dot3(string_go_df(), topn = input$string_go_go_topn, label_format = input$string_go_go_label_width),
    input       = input,
    suffix      = "string_enrichment",
    width       = function() input$go_plot_width  / 100,
    height      = function() input$go_plot_height / 100,
    input_field = "string_go_file"
  )
  ##gene expression------
  
  se_data   <- reactiveVal(NULL)
  observeEvent(input$gene_exp_rds, {
    req(input$gene_exp_rds)
    se <- readRDS(input$gene_exp_rds$datapath)
    se_data(se)
  })
  output$gene_selector_ui <- renderUI({
    req(se_data())   # 等 se 上传后才触发
    se <- se_data()
    gene_list <- rowData(se)$Genes
    col_vars <- colnames(colData(se))
    tagList(
      selectizeInput(
        inputId = "gene_select",
        label = "选择基因：",
        choices = gene_list,
        multiple = TRUE,
        options = list(
          placeholder = "请选择或输入基因名称...",
          maxOptions = 1000   # 避免太多选项一次加载
        )
      ),
      selectInput(
        inputId = "group_col",
        label = "选择分组列",
        choices = col_vars,
        selected = if ("condition" %in% col_vars) "condition" else col_vars[1]
      )
    )
  })
  
  output$choose_samples_UI <- renderUI({
    req(se_data())
    all_cols <- as.data.frame(colData(se_data()))
    
    make_grouped_select("selected_cols", all_cols, default_all = TRUE)$ui
  })
  
  observe({
    req(se_data())
    all_cols <- as.data.frame(colData(se_data()))
    make_grouped_select("selected_cols", all_cols, default_all = TRUE)$server(input, output, session)
  })

  

  output$gene_exp <- renderPlot({
    req(se_data(), input$gene_select)
    
    se <- se_data()
    genes <- input$gene_select
    
    
    
    sel <- input$selected_cols 
    col_name <- sub("\\|\\|.*$", "", sel[1])  
    vals <- sub("^.*\\|\\|", "", sel)        
    col_df <- as.data.frame(colData(se_data()))
    matched_samples <- rownames(col_df)[col_df[[col_name]] %in% vals]
    
    if (length(matched_samples) == 0) {
      showNotification("No samples matched your selection", type = "error")
      return()
    }
    se_sub <- se[, matched_samples, drop = FALSE]
    
    valid_genes <- genes[genes %in% rowData(se_sub)$Genes]
    if (length(valid_genes) == 0) return(NULL)
    
    if (length(valid_genes) == 1) {
      return(
        plot_gene_expression(se_sub, valid_genes, by = input$group_col)
      )
    }else{
      plot_list <- map(valid_genes, ~{
        plot_gene_expression(se_sub, .x, by = input$group_col) 
      })
      combined <- plot_grid(plotlist = plot_list)  
      return(combined)
    }
  }, height = function() input$gene_exp_height,
  width  = function() input$gene_exp_width)
  
  
  output$down_gene_exp_ui <- renderUI({
    req(se_data())
    downloadButton("down_gene_exp", "Download PDF")
  })
  # output$down_gene_exp <- make_download_pdf(
  #   plot_expr   = function() plot_gene_expression(se_sub, input$gene_select,by= input$group_col),
  #   input       = input,
  #   suffix      = paste0(input$gene_select,"_expression"),
  #   width       = function() input$gene_exp_width  / 100,
  #   height      = function() input$gene_exp_height / 100,
  #   input_field = "gene_exp_rds"
  # )
  

  
}
 



