# ======================================================
# Module: Enrichment + STRING server
# 功能：GO / KEGG / GSEA 富集分析 + STRING 网络生成
# 保持原始逻辑、UI ID 完全不变
# ======================================================

mod_enrich_server <- function(input, output, session) {
  
  # ----------------------------------------
  # 1️⃣ 通用数据容器
  # ----------------------------------------
  enriched_data <- reactiveVal(NULL)
  rv <- reactiveValues(latest_url = NULL)
  
  # =====================================================
  #                GO enrichment
  # =====================================================
  observeEvent(input$enrich_upload_GO, {
    req(input$enrich_upload_GO)
    ext <- tools::file_ext(input$enrich_upload_GO$name)
    df <- switch(ext,
                 xls  = readxl::read_xls(input$enrich_upload_GO$datapath),
                 xlsx = readxl::read_xlsx(input$enrich_upload_GO$datapath),
                 stop("Invalid file; Please upload a .xls or .xlsx file")
    )
    enriched_data(df)
    show_gene_selection_modal_go(df)
  })
  
  output$group_value_ui <- renderUI({
    req(enriched_data(), input$group_col)
    df <- enriched_data()
    if (input$group_col == "None") return(NULL)
    vals <- unique(df[[input$group_col]])
    selectInput("group_value", "Select group to enrich:", choices = vals, selected = vals[1])
  })
  
  go_enrich_res <- eventReactive(input$confirm_gene_selection, {
    req(enriched_data())
    df <- enriched_data()
    genes <- if (ncol(df) == 1) df[[1]] else {
      if (!is.null(input$group_col) && input$group_col != "None") {
        df <- df[df[[input$group_col]] == input$group_value, , drop = FALSE]
      }
      df[[input$gene_col]]
    }
    removeModal()
    enrichment_analysis(genes, db = "GO", species = detect_species_from_symbol(genes))
  })
  
  output$enrichres_table <- renderDT({
    req(go_enrich_res())
    datatable(go_enrich_res())
  })
  
  output$download_enrich_data <- downloadHandler(
    filename = function() paste0("GO_enriched_results_", Sys.Date(), ".xlsx"),
    content = function(file) writexl::write_xlsx(go_enrich_res(), file)
  )
  
  output$download_enrich_data_ui <- renderUI({
    req(go_enrich_res())
    downloadButton("download_enrich_data", "Download results")
  })
  
  # =====================================================
  #                KEGG enrichment
  # =====================================================
  observeEvent(input$enrich_upload_KEGG, {
    req(input$enrich_upload_KEGG)
    ext <- tools::file_ext(input$enrich_upload_KEGG$name)
    df <- switch(ext,
                 xls  = readxl::read_xls(input$enrich_upload_KEGG$datapath),
                 xlsx = readxl::read_xlsx(input$enrich_upload_KEGG$datapath),
                 stop("Invalid file; Please upload a .xls or .xlsx file")
    )
    enriched_data(df)
    show_gene_selection_modal_go(df)
  })
  
  kegg_enrich_res <- eventReactive(input$confirm_gene_selection, {
    req(enriched_data())
    df <- enriched_data()
    genes <- if (ncol(df) == 1) df[[1]] else {
      if (!is.null(input$group_col) && input$group_col != "None") {
        df <- df[df[[input$group_col]] == input$group_value, , drop = FALSE]
      }
      df[[input$gene_col]]
    }
    removeModal()
    enrichment_analysis(genes, db = "KEGG", species = detect_species_from_symbol(genes))
  })
  
  output$kegg_enrichres_table <- renderDT({
    req(kegg_enrich_res())
    datatable(kegg_enrich_res())
  })
  
  output$download_kegg_enrich_data <- downloadHandler(
    filename = function() paste0("KEGG_enriched_results_", Sys.Date(), ".xlsx"),
    content = function(file) writexl::write_xlsx(kegg_enrich_res(), file)
  )
  
  output$download_kegg_enrich_data_ui <- renderUI({
    req(kegg_enrich_res())
    downloadButton("download_kegg_enrich_data", "Download results")
  })
  
  # =====================================================
  #                GSEA-GO enrichment
  # =====================================================
  observeEvent(input$enrich_upload_gse_GO, {
    req(input$enrich_upload_gse_GO)
    ext <- tools::file_ext(input$enrich_upload_gse_GO$name)
    df <- switch(ext,
                 xls  = readxl::read_xls(input$enrich_upload_gse_GO$datapath),
                 xlsx = readxl::read_xlsx(input$enrich_upload_gse_GO$datapath),
                 stop("Invalid file; Please upload a .xls or .xlsx file")
    )
    enriched_data(df)
    show_gene_selection_modal_gse_go(df)
  })
  
  gse_GO_enrich_res <- eventReactive(input$confirm_ges_go_gene_selection, {
    req(enriched_data())
    df <- enriched_data()
    removeModal()
    withProgress(message = "Uploading and Calculating...", {
      gesa_res <- run_gsea(
        df,
        gene_col = input$gse_GO_gene_col,
        db = "GO",
        stat_col = input$logFC_col,
        species = detect_species_from_symbol(df[[input$gse_GO_gene_col]])
      )
    })
    gesa_res@result
  })
  
  output$ges_go_enrichres_table <- renderDT({
    req(gse_GO_enrich_res())
    datatable(gse_GO_enrich_res())
  })
  
  output$download_gse_go_enrich_data <- downloadHandler(
    filename = function() paste0("GSEA_GO_enriched_results_", Sys.Date(), ".xlsx"),
    content = function(file) writexl::write_xlsx(gse_GO_enrich_res(), file)
  )
  
  output$download_gse_go_enrich_data_ui <- renderUI({
    req(gse_GO_enrich_res())
    downloadButton("download_gse_go_enrich_data", "Download results")
  })
  
  # =====================================================
  #                GSEA-KEGG enrichment
  # =====================================================
  observeEvent(input$enrich_upload_gse_kegg, {
    req(input$enrich_upload_gse_kegg)
    ext <- tools::file_ext(input$enrich_upload_gse_kegg$name)
    df <- switch(ext,
                 xls  = readxl::read_xls(input$enrich_upload_gse_kegg$datapath),
                 xlsx = readxl::read_xlsx(input$enrich_upload_gse_kegg$datapath),
                 stop("Invalid file; Please upload a .xls or .xlsx file")
    )
    enriched_data(df)
    show_gene_selection_modal_gse_go(df)
  })
  
  gse_KEGG_enrich_res <- eventReactive(input$confirm_ges_go_gene_selection, {
    req(enriched_data())
    df <- enriched_data()
    removeModal()
    withProgress(message = "Uploading and Calculating...", {
      gesa_res <- run_gsea(
        df,
        gene_col = input$gse_GO_gene_col,
        db = "KEGG",
        stat_col = input$logFC_col,
        species = detect_species_from_symbol(df[[input$gse_GO_gene_col]])
      )
    })
    gesa_res@result
  })
  
  output$ges_kegg_enrichres_table <- renderDT({
    req(gse_KEGG_enrich_res())
    datatable(gse_KEGG_enrich_res())
  })
  
  output$download_gse_KEGG_enrich_data <- downloadHandler(
    filename = function() paste0("GSEA_KEGG_enriched_results_", Sys.Date(), ".xlsx"),
    content = function(file) writexl::write_xlsx(gse_KEGG_enrich_res(), file)
  )
  
  output$download_gse_KEGG_enrich_data_ui <- renderUI({
    req(gse_KEGG_enrich_res())
    downloadButton("download_gse_KEGG_enrich_data", "Download results")
  })
  
  # =====================================================
  #                STRING 网络生成
  # =====================================================
  parse_genes <- reactive({
    g <- str_split(input$genes %||% "", pattern = "[,;\\s]+")[[1]]
    g <- unique(g[g != ""])
    validate(need(length(g) > 0, "请至少输入一个基因"))
    g
  })
  
  observeEvent(input$open, {
    showModal(
      modalDialog(
        title = "确认",
        tags$p("将向 STRING 服务器发起请求以生成网络链接。是否继续？"),
        footer = tagList(
          modalButton("取消"),
          actionButton("confirm_call", "确认生成", class = "btn btn-primary")
        ),
        easyClose = TRUE
      )
    )
  })
  
  observeEvent(input$confirm_call, {
    removeModal()
    genes <- parse_genes()
    
    withProgress(message = "正在调用 STRING API …", value = 0, {
      incProgress(0.2, detail = "ID 映射")
      idmap <- tryCatch(
        string_post_tsv(
          "get_string_ids",
          query = list(species = input$species, caller_identity = "EasyProtein"),
          body  = list(identifiers = paste(genes, collapse = "\r"))
        ),
        error = function(e) {
          showNotification(paste("ID 映射失败：", e$message), type = "error")
          NULL
        }
      )
      req(!is.null(idmap), nrow(idmap) > 0)
      idmap <- pick_one_per_query(idmap)
      ids <- unique(idmap$stringId)
      validate(need(length(ids) > 0, "没有映射到任何 STRING ID"))
      
      incProgress(0.7, detail = "生成链接")
      linkdf <- tryCatch(
        string_post_tsv(
          "get_link",
          query = list(
            species = input$species,
            required_score = 700,
            network_flavor = input$flavor,
            caller_identity = "EasyProtein"
          ),
          body = list(identifiers = paste(ids, collapse = "\r"))
        ),
        error = function(e) {
          showNotification(paste("获取链接失败：", e$message), type = "error")
          NULL
        }
      )
      req(!is.null(linkdf), nrow(linkdf) > 0)
      url_col <- intersect(c("url", "string_network_url", "network_url"), names(linkdf))
      validate(need(length(url_col) > 0, "未返回可用链接列"))
      url <- linkdf[[url_col[1]]][1]
      validate(need(!is.na(url) && nzchar(url), "返回了空链接"))
      rv$latest_url <- url
      incProgress(1)
      
      showModal(
        modalDialog(
          title = "STRING 链接已生成",
          tags$p("点击下方按钮在新标签页打开 STRING 原生页面。"),
          actionButton("open_newtab", "在新标签页打开", class = "btn btn-primary"),
          tags$hr(),
          tags$small("或复制下面的链接手动打开："),
          tags$pre(style = "white-space:pre-wrap; word-break:break-all; margin-top:6px;", url),
          easyClose = TRUE, footer = NULL
        )
      )
    })
  })
  
  observeEvent(input$open_newtab, {
    req(rv$latest_url)
    shinyjs::runjs(sprintf("window.open('%s','_blank','noopener');", js_escape(rv$latest_url)))
  })
  
  observeEvent(input$gene_file, {
    req(input$gene_file)
    df <- readxl::read_excel(input$gene_file$datapath)
    genes <- df[[1]]
    genes <- genes[!is.na(genes)]
    updateTextAreaInput(session, "genes", value = paste(genes, collapse = "\n"))
  })
}
