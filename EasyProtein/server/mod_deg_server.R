# ======================================================
# Module: DEG Server
# 功能：读取 SE 对象 → 选择分组列 / 参考组 / 比较组 → 运行差异分析 → 输出表格 + 下载结果
# 兼容原 UI，保持 ID 不变。
# ======================================================

mod_deg_server <- function(input, output, session, rv) {
  
  # --------------------------------------------------
  # 1️⃣ 上传 SummarizedExperiment
  # --------------------------------------------------
  observeEvent(input$Upload_SE2, {
    req(input$Upload_SE2)
    se_path <- input$Upload_SE2$datapath
    
    tryCatch({
      se <- readRDS(se_path)
      rv$se <- se   # ✅ 写入全局 rv$se
      showNotification("文件读取成功 ✅", type = "message")
      
      cols <- colnames(SummarizedExperiment::colData(se))
      default_col <- if ("condition" %in% cols) "condition" else cols[1]
      
      showModal(
        modalDialog(
          title = "Choose grouping column",
          tagList(
            selectInput(
              inputId = "deg_group_col",
              label   = "Grouping column",
              choices = cols,
              selected = default_col,
              width = "100%"
            )
          ),
          # ---- donor 列
          radioButtons(
            inputId = "deg_pair_test",
            label = "Paired test",
            choices = c("NO", "YES"),
            selected = "NO",
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.deg_pair_test == 'YES'",
            selectInput(
              inputId = "deg_pair_col",
              label   = "Pairing column",
              choices = cols,
              selected = NULL,
              width = "100%"
            )
          ),
          
          footer = tagList(
            modalButton("取消"),
            actionButton("confirm_deg_group", "开始计算", class = "btn btn-primary")
          ),
          size = "m",
          easyClose = TRUE
        )
      )
    }, error = function(e) {
      showNotification(paste0("读取失败：", e$message), type = "error", duration = NULL)
    })
  })
  
  # --------------------------------------------------
  # 2️⃣ 用户选择分组列后，弹窗选择 Reference / Compare
  # --------------------------------------------------
  observeEvent(input$confirm_deg_group, {
    req(rv$se, input$deg_group_col)
    removeModal()
    
    tryCatch({
      withProgress(message = "读取 SummarizedExperiment ...", value = 0.1, {
        sel_col <- input$deg_group_col
        se_obj <- rv$se
        
        if (!sel_col %in% colnames(colData(se_obj))) {
          showNotification(paste0("列 ", sel_col, " 不存在！"), type = "error")
          return(NULL)
        }
        
        group_levels <- unique(colData(se_obj)[[sel_col]])
        
        showModal(
          modalDialog(
            title = paste0("选择分组进行比较 (", sel_col, ")"),
            selectInput("ref_group", "Reference group", choices = group_levels, width = "100%"),
            selectInput("cmp_group", "Compare group", choices = group_levels, width = "100%"),
            footer = tagList(
              modalButton("取消"),
              actionButton("confirm_deg_run", "开始计算", class = "btn btn-primary")
            ),
            easyClose = TRUE
          )
        )
      })
    }, error = function(e) {
      showNotification(paste0("分组选择失败：", e$message), type = "error", duration = NULL)
    })
  })
  
  # --------------------------------------------------
  # 3️⃣ 差异分析计算
  # --------------------------------------------------
  observeEvent(input$confirm_deg_run, {
    removeModal()
    
    tryCatch({
      withProgress(message = "开始差异分析 ...", value = 0.1, {
        sel_col <- input$deg_group_col
        ref <- input$ref_group
        cmp <- input$cmp_group
        se_obj <- rv$se
        
        if (ref == cmp) {
          showNotification("Reference 与 Compare 不能相同！", type = "error")
          return(NULL)
        }

        keep_cells <- colData(se_obj)[[sel_col]] %in% c(ref, cmp)
        se_sub <- se_obj[, keep_cells]
        
        
        incProgress(0.3, detail = paste0("计算 ", ref, " vs ", cmp, " ..."))
        pair_on <- !is.null(input$deg_pair_col) && input$deg_pair_test == "YES"
        if (pair_on) {
          message("### Running Paired DEG test ###")
          message("compare_col = ", sel_col)
          message("pair_col    = ", input$deg_pair_col)
          message("ref         = ", ref)
          message("cmp         = ", cmp)
          DEGs_df <- se2DEGs(
            se_sub,
            compare_col = sel_col,
            ref = ref,
            cmp = cmp,
            pair_col = input$deg_pair_col
          )
        } else {
          message("### Running Unpaired DEG test ###")
          message("compare_col = ", sel_col)
          message("ref         = ", ref)
          message("cmp         = ", cmp)
          DEGs_df <- se2DEGs(
            se_sub,
            compare_col = sel_col,
            ref = ref,
            cmp = cmp
          )
        }

        incProgress(0.5, detail = "分析完成")
        
        rv$DEGs <- DEGs_df
      })
      showNotification("计算完成 ✅", type = "message")
    }, error = function(e) {
      showNotification(paste0("计算失败：", e$message), type = "error", duration = NULL)
    })
  })
  
  # --------------------------------------------------
  # 4️⃣ 差异结果表格
  # --------------------------------------------------
  output$DEGs_table <- renderDT({
    req(rv$DEGs)
    datatable(
      rv$DEGs,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(columns = 3:ncol(rv$DEGs), digits = 2)
  })
  
  # --------------------------------------------------
  # 5️⃣ 结果下载
  # --------------------------------------------------
  output$download_DEGs <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(basename(input$Upload_SE2$name)), "_DEGs.xlsx")
    },
    content = function(file) {
      req(rv$DEGs)
      writexl::write_xlsx(rv$DEGs, file)
    }
  )
  
  output$download_DEGs_ui <- renderUI({
    req(rv$DEGs)
    downloadButton("download_DEGs", "下载结果")
  })
}
