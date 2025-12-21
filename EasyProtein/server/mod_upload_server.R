# ======================================================
# Module: Import Server
# 功能：上传 TSV → 构建 SummarizedExperiment → 编辑 colData → 导出 zip
# 兼容原 UI，不改任何输出 ID。
# ======================================================

mod_upload_server <- function(input, output, session, rv) {
  
  # -----------------------------
  # 1️⃣ SummarizedExperiment 构建
  # -----------------------------
  se_rv <- reactiveVal(NULL)
  un_stable_gene_rv <- reactiveVal(NULL)
  missing_gene_rv <- reactiveVal(NULL)
  
  observeEvent(input$upload_tsv, {
    req(input$upload_tsv)
    exp_path <- input$upload_tsv$datapath
    is_tsv <- tolower(tools::file_ext(input$upload_tsv$name)) %in% c("tsv")
    
    if (!is_tsv) {
      showNotification("文件类型不符合要求：需要 .tsv ", type = "error", duration = NULL)
      return(NULL)
    }
    
    tryCatch({
      
      cols_preview <- colnames(data.table::fread(exp_path, nrows = 1))
      obs_choices <- cols_preview[!stringr::str_detect(cols_preview, "raw")]
      default_obs <- if ("Protein.Ids" %in% obs_choices) "Protein.Ids" else obs_choices[1]
      number_of_sample <- cols_preview[stringr::str_detect(cols_preview, "raw")]
      number_of_group <- unique(stringr::str_extract(number_of_sample, "\\w+(?=_[^_]*$)")) 
      showModal(modalDialog(
        title = "选择 Observation ID 列",
        size = "m",
        easyClose = FALSE,
        footer = tagList(
          modalButton("取消"),
          actionButton("confirm_obscol", "确认", class = "btn btn-primary")
        ),
        
        selectInput(
          "obscol_select",
          label = "请选择用于作为行名的列（Observation ID）",
          choices = obs_choices,
          selected = default_obs
        ),
        helpText("默认优先使用 Protein.Ids，如无该列则使用第一列。"),
        sliderInput(
          inputId = "valid_group_cutoff_threhold",
          label   = "Remove genes with ≤ N valid groups ( < 0.5 missing)",
          min     = -1,
          max     = length(number_of_group),
          value   = -1,
          step    = 1
        ),
        checkboxInput("enable_detect_outlier", "Enable detect outlier in replicates", value = FALSE),
        sliderInput(
          inputId = "valid_cv_cutoff_threhold",
          label   = "Remove genes with ≤ N stable groups (CV < 0.5)",
          min     = -1,
          max     = length(number_of_group),
          value   = -1,
          step    = 1
        )
      ))
      
      # ---- 确认按钮后执行构建 SummarizedExperiment ----
      observeEvent(input$confirm_obscol, {
        removeModal()
        withProgress(message = "构建 SummarizedExperiment ...", value = 0.1, {
          incProgress(0.3, detail = "读取与解析文件")
          se_list <- rawdata2se(
            exp_file = exp_path,
            obs_col = input$obscol_select,
            enable_detect_outlier_gene = input$enable_detect_outlier,
            valid_group_cutoff = input$valid_group_cutoff_threhold,
            stable_group_cutoff = input$valid_cv_cutoff_threhold
          )
          
          incProgress(0.5, detail = "完成")
        })
        se_rv(se_list$se)
        missing_gene_rv(se_list$missing_gene_df)
        un_stable_gene_rv(se_list$un_stable_gene)
        
        rv$se <- se_rv() 
        showNotification(paste0("数据载入完成 ✅ (使用列: ", input$obscol_select, ")"), type = "message")
      }, once = TRUE)  # 防止重复绑定
      
    }, error = function(e) {
      showNotification(paste0("载入失败：", e$message), type = "error", duration = NULL)
    })
  }, ignoreInit = TRUE)
  
  

  output$se_summary <- renderPrint({
    req(se_rv())
    se <- se_rv()
    cat("SummarizedExperiment ：\n")
    print(se)
    if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
      cat("\nassays:", paste0(names(SummarizedExperiment::assays(se)), collapse = ", "), "\n")
    }
    cd <- as.data.frame(colData(se))
    cat('Group information:\n')
    cat(
      'Number of genes with too many missing values:', nrow(missing_gene_rv()), '\n',
      'Number of unstable genes (high CV):', nrow(un_stable_gene_rv()), '\n'
    )
    for (grp in unique(cd$condition)) {
      cat("Condition:", grp, "\n")
      cat(cd$sample[cd$condition == grp], sep = " ")
      cat("\n\n")
    }
  })
  

  edit_df <- reactiveVal(NULL)
  
  output$edit_coldata_ui <- renderUI({
    req(se_rv())
    actionButton("edit_coldata", "编辑 colData", icon = icon("pen-to-square"))
  })
  
  observeEvent(input$edit_coldata, {
    df <- as.data.frame(S4Vectors::DataFrame(
      cell_id = colnames(se_rv()),
      colData(se_rv())
    ))
    edit_df(df)
    
    showModal(modalDialog(
      title = "Edit colData",
      size = "l",
      easyClose = FALSE,
      fileInput("meta_excel", "Upload metadata Excel", 
                accept = c(".xlsx", ".xls")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_coldata", "save", class = "btn btn-primary")
      ),
      helpText("The first column cannot be modified "),
      DTOutput("coldata_table"),
      br(),
      uiOutput("download_coldata_ui")  
    ))
  })
  
  output$coldata_table <- renderDT({
    req(edit_df())
    datatable(
      edit_df(),
      options = list(scrollX = TRUE, pageLength = 10),
      rownames = FALSE,
      editable = list(
        target  = "cell",
        disable = list(columns = c(1))
      )
    )
  })
  output$download_coldata_ui <- renderUI({
    req(edit_df())
    downloadButton("download_coldata_excel", "📥 Download colData (Excel)")
  })
  
 
  output$download_coldata_excel <- downloadHandler(
    filename = function() {
      paste0("colData_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(edit_df())
      library(openxlsx)
      openxlsx::write.xlsx(edit_df(), file, rowNames = FALSE)
    }
  )

  observeEvent(input$meta_excel, {
    req(input$meta_excel)
    library(readxl)
    
    new_meta <- readxl::read_excel(input$meta_excel$datapath)
    if (ncol(new_meta) < 2) {
      showNotification("❌ Excel 至少需要一列样本名和一列 metadata！", type = "error")
      return(NULL)
    }
    
    se <- se_rv()
    new_meta <- as.data.frame(new_meta)
    colnames(new_meta)[1] <- "sample"
    
    # ---- 按样本名重新排序 ----
    new_meta <- new_meta[match(colnames(se), new_meta$sample), ]
    
    # ---- 去除重复列 ----
    new_cols <- setdiff(colnames(new_meta), c("sample", colnames(colData(se))))
    
    if (length(new_cols) == 0) {
      showNotification("没有可加入的新列（所有列都已存在）", type = "message")
      return(NULL)
    }
    
    # ---- 加入新列 ----
    for (col in new_cols) {
      se[[col]] <- new_meta[[col]]
    }
    
    # ✅ 更新 reactiveVal
    se_rv(se)
    rv$se <- se  # 确保全局同步
    
    # ✅ 同时更新编辑表格（防止 save 时被覆盖）
    df_new <- as.data.frame(S4Vectors::DataFrame(
      cell_id = colnames(se),
      colData(se)
    ))
    edit_df(df_new)  # 更新显示与保存内容同步
    
    showNotification(paste("✅ 成功添加列：", paste(new_cols, collapse = ", ")), type = "message")
  })
  
  
  
  
  # ---- 编辑表格单元格 ----
  observeEvent(input$coldata_table_cell_edit, {
    info <- input$coldata_table_cell_edit
    df <- isolate(edit_df())
    
    i <- info$row
    j <- info$col + 1
    if (j == 1) return()
    df[i, j] <- DT::coerceValue(info$value, df[i, j])
    
    edit_df(df)
    replaceData(dataTableProxy("coldata_table"), df, resetPaging = FALSE, rownames = FALSE)
  })
  
  # ---- 保存修改并写回 SummarizedExperiment ----
  observeEvent(input$save_coldata, {
    req(edit_df())
    df <- edit_df()
    
    cell_ids <- df[[1]]
    new_cd   <- df[, -1, drop = FALSE]
    
    sce <- se_rv()
    if (!setequal(cell_ids, colnames(sce))) {
      showNotification("cell_id 与 SCE 列名不一致，已按 SCE 列名顺序自动对齐。", type = "warning")
    }
    
    new_cd_df <- S4Vectors::DataFrame(new_cd, row.names = cell_ids)
    new_cd_df <- new_cd_df[colnames(sce), , drop = FALSE]
    
    colData(sce) <- new_cd_df
    se_rv(sce)
    rv$se <- sce  # ✅ 更新全局 rv
    
    removeModal()
    showNotification("已保存：colData 已更新。", type = "message")
    
    output$coldata_preview <- renderDT({
      df2 <- as.data.frame(S4Vectors::DataFrame(
        cell_id = colnames(se_rv()),
        colData(se_rv())
      ))
      datatable(df2, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })
  })
  
  # -----------------------------
  # 4️⃣ 导出 zip
  # -----------------------------
  output$download_se <- make_download_se_qc_zip(
    se_reactive   = se_rv,
    miss_gene     = missing_gene_rv,
    unstable_gene = un_stable_gene_rv,
    input         = input,
    file_input_id = "upload_tsv",
    suffix        = ""
  )
}
