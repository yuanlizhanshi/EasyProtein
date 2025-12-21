mod_tools_server <- function(input, output, session) {
  rv <- reactiveValues(se = NULL, se_sub = NULL)
  observeEvent(input$upload_subset_se, {
    req(input$upload_subset_se)
    rv$se <- readRDS(input$upload_subset_se$datapath)
  })
  
  output$open_selector_UI <- renderUI({
    req(rv$se)
    actionButton("open_selector", "Subset SE by samples and genes")
  })
  
  observeEvent(input$open_selector, {
    req(rv$se)
    show_se_selector_modal(rv$se)
    
    make_grouped_select("se_col_selector", as.data.frame(colData(rv$se)), TRUE)$server(input, output, session)
    make_grouped_select("se_row_selector", as.data.frame(rowData(rv$se)), TRUE)$server(input, output, session)
    
    
  })
  

  
  
  observeEvent(input$confirm_se_index, {
    req(rv$se)
    
   
    col_vals <- input$se_col_selector
    
    if (is.null(col_vals) || length(col_vals) == 0) {
      col_idx <- seq_len(ncol(rv$se))
    } else {
      col_group  <- unique(sub("\\|\\|.*$", "", col_vals))
      if (length(col_group) > 1) {
        stop("Only one colData column should be used in se_col_selector.")
      }
      col_values <- sub("^.*\\|\\|", "", col_vals)
      col_idx <- which(colData(rv$se)[[col_group]] %in% col_values)
    }
    
    row_vals <- input$se_row_selector
    
    if (is.null(row_vals) || length(row_vals) == 0) {
      row_idx <- integer(0)
    } else {
      row_group  <- unique(sub("\\|\\|.*$", "", row_vals))
      if (length(row_group) > 1) {
        stop("Only one rowData column should be used in se_row_selector.")
      }
      row_values <- sub("^.*\\|\\|", "", row_vals)
      row_idx <- which(rowData(rv$se)[[row_group]] %in% row_values)
    }
    
    excel_row_idx <- integer(0)
    
    if (!is.null(input$se_excel_upload)) {
      ext <- tools::file_ext(input$se_excel_upload$name)
      if (ext %in% c("xls", "xlsx")) {
        excel_path <- input$se_excel_upload$datapath
        df_excel   <- readxl::read_excel(excel_path, col_names = TRUE)
        
        if (ncol(df_excel) >= 1) {
          excel_vec <- as.character(df_excel[[1]])
    
          excel_row_idx <- which(rownames(rv$se) %in% excel_vec)
        }
      }
    }
    

    final_row_idx <- sort(unique(c(row_idx, excel_row_idx)))
    
    rv$se_sub <- rv$se[final_row_idx, col_idx, drop = FALSE]
    
    removeModal()
  })
  
  

  output$subset_info <- renderPrint({
    req(rv$se_sub)
    
    se <- rv$se_sub
    cat("SummarizedExperiment ：\n")
    print(se)
    if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
      cat("\nassays:", paste0(names(SummarizedExperiment::assays(se)), collapse = ", "), "\n")
    }
    cd <- as.data.frame(rowData(se))
    for (grp in unique(cd$gene_group)) {
      cat(grp,"gene number:", length(cd$gene_group[cd$gene_group == grp]), "\n")
    }
  })
  

  
  output$download_tools_subset_se <- make_download_time_se_zip(
    se_reactive   = reactive(rv$se_sub),   
    input         = input,
    file_input_id = "upload_subset_se",     
    suffix        = "_subset"   
  )
  output$download_subset_se_UI <- renderUI({
    req(rv$se_sub)
    downloadButton("download_tools_subset_se", "Download SE")
  })
  
  
  
}