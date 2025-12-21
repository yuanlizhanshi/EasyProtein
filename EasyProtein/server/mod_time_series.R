mod_time_series_server <- function(input, output, session) {
  se_data   <- reactiveVal(NULL)
  se_filtered <- reactiveVal(NULL)
  mat_data  <- reactiveVal(NULL)
  row_info  <- reactiveVal(NULL)
  col_info  <- reactiveVal(NULL)
  pdf_path  <- reactiveVal(NULL)
  param_confirmed <- reactiveVal(FALSE)
  observeEvent(input$time_rds_file, {
    req(input$time_rds_file)
    se <- readRDS(input$time_rds_file$datapath)
    se_data(se)
    show_time_series_param(se_data())
  })
  
  
  observeEvent(input$confirm_time_params,{
      req(se_data())
      removeModal()
      
      # print(input[["time_filter-time_threshold"]])
      # print(input[["time_filter-min_expression_threshold"]])
      # print(input[["time_filter-CV_with_time_threshold"]])
      # print(input[["time_filter-CV_with_time_threshold"]])
      # print(input[["time_filter-padj_threshold"]])
      
      sel <- input$selected_cols 
      col_name <- sub("\\|\\|.*$", "", sel[1])  
      vals <- sub("^.*\\|\\|", "", sel)        
      col_df <- as.data.frame(colData(se_data()))
      matched_samples <- rownames(col_df)[col_df[[col_name]] %in% vals]
      
      if (length(matched_samples) == 0) {
        showNotification("No samples matched your selection", type = "error")
        return()
      }
      se_sub <- se_data()[, matched_samples, drop = FALSE]
      
      
      new_se <- se2gene_group(
        se = se_sub,
        group_by = input$coldata_col_selector,
        cor_method = input[["time_filter-cor_method"]], 
        time_threhold = input[["time_filter-time_threshold"]],
        min_expresion_threhold = input[["time_filter-min_expression_threshold"]],
        CV_with_time_threhold = input[["time_filter-CV_with_time_threshold"]],
        padj_threhold = input[["time_filter-padj_threshold"]]
      )
      
      se_filtered(new_se)
      param_confirmed(TRUE)
      #print(table(rowData(se_data())['gene_group']))
    }
  )
    output$se_summary_time <- renderPrint({
      req(se_filtered(), param_confirmed())

      se <- se_filtered()
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
    
    output$FC_fold_plot <- renderPlot({
      req(se_filtered(), param_confirmed())
      se <- se_filtered()
      plot_FC_trend(se)
    },
    height = function() input$FC_plot_height,
    width = function() input$FC_plot_width)
    
    output$download_FC_fold_plot_pdf <- make_download_pdf(
      plot_expr   = function() plot_FC_trend(se_filtered()),
      input       = input,
      suffix      = "FC_trend",
      width       = function() input$FC_plot_width / 100,
      height      = function() input$FC_plot_height / 100,
      input_field = "time_rds_file"
    )
    
    
    output$download_time_se <- make_download_time_se_zip(
      se_reactive   = se_filtered,   
      input         = input,
      file_input_id = "time_rds_file",     
      suffix        = "_timeseries"   
    )
    
    output$download_time_se_UI <- renderUI({
      req(se_filtered())
      downloadButton("download_time_se", "Download SE")
    })
    


}