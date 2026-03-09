mod_tools_server <- function(input, output, session) {
  rv <- reactiveValues(se = NULL, se_sub = NULL, corr_se = NULL, corr_result = NULL)
  observeEvent(input$upload_subset_se, {
    req(input$upload_subset_se)
    rv$se <- readRDS(input$upload_subset_se$datapath)
  })

  observeEvent(input$upload_corr_se, {
    req(input$upload_corr_se)

    se_obj <- tryCatch(readRDS(input$upload_corr_se$datapath), error = function(e) NULL)
    validate(need(!is.null(se_obj), "Failed to read Rds, please check file format."))

    rv$corr_se <- se_obj
    rv$corr_result <- NULL

    showModal(
      modalDialog(
        title = "Upload target gene list and choose correlation method",
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_corr_analysis", "Run correlation", class = "btn btn-primary")
        ),
        fileInput(
          "corr_gene_excel_upload",
          "Upload Excel file containing target genes",
          multiple = FALSE,
          accept = c(".xls", ".xlsx")
        ),
        helpText("The first column of the Excel file will be used as the target gene list."),
        selectInput(
          "corr_method",
          "Correlation method",
          choices = c("spearman", "pearson", "kendall"),
          selected = "spearman"
        )
      )
    )
  })

  output$open_selector_UI <- renderUI({
    req(rv$se)
    actionButton("open_selector", "Subset SE by samples and genes")
  })

  observeEvent(input$open_selector, {
    req(rv$se)
    show_se_selector_modal(rv$se)
    make_grouped_select(id = "se_col_selector", df = as.data.frame(colData(rv$se)),label = 'Select coldata columns',default_all= TRUE)$server(input, output, session)
    make_grouped_select(id = "se_row_selector", df = as.data.frame(rowData(rv$se)),label = 'Select coldata columns',default_all= TRUE)$server(input, output, session)
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
      if (!load_module_packages("readxl")) {
        return()
      }
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
    if (length(final_row_idx) == 0) {
      final_row_idx <- seq_len(nrow(rv$se))
    }

    if (length(col_idx) == 0) {
      showNotification("No samples selected after filtering. Please revise your selection.", type = "error")
      return(invisible(NULL))
    }

    rv$se_sub <- rv$se[final_row_idx, col_idx, drop = FALSE]

    removeModal()
  })



  output$subset_info <- renderPrint({
    req(rv$se_sub)

    se <- rv$se_sub
    cat("SummarizedExperiment :\n")
    print(se)
    if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
      cat("\nassays:", paste0(names(SummarizedExperiment::assays(se)), collapse = ", "), "\n")
    }
    cd <- as.data.frame(rowData(se))
    for (grp in unique(cd$gene_group)) {
      cat(grp,"gene number:", length(cd$gene_group[cd$gene_group == grp]), "\n")
    }
  })



  output$download_tools_subset_se <- make_download_se_zip(
    se_reactive   = reactive(rv$se_sub),
    input         = input,
    file_input_id = "upload_subset_se",
    suffix        = "_subset",
    session       = session
  )
  output$download_subset_se_UI <- renderUI({
    req(rv$se_sub)
    downloadButton("download_tools_subset_se", "Download SE")
  })

  observeEvent(input$confirm_corr_analysis, {
    req(rv$corr_se, input$corr_gene_excel_upload)

    if (!load_module_packages(c("readxl", "writexl"))) {
      return()
    }

    gene_df <- tryCatch(
      readxl::read_excel(input$corr_gene_excel_upload$datapath),
      error = function(e) {
        showNotification("Failed to read Excel file", type = "error")
        NULL
      }
    )

    if (is.null(gene_df) || nrow(gene_df) == 0 || ncol(gene_df) == 0) {
      showNotification("Empty or invalid Excel file.", type = "error")
      return(invisible(NULL))
    }

    target_genes <- unique(trimws(as.character(gene_df[[1]])))
    target_genes <- target_genes[!is.na(target_genes) & nzchar(target_genes)]

    if (length(target_genes) == 0) {
      showNotification("No valid genes found in the first Excel column.", type = "error")
      return(invisible(NULL))
    }

    corr_res <- tryCatch({
      corr_df <- correlate_genes_to_target(
        se = rv$corr_se,
        gene = target_genes,
        assay_name = "conc",
        method = input$corr_method
      )

      meta <- as.data.frame(colData(rv$corr_se))
      conc_df <- if (ncol(meta) > 0) {
        group_by <- if ("condition" %in% colnames(meta)) "condition" else colnames(meta)[1]
        se2conc(rv$corr_se, group_by = group_by)
      } else {
        cbind(
          as.data.frame(rowData(rv$corr_se)),
          as.data.frame(assay(rv$corr_se, "conc"))
        )
      }

      if (!"gene" %in% colnames(conc_df)) {
        conc_df$gene <- rownames(rv$corr_se)
      }

      dplyr::left_join(corr_df, conc_df, by = "gene")
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })

    req(corr_res)
    rv$corr_result <- corr_res
    removeModal()
  })

  output$corr_gene_table <- renderTable({
    req(rv$corr_result)
    head(rv$corr_result[, 1:2, drop = FALSE], 10)
  }, rownames = FALSE)

  output$download_corr_gene <- downloadHandler(
    filename = function() {
      base <- tools::file_path_sans_ext(basename(input$upload_corr_se$name))
      paste0(base, "_correlated_genes.xlsx")
    },
    content = function(file) {
      req(rv$corr_result)
      writexl::write_xlsx(rv$corr_result, file)
    }
  )

  output$download_corr_gene_UI <- renderUI({
    req(rv$corr_result)
    downloadButton("download_corr_gene", "Download full table")
  })



}
