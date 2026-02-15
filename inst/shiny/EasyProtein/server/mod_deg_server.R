# ======================================================
# Module: DEG Server
# Purpose: read SE object → choose grouping/reference/compare → run DE → output table + download
# Keep compatibility with existing UI (IDs unchanged).
# ======================================================

mod_deg_server <- function(input, output, session, rv) {
  
  # --------------------------------------------------
  # 1️⃣ Upload SummarizedExperiment
  # --------------------------------------------------
  observeEvent(input$Upload_SE2, {
    req(input$Upload_SE2)
    se_path <- input$Upload_SE2$datapath
    
    tryCatch({
      se <- readRDS(se_path)
  rv$se <- se   # ✅ write to global rv$se
  showNotification("File loaded ✅", type = "message")
      
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
          # ---- donor column
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
            modalButton("Cancel"),
            actionButton("confirm_deg_group", "Start", class = "btn btn-primary")
          ),
          size = "m",
          easyClose = TRUE
        )
      )
    }, error = function(e) {
      showNotification(paste0("Load failed: ", e$message), type = "error", duration = NULL)
    })
  })
  
  # --------------------------------------------------
  # 2️⃣ After choosing grouping column, select reference/compare
  # --------------------------------------------------
  observeEvent(input$confirm_deg_group, {
    req(rv$se, input$deg_group_col)
    removeModal()
    
    tryCatch({
  withProgress(message = "Loading SummarizedExperiment ...", value = 0.1, {
        sel_col <- input$deg_group_col
        se_obj <- rv$se
        
        if (!sel_col %in% colnames(colData(se_obj))) {
          showNotification(paste0("Column ", sel_col, " does not exist!"), type = "error")
          return(NULL)
        }
        
        group_levels <- unique(colData(se_obj)[[sel_col]])
        
        showModal(
          modalDialog(
            title = paste0("Select groups to compare (", sel_col, ")"),
            selectInput("ref_group", "Reference group", choices = group_levels, width = "100%"),
            selectInput("cmp_group", "Compare group", choices = group_levels, width = "100%"),
            sliderInput(
              "deg_logFC_cutoff",
              "logFC cutoff",
              min = 0,
              max = 10,
              value = 1,
              step = 0.1
            ),
            sliderInput(
              "deg_adj_p_cutoff",
              "Adjusted p-value cutoff",
              min = 0,
              max = 0.1,
              value = 0.05,
              step = 0.001
            ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton("confirm_deg_run", "Start", class = "btn btn-primary")
            ),
            easyClose = TRUE
          )
        )
      })
    }, error = function(e) {
      showNotification(paste0("Group selection failed: ", e$message), type = "error", duration = NULL)
    })
  })
  
  # --------------------------------------------------
  # 3️⃣ Differential analysis
  # --------------------------------------------------
  observeEvent(input$confirm_deg_run, {
    removeModal()
    
    tryCatch({
  withProgress(message = "Running differential analysis ...", value = 0.1, {
        sel_col <- input$deg_group_col
        ref <- input$ref_group
        cmp <- input$cmp_group
        se_obj <- rv$se
        
        if (ref == cmp) {
          showNotification("Reference and Compare cannot be the same!", type = "error")
          return(NULL)
        }

        keep_cells <- colData(se_obj)[[sel_col]] %in% c(ref, cmp)
        se_sub <- se_obj[, keep_cells]
        
        
  incProgress(0.3, detail = paste0("Computing ", ref, " vs ", cmp, " ..."))
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
            pair_col = input$deg_pair_col,
            logFC_cutoff = input$deg_logFC_cutoff,
            adj_p_cutoff = input$deg_adj_p_cutoff
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
            cmp = cmp,
            logFC_cutoff = input$deg_logFC_cutoff,
            adj_p_cutoff = input$deg_adj_p_cutoff
          )
        }

  incProgress(0.5, detail = "Done")
        
        rv$DEGs <- DEGs_df
      })
      showNotification("Completed ✅", type = "message")
    }, error = function(e) {
      showNotification(paste0("Computation failed: ", e$message), type = "error", duration = NULL)
    })
  })
  
  # --------------------------------------------------
  # 4️⃣ Differential result table
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
  # 5️⃣ Download results
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
    downloadButton("download_DEGs", "Download results")
  })
}
