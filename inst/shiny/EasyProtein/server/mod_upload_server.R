# ======================================================
# Module: Import Server
# Purpose: upload TSV → build SummarizedExperiment → edit colData → export zip
# Keep compatibility with the existing UI (do not change output IDs).
# ======================================================

mod_upload_server <- function(input, output, session, rv) {

  # -----------------------------
  # 1️⃣ Build SummarizedExperiment
  # -----------------------------
  se_rv <- reactiveVal(NULL)
  un_stable_gene_rv <- reactiveVal(NULL)
  missing_gene_rv <- reactiveVal(NULL)
  qc_context <- reactiveValues(
    exp_path = NULL,
    obs_choices = NULL,
    default_obs = NULL,
    number_of_group = NULL
  )

  open_qc_modal <- function() {
    req(qc_context$exp_path, qc_context$obs_choices, qc_context$default_obs, qc_context$number_of_group)

    showModal(modalDialog(
      title = "Select Observation ID column",
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_obscol", "Confirm", class = "btn btn-primary")
      ),
      selectInput(
        "obscol_select",
        label = "Select the column to use as row names (Observation ID)",
        choices = qc_context$obs_choices,
        selected = qc_context$default_obs
      ),
      helpText("Gene is preferred by default; otherwise the selected column is used."),
      sliderInput(
        inputId = "valid_group_cutoff_threhold",
        label = div(
          style = "display:flex; align-items:center; gap:8px; flex-wrap:nowrap; white-space:nowrap;",
          span("Remove genes with <= N valid groups (default: < 0.5 missing; adjustable in Advanced options)"),
          help_qmark(tagList(
            tags$b("Meaning:"), " a gene is kept only if it has enough condition groups with acceptable missingness.", tags$br(),
            "Default condition: ", tags$b("< 0.5 missing"), ".", tags$br(),
            tags$b("Default -1:"), " do not filter out any gene by this rule."
          ))
        ),
        min = -1,
        max = length(qc_context$number_of_group),
        value = -1,
        step = 1
      ),
      sliderInput(
        inputId = "valid_cv_cutoff_threhold",
        label = div(
          style = "display:flex; align-items:center; gap:8px; flex-wrap:nowrap; white-space:nowrap;",
          span("Remove genes with <= N stable groups (default: < 0.5 CV; adjustable in Advanced options)"),
          help_qmark(tagList(
            tags$b("Meaning:"), " a gene is kept only if it is sufficiently stable across condition groups.", tags$br(),
            "Default condition: ", tags$b("< 0.5 CV"), ".", tags$br(),
            tags$b("Default -1:"), " do not filter out any gene by this rule."
          ))
        ),
        min = -1,
        max = length(qc_context$number_of_group),
        value = -1,
        step = 1
      ),
      checkboxInput(
        "enable_detect_outlier",
        label = div(
          style = "display:flex; align-items:center; gap:8px; flex-wrap:nowrap; white-space:nowrap;",
          span("Enable detect outlier in replicates"),
          help_qmark(tagList(
            tags$b("Meaning:"), " detect extreme replicate-level values within each condition (>5 fold change) and mask them as missing before downstream filtering/imputation.", tags$br(),
            "This is useful when a single replicate is obviously inconsistent with the rest of the group."
          ))
        ),
        value = FALSE
      ),
      tags$details(
        style = "margin: 8px 0 12px 0;",
        tags$summary(
          style = "cursor:pointer; font-weight:600; color:#2563EB; outline:none;",
          "Advanced options"
        ),
        div(
          class = "ep-soft-panel",
          style = "margin-top:10px;",
          sliderInput(
            inputId = "frac_NA_threshold",
            label = div(
              style = "display:flex; align-items:center; gap:8px; flex-wrap:nowrap; white-space:nowrap;",
              span("Missing-value threshold (frac_NA_threshold)"),
              help_qmark(tagList(
                tags$b("Meaning:"), " a condition is considered valid only when its missing-value fraction is below this cutoff.", tags$br(),
                "Lower values make the missing-value filter stricter.", tags$br(),
                tags$b("Default:"), " 0.5"
              ))
            ),
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.05
          ),
          sliderInput(
            inputId = "cv_threshold",
            label = div(
              style = "display:flex; align-items:center; gap:8px; flex-wrap:nowrap; white-space:nowrap;",
              span("Stability threshold (cv_threshold)"),
              help_qmark(tagList(
                tags$b("Meaning:"), " a condition is counted as stable only when its coefficient of variation is below this cutoff.", tags$br(),
                "Lower values make the stability filter stricter.", tags$br(),
                tags$b("Default:"), " 0.5"
              ))
            ),
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.05
          )
        )
      )
    ))

    session$onFlushed(function() {
      updateSliderInput(session, "valid_group_cutoff_threhold", value = -1)
      updateSliderInput(session, "valid_cv_cutoff_threhold", value = -1)
      updateCheckboxInput(session, "enable_detect_outlier", value = FALSE)
      updateSliderInput(session, "frac_NA_threshold", value = 0.5)
      updateSliderInput(session, "cv_threshold", value = 0.5)
    }, once = TRUE)
  }

  observeEvent(input$upload_tsv, {
    req(input$upload_tsv)
    exp_path <- input$upload_tsv$datapath
    is_tsv <- tolower(tools::file_ext(input$upload_tsv$name)) %in% c("tsv")

    if (!is_tsv) {
      showNotification("Invalid file type: .tsv required.", type = "error", duration = NULL)
      return(NULL)
    }

    tryCatch({
      cols_preview <- colnames(data.table::fread(exp_path, nrows = 1))
      obs_choices <- cols_preview[!stringr::str_detect(cols_preview, "raw")]
      default_obs <- if ("Genes" %in% obs_choices) "Genes" else obs_choices[1]
      number_of_sample <- cols_preview[stringr::str_detect(cols_preview, "raw")]
      number_of_group <- unique(stringr::str_extract(number_of_sample, "\\w+(?=_[^_]*$)"))
      qc_context$exp_path <- exp_path
      qc_context$obs_choices <- obs_choices
      qc_context$default_obs <- default_obs
      qc_context$number_of_group <- number_of_group

      open_qc_modal()

    }, error = function(e) {
      showNotification(paste0("Load failed: ", e$message), type = "error", duration = NULL)
    })
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_obscol, {
    req(qc_context$exp_path)
    removeModal()
    withProgress(message = "Building SummarizedExperiment ...", value = 0.1, {
      incProgress(0.3, detail = "Reading and parsing file")
      se_list <- rawdata2se(
        exp_file = qc_context$exp_path,
        obs_col = input$obscol_select,
        enable_detect_outlier_gene = input$enable_detect_outlier,
        frac_NA_threshold = input$frac_NA_threshold,
        min_valid_groups = input$valid_group_cutoff_threhold,
        cv_threshold = input$cv_threshold,
        min_stable_groups = input$valid_cv_cutoff_threhold
      )

      incProgress(0.5, detail = "Done")
    })
    se_rv(se_list$se)
    missing_gene_rv(se_list$missing_gene_df)
    un_stable_gene_rv(se_list$un_stable_gene)

    rv$se <- se_rv()
    showNotification(paste0("Data loaded ✅ (column: ", input$obscol_select, ")"), type = "message")
  }, ignoreInit = TRUE)

  output$reset_qc_params_ui <- renderUI({
    req(se_rv())
    div(
      style = "margin-top: 12px;",
      actionButton("reset_qc_params", "Reset QC parameters", icon = icon("rotate-left"), class = "btn btn-outline-primary")
    )
  })

  observeEvent(input$reset_qc_params, {
    req(se_rv(), qc_context$exp_path)
    open_qc_modal()
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
  actionButton("edit_coldata", "Edit colData", icon = icon("pen-to-square"))
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
    if (!load_module_packages("readxl")) {
      return()
    }
    library(readxl)

    new_meta <- readxl::read_excel(input$meta_excel$datapath)
    if (ncol(new_meta) < 2) {
      showNotification("❌ Excel must contain at least one sample column and one metadata column!", type = "error")
      return(NULL)
    }

    se <- se_rv()
    new_meta <- as.data.frame(new_meta)
    colnames(new_meta)[1] <- "sample"

    # ---- Reorder by sample name ----
    new_meta <- new_meta[match(colnames(se), new_meta$sample), ]

    # ---- Remove duplicate columns ----
    new_cols <- setdiff(colnames(new_meta), c("sample", colnames(colData(se))))

    if (length(new_cols) == 0) {
      showNotification("No new columns to add (all columns already exist).", type = "message")
      return(NULL)
    }

    # ---- Add new columns ----
    for (col in new_cols) {
      se[[col]] <- new_meta[[col]]
    }

    # ✅ Update reactiveVal
    se_rv(se)
    rv$se <- se  # ensure global state stays in sync

    # ✅ Update edit table to avoid overwriting on save
    df_new <- as.data.frame(S4Vectors::DataFrame(
      cell_id = colnames(se),
      colData(se)
    ))
    edit_df(df_new)  # keep display and saved content in sync

    showNotification(paste("✅ Added columns:", paste(new_cols, collapse = ", ")), type = "message")
  })




  # ---- Edit table cells ----
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

  # ---- Save changes and write back to SummarizedExperiment ----
  observeEvent(input$save_coldata, {
    req(edit_df())
    df <- edit_df()

    cell_ids <- df[[1]]
    new_cd   <- df[, -1, drop = FALSE]

    sce <- se_rv()
    if (!setequal(cell_ids, colnames(sce))) {
      showNotification("cell_id does not match SCE column names; auto-aligned by SCE column order.", type = "warning")
    }

    new_cd_df <- S4Vectors::DataFrame(new_cd, row.names = cell_ids)
    new_cd_df <- new_cd_df[colnames(sce), , drop = FALSE]

    colData(sce) <- new_cd_df
    se_rv(sce)
  rv$se <- sce  # ✅ Update global rv

    removeModal()
  showNotification("Saved: colData updated.", type = "message")

    output$coldata_preview <- renderDT({
      df2 <- as.data.frame(S4Vectors::DataFrame(
        cell_id = colnames(se_rv()),
        colData(se_rv())
      ))
      datatable(df2, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })
  })

  # -----------------------------
  # 4️⃣ Export zip
  # -----------------------------
  output$download_se <- make_download_se_qc_zip(
    se_reactive   = se_rv,
    miss_gene     = missing_gene_rv,
    unstable_gene = un_stable_gene_rv,
    input         = input,
    file_input_id = "upload_tsv",
    suffix        = "",
    session       = session
  )
}
