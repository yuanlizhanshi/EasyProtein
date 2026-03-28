# ======================================================
# Module: Visualization Server
# Purpose: visualize differential analysis results (Volcano plot + GO dotplot)
# Keep all UI output IDs and logic unchanged
# ======================================================

mod_visualization_server <- function(input, output, session) {

  # =====================================================
  # 1️⃣ Volcano plot section
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
      gene_col      = "gene",
      pval_cutoff   = input$Volcano_FDR,
      logFC_cutoff  = input$Volcano_log2FC,
      top_n         = input$Volcano_topN
    )
  })

  output$volcano <- ggiraph::renderGirafe({
    req(DEGs_rv2())
    make_vol_plot()$ggiraph_obj
  })

  output$download_vol_pdf <- make_download_pdf(
    plot_expr   = function() make_vol_plot()$ggobj,
    input       = input,
    suffix      = "volcano",
    width       = 8,
    height      = 6,
    input_field = "Upload_DEG_table1"
  )



  # =====================================================
  # 2️⃣ GO enrichment dotplot section
  # =====================================================
  go_table_info <- reactive({
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

    if ("ONTOLOGY" %in% names(df)) {
      table_type <- "GO"
      entry_col <- "ONTOLOGY"
    } else if ("category" %in% names(df)) {
      table_type <- "KEGG"
      entry_col <- "category"
    } else {
      validate(
        need(FALSE, "Cannot detect enrichment type. GO requires column 'ONTOLOGY'; KEGG requires column 'category'.")
      )
    }

    entry_vals <- unique(as.character(df[[entry_col]]))
    entry_vals <- entry_vals[!is.na(entry_vals) & trimws(entry_vals) != ""]
    entry_vals <- sort(entry_vals)

    list(
      df = df,
      table_type = table_type,
      entry_col = entry_col,
      entry_choices = c("All", entry_vals)
    )
  })

  output$go_entry_selector_ui <- renderUI({
    req(go_table_info())
    info <- go_table_info()
    selectInput(
      inputId = "go_entry_choice",
      label = paste0("Select ", info$table_type, " entry"),
      choices = info$entry_choices,
      selected = "All"
    )
  })

  go_df <- reactive({
    req(go_table_info())
    info <- go_table_info()
    df <- info$df

    if (!is.null(input$go_entry_choice) && input$go_entry_choice != "All") {
      df <- df[df[[info$entry_col]] %in% input$go_entry_choice, , drop = FALSE]
    }

    validate(need(nrow(df) > 0, "No enrichment terms left after filtering."))
    df
  })

  go_entry_suffix_tag <- reactive({
    choice <- if (is.null(input$go_entry_choice)) "All" else input$go_entry_choice
    if (!nzchar(choice)) choice <- "All"

    tag <- toupper(trimws(as.character(choice)))
    tag <- gsub("[^A-Z0-9]+", "_", tag)
    tag <- gsub("^_+|_+$", "", tag)
    if (!nzchar(tag)) tag <- "ALL"
    paste0(".", tag)
  })

  enrich_type_tag <- reactive({
    req(go_table_info())
    toupper(go_table_info()$table_type)
  })

  sanitize_pdf_filename <- function(x) {
    x <- as.character(x)[1]
    if (is.na(x) || !nzchar(x)) x <- "plot"
    x <- gsub("[\\\\/:*?\"<>|]", "_", x)
    x <- gsub("\\s+", " ", trimws(x))
    if (!nzchar(x)) x <- "plot"
    x
  }

  go_pdf_filename <- reactive({
    req(go_table_info())
    info <- go_table_info()
    type_label <- if (identical(info$table_type, "KEGG")) "KEGG" else "GO"
    entry_label <- if (is.null(input$go_entry_choice) || !nzchar(input$go_entry_choice)) {
      "All"
    } else {
      as.character(input$go_entry_choice)
    }

    raw_name <- paste0(
      "Top ", input$go_topn,
      " terms of ", type_label,
      " function enrichment plot in ", entry_label,
      " category"
    )
    sanitize_pdf_filename(raw_name)
  })


  output$GO_enrich_plot1 <- renderPlot({
    req(go_df())
    plot_GO_dot1(
      go_df(),
      topn = input$go_topn,
      label_format = input$go_label_width,
      group_by = input$go_group_by
    )
  },
  height = function() input$go_plot_height,
  width = function() input$go_plot_width)

  output$GO_enrich_plot2 <- renderPlot({
    req(go_df())
    plot_GO_dot2(
      go_df(),
      topn = input$go_topn,
      label_format = input$go_label_width,
      group_by = input$go_group_by,
      x_axis = input$go_x_axis
    )
  },
  height = function() input$go_plot_height,
  width = function() input$go_plot_width)


  output$down_GO_pdf_style1 <- renderUI({
    req(go_table_info())
    downloadButton("dl_go_style1", paste0("Download ", enrich_type_tag(), " PDF"))
  })

  output$down_GO_pdf_style2 <- renderUI({
    req(go_table_info())
    downloadButton("dl_go_style2", paste0("Download ", enrich_type_tag(), " PDF"))
  })


  output$dl_go_style1 <- make_download_pdf(
    plot_expr   = function() plot_GO_dot1(
      go_df(),
      topn = input$go_topn,
      label_format = input$go_label_width,
      group_by = input$go_group_by
    ),
    input       = input,
    suffix      = function() paste0(enrich_type_tag(), "_style1", go_entry_suffix_tag()),
    filename    = function() go_pdf_filename(),
    width       = function() input$go_plot_width  / 100,
    height      = function() input$go_plot_height / 100,
    input_field = "go_file"
  )

  output$dl_go_style2 <- make_download_pdf(
    plot_expr   = function() plot_GO_dot2(
      go_df(),
      topn = input$go_topn,
      label_format = input$go_label_width,
      group_by = input$go_group_by,
      x_axis = input$go_x_axis
    ),
    input       = input,
    suffix      = function() paste0(enrich_type_tag(), "_style2", go_entry_suffix_tag()),
    filename    = function() go_pdf_filename(),
    width       = function() input$go_plot_width  / 100,
    height      = function() input$go_plot_height / 100,
    input_field = "go_file"
  )
  #string
  string_table_info <- reactive({
    req(input$string_go_file)
    ext <- tools::file_ext(input$string_go_file$name)
    df <- switch(tolower(ext),
                 "xlsx" = readxl::read_xlsx(input$string_go_file$datapath),
                 "xls"  = readxl::read_xls(input$string_go_file$datapath),
                 "tsv"  = data.table::fread(input$string_go_file$datapath),
                 stop("Only .tsv/.xls/.xlsx are supported")
    )

    norm_name <- function(x) tolower(gsub("[^a-z0-9]", "", x))
    pick_col <- function(df0, aliases) {
      nms <- names(df0)
      idx <- match(norm_name(aliases), norm_name(nms))
      idx <- idx[!is.na(idx)]
      if (!length(idx)) return(NA_character_)
      nms[idx[1]]
    }

    desc_col <- pick_col(df, c("term description", "Description", "description"))
    fdr_col  <- pick_col(df, c("false discovery rate", "p.adjust", "FDR"))
    obs_col  <- pick_col(df, c("observed gene count", "observed_gene_count", "genes mapped", "Count"))
    validate(need(!is.na(desc_col) && !is.na(fdr_col) && !is.na(obs_col),
                  "Missing required columns: description / false discovery rate / observed gene count."))

    cat_col <- pick_col(df, c("#category", "category"))
    direction_col <- pick_col(df, c("direction", "#direction", "enrichment direction", "enrichment_direction"))
    bg_col <- pick_col(df, c("background gene count", "background_gene_count"))
    strength_col <- pick_col(df, c("strength", "strength signal", "enrichment score"))
    if (!is.na(cat_col)) {
      cat_vals <- unique(as.character(df[[cat_col]]))
      cat_vals <- cat_vals[!is.na(cat_vals) & trimws(cat_vals) != ""]
      cat_vals <- sort(cat_vals)
    } else {
      cat_vals <- character(0)
    }

    if (!is.na(direction_col)) {
      has_direction <- TRUE
    } else {
      has_direction <- FALSE
    }

    list(
      df = df,
      desc_col = desc_col,
      fdr_col = fdr_col,
      obs_col = obs_col,
      category_col = cat_col,
      category_choices = c("All", cat_vals),
      direction_col = direction_col,
      has_direction = has_direction,
      has_background = !is.na(bg_col),
      has_strength = !is.na(strength_col)
    )
  })

  output$string_category_selector_ui <- renderUI({
    req(string_table_info())
    info <- string_table_info()
    selectInput(
      inputId = "string_category_choice",
      label = "Select category",
      choices = info$category_choices,
      selected = "All"
    )
  })

  output$string_direction_selector_ui <- renderUI({
    req(string_table_info())
    info <- string_table_info()
    if (!isTRUE(info$has_direction)) return(NULL)

    selectInput(
      inputId = "string_direction_choice",
      label = "Select direction",
      choices = c("Both end", "top", "bottom"),
      selected = "Both end"
    )
  })

  string_metric_choices <- reactive({
    req(string_table_info())
    info <- string_table_info()

    if (!is.na(info$direction_col)) {
      choices <- c(
        "Genes mapped" = "genes_mapped",
        "Enrichment score" = "enrichment_score"
      )
    } else {
      choices <- c("Observed gene count" = "observed_gene_count")
      if (isTRUE(info$has_background)) {
        choices <- c(
          choices,
          "Background gene count" = "background_gene_count",
          "Gene ratio (observed/background)" = "gene_ratio"
        )
      }
      if (isTRUE(info$has_strength)) {
        choices <- c(choices, "Strength" = "strength")
      }
    }
    choices
  })

  output$string_x_axis_selector_ui <- renderUI({
    req(string_metric_choices())
    choices <- string_metric_choices()
    selected <- if ("gene_ratio" %in% choices) "gene_ratio" else unname(choices[1])

    selectInput(
      inputId = "string_go_x_axis",
      label = "Dotplot X axis",
      choices = choices,
      selected = selected
    )
  })

  output$string_group_by_selector_ui <- renderUI({
    req(string_metric_choices())
    choices <- c("-log10(FDR)" = "fdr", string_metric_choices())

    selectInput(
      inputId = "string_go_group_by",
      label = "Top N group_by",
      choices = choices,
      selected = "fdr"
    )
  })

  string_go_df <- reactive({
    req(string_table_info())
    info <- string_table_info()
    df <- info$df

    norm_key <- function(x) {
      x <- as.character(x)
      x <- trimws(x)
      tolower(gsub("[^a-z0-9]", "", x))
    }

    if (!is.na(info$category_col) && !is.null(input$string_category_choice) && input$string_category_choice != "All") {
      cat_key <- norm_key(input$string_category_choice)
      df <- df[norm_key(df[[info$category_col]]) == cat_key, , drop = FALSE]
    }

    if (isTRUE(info$has_direction) && !is.null(input$string_direction_choice) && nzchar(input$string_direction_choice)) {
      dir_key <- norm_key(input$string_direction_choice)
      # Both end behaves like ALL (no direction filtering)
      if (dir_key %in% c("top", "bottom")) {
        df <- df[norm_key(df[[info$direction_col]]) == dir_key, , drop = FALSE]
      }
    }

    # Deduplicate same term label after filtering.
    # In STRING tables, the same description can appear multiple times
    # (e.g. different direction/source rows). Keep the most significant one.
    if (!is.null(info$desc_col) && !is.na(info$desc_col) && info$desc_col %in% names(df)) {
      desc_key <- as.character(df[[info$desc_col]])
      desc_key <- gsub("\\s+", " ", trimws(desc_key))
      desc_key_norm <- tolower(desc_key)

      fdr_val <- if (!is.null(info$fdr_col) && !is.na(info$fdr_col) && info$fdr_col %in% names(df)) {
        suppressWarnings(as.numeric(df[[info$fdr_col]]))
      } else {
        rep(Inf, nrow(df))
      }
      fdr_val[!is.finite(fdr_val)] <- Inf

      obs_val <- if (!is.null(info$obs_col) && !is.na(info$obs_col) && info$obs_col %in% names(df)) {
        suppressWarnings(as.numeric(df[[info$obs_col]]))
      } else {
        rep(-Inf, nrow(df))
      }
      obs_val[!is.finite(obs_val)] <- -Inf

      ord <- order(fdr_val, -obs_val)
      df <- df[ord, , drop = FALSE]
      keep <- !duplicated(desc_key_norm[ord])
      df <- df[keep, , drop = FALSE]
    }

    validate(need(nrow(df) > 0, "No STRING terms left after category/direction filtering."))
    df
  })

  string_param_suffix_tag <- reactive({
    clean_tag <- function(x) {
      x <- as.character(x)[1]
      x <- toupper(trimws(x))
      x <- gsub("[^A-Z0-9]+", "_", x)
      x <- gsub("^_+|_+$", "", x)
      if (!nzchar(x)) x <- "ALL"
      x
    }

    cat_tag <- clean_tag(if (is.null(input$string_category_choice)) "All" else input$string_category_choice)

    if (is.null(input$string_direction_choice) || !nzchar(input$string_direction_choice)) {
      dir_raw <- "Both end"
    } else {
      dir_raw <- input$string_direction_choice
    }
    dir_tag <- clean_tag(dir_raw)

    x_tag <- clean_tag(if (is.null(input$string_go_x_axis)) "NA" else input$string_go_x_axis)
    g_tag <- clean_tag(if (is.null(input$string_go_group_by)) "FDR" else input$string_go_group_by)

    paste0(".", cat_tag, ".", dir_tag, ".", g_tag, ".", x_tag)
  })

  string_pdf_filename <- reactive({
    req(string_table_info())
    info <- string_table_info()

    category_label <- if (is.null(input$string_category_choice) || !nzchar(input$string_category_choice)) {
      "All"
    } else {
      as.character(input$string_category_choice)
    }

    direction_label <- if (is.null(input$string_direction_choice) || !nzchar(input$string_direction_choice)) {
      "Both end"
    } else {
      as.character(input$string_direction_choice)
    }

    category_with_direction <- if (isTRUE(info$has_direction)) {
      paste0(category_label, " (", direction_label, ")")
    } else {
      category_label
    }

    raw_name <- paste0(
      "Top ", input$string_go_go_topn,
      " terms of String function enrichment plot in ",
      category_with_direction,
      " category"
    )
    sanitize_pdf_filename(raw_name)
  })

  output$GO_enrich_plot3 <- renderPlot({
    req(string_go_df())
    plot_GO_dot3(
      string_go_df(),
      topn = input$string_go_go_topn,
      label_format = input$string_go_go_label_width,
      group_by = if (is.null(input$string_go_group_by)) "fdr" else input$string_go_group_by,
      x_axis = input$string_go_x_axis
    )
  },
  height = function() input$string_go_plot_height,
  width = function() input$string_go_width)


  output$down_GO_pdf_style3 <- renderUI({
    req(string_go_df())
    downloadButton("dl_go_style3", "Download PDF")
  })
  output$dl_go_style3 <- make_download_pdf(
    plot_expr   = function() plot_GO_dot3(
      string_go_df(),
      topn = input$string_go_go_topn,
      label_format = input$string_go_go_label_width,
      group_by = if (is.null(input$string_go_group_by)) "fdr" else input$string_go_group_by,
      x_axis = input$string_go_x_axis
    ),
    input       = input,
    suffix      = function() paste0("STRING_enrichment", string_param_suffix_tag()),
    filename    = function() string_pdf_filename(),
    width       = function() input$string_go_width  / 100,
    height      = function() input$string_go_plot_height / 100,
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
  req(se_data())   # wait for SE upload before rendering
    se <- se_data()
    gene_list <- rownames(se)
    col_vars <- colnames(colData(se))
    tagList(
      selectizeInput(
        inputId = "gene_select",
  label = "Select genes:",
        choices = gene_list,
        multiple = TRUE,
        options = list(
          placeholder = "Select or enter gene names...",
          maxOptions = 1000   # avoid loading too many options at once
        )
      ),
      selectInput(
        inputId = "group_col",
        label = "Select grouping column",
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


  gene_exp_plot <- reactiveVal(NULL)
  output$gene_exp <- renderPlot({
    req(se_data(), input$gene_select)

    se <- se_data()
    genes <- input$gene_select
    print(genes)
    sel <- input$selected_cols
    col_name <- sub("\\|\\|.*$", "", sel[1])
    vals <- sub("^.*\\|\\|", "", sel)

    col_df <- as.data.frame(colData(se))
    matched_samples <- rownames(col_df)[col_df[[col_name]] %in% vals]

    if (length(matched_samples) == 0) {
      showNotification("No samples matched your selection", type = "error")
      return(NULL)
    }

    se_sub <- se[, matched_samples, drop = FALSE]

    valid_genes <- genes[genes %in% rownames(se_sub)]
    if (length(valid_genes) == 0) return(NULL)

    if (length(valid_genes) == 1) {
      p <- plot_gene_expression(
        se_sub,
        valid_genes,
        by = input$group_col
      )
    } else {
      plot_list <- purrr::map(valid_genes, ~{
        plot_gene_expression(se_sub, .x, by = input$group_col)
      })
      p <- cowplot::plot_grid(plotlist = plot_list)
    }

    gene_exp_plot(p)
    p
  },
  height = function() input$gene_exp_height,
  width  = function() input$gene_exp_width
  )



  output$down_gene_exp_ui <- renderUI({
    req(se_data())
    downloadButton("down_gene_exp", "Download PDF")
  })
  output$down_gene_exp <- make_download_pdf(
    plot_expr   = function() {
      req(gene_exp_plot())
      gene_exp_plot()
    },
    input       = input,
    suffix      = paste0(input$gene_select, "_expression"),
    width       = function() input$gene_exp_width  / 100,
    height      = function() input$gene_exp_height / 100,
    input_field = "gene_exp_rds"
  )




}




