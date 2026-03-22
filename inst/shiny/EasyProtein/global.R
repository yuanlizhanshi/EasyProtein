library(EasyProtein)
library(tidyverse)
library(SummarizedExperiment)
library(bslib)
library(DT)
library(htmltools)
library(httr)
library(reactable)
library(shiny)
library(shinyjqui)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(ggiraph)


# Limit user file uploads to 2GB
options(shiny.maxRequestSize=1024*1024^2)

col<- rev(colorRampPalette(c("#cc0000", "#FFff00",'#66ccff','#000066'))(50))

# Define custom color palette
custom_colors <- c(
  "#BDBDBD", "#8DB5CE", "#542E8B", "#D7BD90", "#C82129", "#F296C0", "#647950", "#114B9B", "#FBBD90", "#193E51", "#BE93BF")

cols <- c("OrangeRed","SlateBlue3","DarkOrange","GreenYellow","Purple",
          "DarkSlateGray","Gold","DarkGreen","DeepPink2","Red4","#4682B4",
          "#FFDAB9","#708090","#836FFF","#CDC673","#CD9B1D","#FF6EB4","#CDB5CD"
          ,"#008B8B","#43CD80","#483D8B","#66CD00","#CDC673","#CDAD00","#CD9B9B"
          ,"#FF8247","#8B7355","#8B3A62","#68228B","#CDB7B5","#CD853F","#6B8E23"
          ,"#696969","#7B68EE","#9F79EE","#B0C4DE","#7A378B","#66CDAA","#EEE8AA"
          ,"#00FF00","#EEA2AD","#A0522D","#000080","#E9967A","#00CDCD","#8B4500"
          ,"#DDA0DD","#EE9572","#EEE9E9","#8B1A1A","#8B8378","#EE9A49","#EECFA1"
          ,"#8B4726","#8B8878","#EEB4B4","#C1CDCD","#8B7500","#0000FF","#EEEED1"
          ,"#4F94CD","#6E8B3D","#B0E2FF","#76EE00","#A2B5CD","#548B54","#BBFFFF"
          ,"#B4EEB4","#00C5CD","#008B8B","#7FFFD4","#8EE5EE","#43CD80","#68838B"
          ,"#00FF00","#B9D3EE","#9ACD32","#00688B","#FFEC8B","#1C86EE","#CDCD00"
          ,"#473C8B","#FFB90F","#EED5D2","#CD5555","#CDC9A5","#FFE7BA","#FFDAB9"
          ,"#CD661D","#CDC5BF","#FF8C69","#8A2BE2","#CD8500","#B03060","#FF6347"
          ,"#FF7F50","#CD0000","#F4A460","#FFB5C5","#DAA52")


STRING_BASE <- "https://string-db.org/api"
CALLER_ID   <- "EasyProtein"
`%||%` <- function(a, b) if (is.null(a)) b else a
js_escape  <- function(x) gsub("'", "\\\\'", x, fixed = TRUE)

help_popover_init <- function() {
  tagList(
    tags$style(HTML(" 
      .ep-help-qmark {
        cursor: pointer;
        color: #6c757d;
        display: inline-flex;
        align-items: center;
      }
      .ep-help-popover {
        position: absolute;
        z-index: 20000;
        background: #ffffff;
        color: #1f2937;
        border: 1px solid #d1d5db;
        border-radius: 10px;
        box-shadow: 0 10px 25px rgba(0, 0, 0, 0.12);
        padding: 10px 12px;
        font-size: 13px;
      }
    ")),
    tags$script(HTML(" 
      (function () {
        function removePopovers() {
          document.querySelectorAll('.ep-help-popover').forEach(function (el) { el.remove(); });
          document.querySelectorAll('[data-help-popover=\"true\"]').forEach(function (el) {
            el.setAttribute('aria-expanded', 'false');
          });
        }

        function positionPopover(trigger, popover, placement) {
          var rect = trigger.getBoundingClientRect();
          var popRect = popover.getBoundingClientRect();
          var top = window.scrollY + rect.top + (rect.height - popRect.height) / 2;
          var left = window.scrollX + rect.right + 10;

          if (placement === 'left') {
            left = window.scrollX + rect.left - popRect.width - 10;
          } else if (placement === 'top') {
            top = window.scrollY + rect.top - popRect.height - 10;
            left = window.scrollX + rect.left + (rect.width - popRect.width) / 2;
          } else if (placement === 'bottom') {
            top = window.scrollY + rect.bottom + 10;
            left = window.scrollX + rect.left + (rect.width - popRect.width) / 2;
          }

          popover.style.top = Math.max(8, top) + 'px';
          popover.style.left = Math.max(8, left) + 'px';
        }

        function showPopover(trigger) {
          var isOpen = trigger.getAttribute('aria-expanded') === 'true';
          removePopovers();
          if (isOpen) return;

          var popover = document.createElement('div');
          popover.className = 'ep-help-popover';
          popover.innerHTML = trigger.getAttribute('data-help-content') || '';
          document.body.appendChild(popover);

          positionPopover(trigger, popover, trigger.getAttribute('data-help-placement') || 'right');
          trigger.setAttribute('aria-expanded', 'true');
        }

        document.addEventListener('click', function (event) {
          var trigger = event.target.closest('[data-help-popover=\"true\"]');
          if (trigger) {
            event.preventDefault();
            event.stopPropagation();
            showPopover(trigger);
            return;
          }

          if (!event.target.closest('.ep-help-popover')) {
            removePopovers();
          }
        });

        document.addEventListener('keydown', function (event) {
          if (event.key === 'Escape') removePopovers();
        });

        window.addEventListener('resize', removePopovers);
        document.addEventListener('shown.bs.tab', removePopovers);
        document.addEventListener('shown.bs.modal', removePopovers);
      })();
    "))
  )
}

help_qmark <- function(content,
                       placement = "right",
                       icon_name = "question-circle",
                       size_px = 16,
                       max_width_px = 280) {
  content_html <- as.character(tagList(
    tags$div(
      style = sprintf("max-width:%dpx; line-height:1.45;", max_width_px),
      content
    )
  ))

  tags$span(
    shiny::icon(icon_name),
    class = "ep-help-qmark",
    style = sprintf("font-size:%dpx;", size_px),
    `data-help-popover` = "true",
    `data-help-placement` = placement,
    `data-help-content` = content_html,
    `aria-expanded` = "false",
    tabindex = "0",
    role = "button"
  )
}


# make_download_pdf <- function(plot_expr, input, suffix = NULL,
#                               width = 7, height = 5,
#                               input_field = "se_file") {
#   downloadHandler(
#     filename = function() {
#       base <- tools::file_path_sans_ext(basename(input[[input_field]]$name))
#       paste0(base, if (!is.null(suffix)) paste0("_", suffix) else "", ".pdf")
#     },
#     content = function(file) {
#       w <- if (is.function(width))  width()  else width
#       h <- if (is.function(height)) height() else height
#       cairo_pdf(file, width = w, height = h, fallback_resolution = 300)
#       print(plot_expr())  # plot_expr() must return a ggplot
#       dev.off()
#     }
#   )
# }

make_download_pdf <- function(plot_expr, input, suffix = NULL,
                              width = 7, height = 5,
                              input_field = "se_file") {
  downloadHandler(
    filename = function() {
      base <- tools::file_path_sans_ext(basename(input[[input_field]]$name))
      paste0(base, if (!is.null(suffix)) paste0("_", suffix) else "", ".pdf")
    },
    content = function(file) {
      w <- if (is.function(width))  width()  else width
      h <- if (is.function(height)) height() else height

      showModal(modalDialog(
        title = NULL,
  "Generating PDF, please wait...",
        footer = NULL,
        easyClose = FALSE
      ))
      on.exit(removeModal(), add = TRUE)

      cairo_pdf(file, width = w, height = h, fallback_resolution = 300)

      p <- plot_expr()

      if (inherits(p, "Heatmap") || inherits(p, "HeatmapList")) {
        ComplexHeatmap::draw(p)
      } else {
        print(p)  # ggplot
      }

      dev.off()
    }
  )
}

#ui ----

timeFilterUI <- function(id) {
  ns <- NS(id)

  div(
    style = "border:1px solid #ddd; padding:15px; border-radius:8px; background:#fafafa;",

    h4("Time-series parameters"),
    numericInput(
      inputId = ns("min_expression_threshold"),
      label   = "Minimum expression threshold",
      value   = 1,
      min     = 0,
      max     = 20,
      step    = 1
    ),
    sliderInput(
      inputId = ns("CV_with_time_threshold"),
      label   = "CV threshold",
      min     = 0,
      max     = 1,
      value   = 0.2,
      step    = 0.01
    ),
    sliderInput(
      inputId = ns("padj_threshold"),
      label   = "Adjusted P-value threshold",
      min     = 0,
      max     = 0.1,
      value   = 0.05,
      step    = 0.001
    ),
    numericInput(
      inputId = ns("k_cluster"),
      label   = "Number of clusters",
      value   = 10,
      min     = 2,
      max     = 20,
      step    = 1
    )
  )
}

load_module_packages <- function(pkgs, prefix = "Missing packages: ") {
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0) {
    showNotification(paste0(prefix, paste(missing, collapse = ", ")), type = "error")
    return(FALSE)
  }
  suppressPackageStartupMessages(
    lapply(pkgs, library, character.only = TRUE)
  )
  TRUE
}

#server logic------
.get_upload_base <- function(upload_name) {
  base_no_ext <- tools::file_path_sans_ext(basename(upload_name))
  base <- stringr::str_extract(base_no_ext, ".*(?=_report)")
  if (is.na(base) || !nzchar(base)) {
    base <- base_no_ext
  }
  base
}

.write_assay_export_if_exists <- function(se_obj, assay_name, out_path) {
  assay_names <- SummarizedExperiment::assayNames(se_obj)
  if (!assay_name %in% assay_names) return(FALSE)

  assay_df <- cbind(
    as.data.frame(SummarizedExperiment::rowData(se_obj)),
    as.data.frame(SummarizedExperiment::assay(se_obj, assay_name))
  )
  writexl::write_xlsx(assay_df, path = out_path)
  TRUE
}
make_download_se_qc_zip <- function(
  se_reactive,        # reactiveExpr: returns a SummarizedExperiment
  miss_gene,          # reactiveVal: missing_gene_rv
  unstable_gene,      # reactiveVal: un_stable_gene_rv
  input,              # Shiny input object
  file_input_id,      # upload file ID
  suffix = "_filtered",  # output filename suffix
    session = NULL
) {
  downloadHandler(
    filename = function() {
      req(input[[file_input_id]]$name)
      base <- .get_upload_base(input[[file_input_id]]$name)
      paste0(base, suffix, ".zip")
    },
    content = function(file) {
      req(se_reactive(), input[[file_input_id]])
      if (!is.null(session)) {
  session$sendCustomMessage("download_status", list(show = TRUE, text = "Packaging files for download, please wait..."))
      }
      on.exit({
        if (!is.null(session)) {
          session$sendCustomMessage("download_status", list(show = FALSE))
        }
      }, add = TRUE)

      se_obj <- se_reactive()
  miss_gene_df <- miss_gene()         # ✅ reactive value
  unstable_gene_df <- unstable_gene() # ✅ reactive value

      base_full <- tools::file_path_sans_ext(basename(input[[file_input_id]]$name))
      tmpdir <- tempfile("pack_")
      dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)

  # ---- Define output paths
      rds_path   <- file.path(tmpdir, paste0(base_full, "_se.Rds"))
      excel1_path <- file.path(tmpdir, paste0(base_full, "_imputed_intensity.xlsx"))
      excel2_path <- file.path(tmpdir, paste0(base_full, "_normalized_expression.xlsx"))
      excel3_path <- file.path(tmpdir, paste0(base_full, "_Zscaled_expression.xlsx"))
      excel4_path <- file.path(tmpdir, paste0(base_full, "_many_missing_value_gene.xlsx"))
      excel5_path <- file.path(tmpdir, paste0(base_full, "_unstable_gene.xlsx"))

  # ---- Write files
      saveRDS(se_obj, rds_path)
      writexl::write_xlsx(se2intensity(se_obj), path = excel1_path)
      writexl::write_xlsx(se2conc(se_obj), path = excel2_path)
      writexl::write_xlsx(se2scale(se_obj), path = excel3_path)
      if (!is.null(miss_gene_df)) writexl::write_xlsx(miss_gene_df, path = excel4_path)
      if (!is.null(unstable_gene_df)) writexl::write_xlsx(unstable_gene_df, path = excel5_path)

  # ---- Package ZIP
      zip::zipr(
        zipfile = file,
        files = c(
          rds_path,
          excel1_path,
          excel2_path,
          excel3_path,
          excel4_path,
          excel5_path
        ),
        mode = "cherry-pick"
      )
    },
    contentType = "application/zip"
  )
}

make_download_se_zip <- function(
  se_reactive,        # reactiveExpr: returns a SummarizedExperiment
  input,              # Shiny input object
  file_input_id,      # e.g. "se_file" — upload file ID for filename
  suffix = "_filtered",  # filename suffix (optional)
    session = NULL
) {
  downloadHandler(
    filename = function() {
      req(input[[file_input_id]]$name)
      base <- .get_upload_base(input[[file_input_id]]$name)
      paste0(base, suffix, ".zip")
    },
    content = function(file) {
      req(se_reactive(), input[[file_input_id]])
      if (!is.null(session)) {
  session$sendCustomMessage("download_status", list(show = TRUE, text = "Packaging files for download, please wait..."))
      }
      on.exit({
        if (!is.null(session)) {
          session$sendCustomMessage("download_status", list(show = FALSE))
        }
      }, add = TRUE)
      se_obj <- se_reactive()
      base_full <- tools::file_path_sans_ext(basename(input[[file_input_id]]$name))

      tmpdir <- tempfile("pack_")
      dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)

  # Temporary file paths
      rds_path   <- file.path(tmpdir, paste0(base_full, "_se.Rds"))
      excel1_path <- file.path(tmpdir, paste0(base_full, "_imputated_intensity.xlsx"))
      excel2_path <- file.path(tmpdir, paste0(base_full, "_normalized_expression.xlsx"))
      excel3_path <- file.path(tmpdir, paste0(base_full, "_Zscaled_expression.xlsx"))

  # Write files
      saveRDS(se_obj, rds_path)
      writexl::write_xlsx(se2intensity(se_obj), path = excel1_path)
      writexl::write_xlsx(se2conc(se_obj),       path = excel2_path)
      writexl::write_xlsx(se2scale(se_obj),      path = excel3_path)

  # Package ZIP
      zip::zipr(
        zipfile = file,
        files = c(rds_path, excel1_path, excel2_path, excel3_path),
        mode = "cherry-pick"
      )
    },
    contentType = "application/zip"
  )
}
make_download_time_se_zip <- function(
    se_reactive,
    input,
    file_input_id,
    suffix = "_time_series",
    session = NULL
) {
  downloadHandler(
    filename = function() {
      req(input[[file_input_id]]$name)
      base <- .get_upload_base(input[[file_input_id]]$name)
      paste0(base, suffix, ".zip")
    },
    content = function(file) {
      req(se_reactive(), input[[file_input_id]])
      if (!is.null(session)) {
  session$sendCustomMessage("download_status", list(show = TRUE, text = "Packaging files for download, please wait..."))
      }
      on.exit({
        if (!is.null(session)) {
          session$sendCustomMessage("download_status", list(show = FALSE))
        }
      }, add = TRUE)
      se_obj <- se_reactive()
      base_full <- tools::file_path_sans_ext(basename(input[[file_input_id]]$name))

      tmpdir <- tempfile("pack_")
      dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)

  # Temporary file paths
      rds_path    <- file.path(tmpdir, paste0(base_full, "_se.Rds"))
      excel2_path <- file.path(tmpdir, paste0(base_full, "_normalized_expression.xlsx"))
      excel3_path <- file.path(tmpdir, paste0(base_full, "_Zscaled_expression.xlsx"))

      saveRDS(se_obj, rds_path)

      files_to_zip <- c(rds_path)
      if ("conc" %in% SummarizedExperiment::assayNames(se_obj)) {
        writexl::write_xlsx(se2conc(se_obj), path = excel2_path)
        files_to_zip <- c(files_to_zip, excel2_path)
      } else {
        has_intensity <- .write_assay_export_if_exists(se_obj, "intensity", excel2_path)
        if (has_intensity) files_to_zip <- c(files_to_zip, excel2_path)
      }

      if ("zscale" %in% SummarizedExperiment::assayNames(se_obj)) {
        writexl::write_xlsx(se2scale(se_obj), path = excel3_path)
        files_to_zip <- c(files_to_zip, excel3_path)
      }

  # Package ZIP
      zip::zipr(
        zipfile = file,
        files = files_to_zip,
        mode = "cherry-pick"
      )
    },
    contentType = "application/zip"
  )
}
# make_grouped_select <- function(id, df, default_all = FALSE) {
#
#   choices <- map(names(df), function(col) {
#     vals <- unique(df[[col]])
#     vals <- as.character(vals)
#     setNames(paste0(col, "||", vals), vals)
#   })
#   names(choices) <- names(df)
#
#   # Remove rep / group
#   choices <- choices[!names(choices) %in% c("rep", "group")]
#   choices <- choices[c(2:length(choices),1)]
#
#   default_selected <- NULL
#   if (default_all && length(choices) > 0) {
#     default_selected <- unname(unlist(choices[[1]]))  # Default: select all values of the first group
#   }
#
#
#   ui <- virtualSelectInput(
#     inputId = id,
#     label   = "Select a column (single column, multiple values)",
#     choices = choices,
#     multiple = TRUE,
#     search = TRUE,
#     selected = default_selected,
#     placeholder = "Select column values..."
#   )
#
#
#   server <- function(input, output, session) {
#     observeEvent(input[[id]], ignoreInit = TRUE, {
#       vals <- input[[id]]
#       if (is.null(vals) || length(vals) <= 1) return()
#
#       groups <- sub("\\|\\|.*$", "", vals)
#       g0 <- groups[1]
#       keep <- vals[groups == g0]
#
#       if (length(keep) != length(vals)) {
#         updateVirtualSelect(
#           inputId = id,
#           selected = keep
#         )
#       }
#     })
#   }
#
#   list(ui = ui, server = server)
# }

make_grouped_select <- function(id,
                                df,
                                label = "Select coldata columns",
                                default_all = FALSE,
                                max_levels = 1000) {

  valid_cols <- names(df)[
    sapply(df, function(x) {
      is.character(x) && length(unique(x)) <= max_levels
    })
  ]

  df <- df[, valid_cols, drop = FALSE]
  df <- df[, !names(df) %in% c("rep", "group"), drop = FALSE]

  if (ncol(df) == 0) {
    ui <- helpText("No valid columns (character type with <= 1000 levels).")
    return(list(ui = ui, server = function(...) {}))
  }

  col_choices <- names(df)

  ui <- tagList(
    selectInput(
      inputId = paste0(id, "_col"),
      label = paste0(label, " (column)"),
      choices = col_choices,
      selected = col_choices[1]
    ),
    virtualSelectInput(
      inputId = id,
      label   = paste0(label, " (values)"),
      choices = NULL,
      multiple = TRUE,
      search = TRUE,
      selected = NULL,
  placeholder = "Select values for the chosen column..."
    )
  )

  server <- function(input, output, session) {
    observeEvent(input[[paste0(id, "_col")]], ignoreInit = TRUE, {
      col_name <- input[[paste0(id, "_col")]]
      vals <- unique(df[[col_name]])
      vals <- as.character(vals)
      choices <- setNames(paste0(col_name, "||", vals), vals)
      selected <- if (default_all) unname(choices) else NULL

      updateVirtualSelect(
        inputId = id,
        choices = choices,
        selected = selected
      )
    }, ignoreNULL = TRUE)

    observeEvent(TRUE, {
      col_name <- input[[paste0(id, "_col")]] %||% col_choices[1]
      vals <- unique(df[[col_name]])
      vals <- as.character(vals)
      choices <- setNames(paste0(col_name, "||", vals), vals)
      selected <- if (default_all) unname(choices) else NULL

      updateVirtualSelect(
        inputId = id,
        choices = choices,
        selected = selected
      )
    }, once = TRUE)
  }

  list(ui = ui, server = server)
}

show_heatmap_param_modal <- function(se_data = NULL) {

  all_cols <- if (!is.null(se_data)) as.data.frame(colData(se_data)) else character(0)
  all_rows <- if (!is.null(se_data)) as.data.frame(rowData(se_data)) else character(0)
  showModal(
    modalDialog(
      title = "Please set Heatmap parameters",
      size = 'xl',
      tags$div(

        fluidRow(

          ## ===============================
          ## Left column: column / sample selection
          ## ===============================
          column(
            width = 6,

            h4("Column selection"),

            make_grouped_select(
              id = "selected_cols",
              df = all_cols,
              label = 'Select coldata columns',
              default_all = TRUE
            )$ui,
            selectInput(
              "row_k",
              "Row k-means clusters",
              choices = c("NONE", "AUTO", 2:10),
              selected = "AUTO"
            ),
            conditionalPanel(
              condition = "input.row_k == 'NONE'",
              uiOutput("coldata_row_selector")
            ),

            radioButtons(
              "col_cluster_mode",
              "Column clustering mode",
              choices = c(
                "K-means" = "kmeans",
                "Use colData group" = "coldata"
              ),
              selected = "kmeans",
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.col_cluster_mode == 'kmeans'",
              selectInput(
                "col_k",
                "Column k-means clusters",
                choices = c("AUTO", 2:10),
                selected = "AUTO"
              )
            ),

            conditionalPanel(
              condition = "input.col_cluster_mode == 'coldata'",
              uiOutput("coldata_col_selector")
            ),
            selectInput(
              "top_annotaion_legend",
              "Top annotation grouping",
              choices = c("NULL", colnames(all_cols)),
              selected = "condition"
            ),


            make_grouped_select(
              id = "selected_rows",
              df = all_rows,
              label = 'Select rowdata columns',
              default_all = TRUE
            )$ui


          ),

          ## ===============================
          ## Right column: heatmap parameters
          ## ===============================
          column(
            width = 6,

            h4("Heatmap parameters"),


            fluidRow(
              column(
                  6,
                  checkboxInput(
                    "enable_col_cluster",
                    "Enable column clustering",
                    value = TRUE
                  )
              ),
              column(
                6,
                checkboxInput(
                  "enable_row_cluster",
                  "Enable row clustering",
                  value = TRUE
                )
              )
            ),

            fluidRow(
              column(
                6,
                checkboxInput(
                  "show_col_names",
                  "Display column names",
                  value = FALSE
                ),
              ),
              column(
                6,
                checkboxInput(
                  "show_row_names",
                  "Display row names",
                  value = FALSE
                ),
              )
            ),





            sliderInput(
              "expr_min",
              "Filter low-expression genes",
              min = 0, max = 20, value = 0, step = 1
            ),

            sliderInput(
              "cv_min",
              "Filter low-CV genes",
              min = 0, max = 1, value = 0.1, step = 0.05
            ),

            sliderInput(
              "column_title_size",
              "Font size of column title",
              min = 0, max = 20, value = 8, step = 1
            ),

            fluidRow(
              column(
                6,
                numericInput(
                  "pdf_width",
                  "PDF width (inch)",
                  value = 7, min = 3, step = 1
                )
              ),
              column(
                6,
                numericInput(
                  "pdf_height",
                  "PDF height (inch)",
                  value = 7, min = 3, step = 1
                )
              )
            )
          )
        )
      ),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_params", "Confirm")
      ),

      easyClose = TRUE
    )
  )
}

show_gene_selection_modal <- function(df) {
  showModal(
    modalDialog(
      title = "Select gene column and optional grouping",
      size = "l",
      easyClose = TRUE,

      tagList(
        selectInput(
          "gene_col",
          "Select the gene column:",
          choices  = colnames(df),
          selected = colnames(df)[1]
        ),
        selectInput(
          "group_col",
          "Select grouping column (optional):",
          choices  = c("None", colnames(df)),
          selected = "None"
        ),
        uiOutput("qc_group_value_ui")
      ),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_gene_selection", "Confirm", class = "btn-primary")
      )
    )
  )
}
show_gene_selection_modal_go <- function(df) {
  showModal(
    modalDialog(
      title = "Select gene column and optional grouping",

  # If there is more than one column, provide a column selector
      if (ncol(df) > 1) {
        tagList(
          selectInput(
            "gene_col",
            "Select the gene column:",
            choices  = colnames(df),
            selected = colnames(df)[1]
          ),
          selectInput(
            "group_col",
            "Select grouping column (optional):",
            choices  = c("None", setdiff(colnames(df), grep("SYMBOL|GENE", colnames(df), value = TRUE))),
            selected = "None"
          ),
          uiOutput("group_value_ui")
        )
      } else {
        h4("Only one column detected — will use all genes in this column for enrichment.")
      },

      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_gene_selection", "Confirm", class = "btn-primary")
      ),
      easyClose = TRUE
    )
  )
}
show_time_series_param <- function(se_data = NULL) {

  all_cols <- if (!is.null(se_data)) as.data.frame(colData(se_data)) else character(0)

  showModal(
    modalDialog(
      title = "Please set Heatmap parameters",
      size = 'xl',
      tags$div(
        fluidRow(
          column(
            width = 6,
            make_grouped_select("selected_cols", all_cols, default_all = TRUE)$ui,
            selectInput(
              "coldata_col_selector",
              "Select grouping column from colData",
              choices  = colnames(colData(se_data)),
              selected = "condition"
            )
          ),
          column(
            width = 6,
            timeFilterUI("time_filter")
          )
        )
      ),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_time_params", "Confirm")
      ),

      easyClose = TRUE
    )
  )
}


show_gene_selection_modal_gse_go <- function(df){
  showModal(
    modalDialog(
      title = "Select gene column and optional grouping",


      tagList(
        selectInput(
          "gse_GO_gene_col",
          "Select the gene column:",
          choices  = colnames(df),
          selected = colnames(df)[1]
        ),
        selectInput(
          "logFC_col",
          "Select logFC_col column:",
          choices  = colnames(df),
          selected = colnames(df)[2]
        )
      ),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_ges_go_gene_selection", "Confirm", class = "btn-primary")
      ),
      easyClose = TRUE
    )
  )
}

string_post_tsv <- function(endpoint, query = list(), body = list()) {
  Sys.sleep(1)
  res <- httr::POST(
    url   = sprintf("%s/tsv/%s", STRING_BASE, endpoint),
    query = query,
    body  = body,
    encode = "form"
  )
  httr::stop_for_status(res)

  readr::read_tsv(
    httr::content(res, as = "text", encoding = "UTF-8"),
    show_col_types = FALSE
  )
}

pick_one_per_query <- function(idmap) {
  nms <- names(idmap)
  if ("queryItem" %in% nms) {
    idmap %>% group_by(.data$queryItem) %>% slice_head(n = 1) %>% ungroup()
  } else if ("queryIndex" %in% nms) {
    idmap %>% arrange(.data$queryIndex) %>%
      group_by(.data$queryIndex) %>% slice_head(n = 1) %>% ungroup()
  } else {
    idmap %>% distinct(.data$stringId, .keep_all = TRUE)
  }
}

##
show_se_selector_modal <- function(se_data) {
  col_df <- as.data.frame(colData(se_data))
  row_df <- as.data.frame(rowData(se_data))

  showModal(
    modalDialog(
      title = "Select samples and genes from SE",
      size = "l",

      h4("Subset samples in SE"),
      make_grouped_select(
        id = "se_col_selector",
        df = col_df,
        label = "Select coldata columns",
        default_all = TRUE
      )$ui,

      tags$hr(),
      h4("Option1:Subset genes in SE by gene SYMBOL"),
      make_grouped_select(
        id = "se_row_selector",
        df = row_df,
        label = "Select rowdata columns",
        default_all = TRUE
      )$ui,


      h4("Option2:Upload excel contains gene SYMBOL"),
      fileInput(
        inputId = "se_excel_upload",
        label   = "Upload Excel file",
        accept  = c(
          ".xls",
          ".xlsx"
        )
      ),


      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_se_index", "Confirm", class = "btn-primary")
      ),
      easyClose = TRUE
    )
  )
}
