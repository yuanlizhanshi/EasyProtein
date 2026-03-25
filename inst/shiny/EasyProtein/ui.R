
ui <- {
  fluidPage(
    #useShinyjs(),
    tags$div(id = "download_loading_overlay",
             tags$div(class = "overlay-box",
                      icon("spinner", class = "fa-spin"),
                      span(id = "download_loading_text", " Preparing download, please wait...")
             )
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style_1.css"),
      #tags$script(src = "scripts.js"),
      tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"),
      help_popover_init(),
      tags$style(HTML("
        #download_loading_overlay {
          display: none;
          position: fixed;
          z-index: 99999;
          left: 0; top: 0;
          width: 100vw; height: 100vh;
          background: rgba(255, 255, 255, 0.7);
        }
        #download_loading_overlay .overlay-box {
          position: absolute;
          top: 50%; left: 50%;
          transform: translate(-50%, -50%);
          background: #fff;
          border: 1px solid #ddd;
          border-radius: 8px;
          padding: 14px 20px;
          font-size: 16px;
          box-shadow: 0 2px 10px rgba(0,0,0,.15);
        }
        .ep-page {
          background-color: #F8FAFC;
          min-height: 100vh;
          padding: 24px 0 36px 0;
        }
        .ep-hero {
          background: linear-gradient(135deg, #FFFFFF 0%, #F8FBFF 100%);
          border: 1px solid #E5E7EB;
          border-radius: 16px;
          padding: 24px 28px;
          box-shadow: 0 8px 24px rgba(15, 23, 42, 0.06);
          margin-bottom: 22px;
        }
        .ep-card {
          background: #FFFFFF;
          border: 1px solid #E5E7EB;
          border-radius: 16px;
          padding: 22px 24px;
          box-shadow: 0 6px 18px rgba(15, 23, 42, 0.05);
          margin-bottom: 20px;
        }
        .ep-soft-panel {
          background: #F8FAFC;
          border: 1px solid #E2E8F0;
          border-radius: 12px;
          padding: 14px 16px;
        }
        .ep-dark-panel {
          background: #0F172A;
          color: #E2E8F0;
          border-radius: 12px;
          padding: 16px;
          overflow: auto;
        }
        .ep-title {
          font-size: 28px;
          font-weight: 800;
          color: #0F172A;
          line-height: 1.2;
          margin-bottom: 8px;
        }
        .ep-subtitle {
          font-size: 15px;
          color: #475569;
          line-height: 1.7;
          max-width: none;
          white-space: nowrap;
        }
        .ep-card-title {
          font-size: 22px;
          font-weight: 700;
          color: #111827;
          line-height: 1.2;
        }
        .ep-card-subtitle {
          font-size: 14px;
          color: #6B7280;
          margin-top: 4px;
          line-height: 1.7;
        }
        .ep-section-title {
          font-size: 16px;
          font-weight: 700;
          color: #1F2937;
          margin-bottom: 10px;
        }
        .ep-pill {
          display: inline-block;
          border-radius: 999px;
          padding: 8px 14px;
          font-size: 13px;
          font-weight: 600;
          margin-right: 8px;
          margin-top: 6px;
        }
        .ep-tabs .nav-tabs > li > a {
          min-width: 150px;
          text-align: center;
        }
        .ep-tabs .nav-tabs {
          margin-bottom: 10px;
        }
        .ep-tabs .tab-content {
          padding-top: 8px;
        }
      ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('download_status', function(msg) {
          var overlay = document.getElementById('download_loading_overlay');
          var textEl = document.getElementById('download_loading_text');
          if (!overlay) return;
          if (msg && msg.text && textEl) textEl.textContent = msg.text;
          overlay.style.display = (msg && msg.show) ? 'block' : 'none';
        });
      ")),
      theme = bslib::bs_theme(bootswatch = "journal"),
      # tags$script(
      #   HTML("
      #   $(document).on('shiny:sessioninitialized', function(event) {
      #     if (localStorage.getItem('firstVisit') === null) {
      #       localStorage.setItem('firstVisit', 'true');
      #       location.reload();
      #     } else {
      #       localStorage.removeItem('firstVisit');
      #     }
      #   });
      # ")
      # ),
      navbarPage(
        title = NULL,
        bg = "#FFFFFF",
        id = "inTabset",
        #theme = bslib::bs_theme(bootswatch = "journal"),
        ###Home-----
        tabPanel(
          title = "Home",
          icon = icon("home", lib = "glyphicon"),
          div(
            class = "home",
            fluidPage(
              tags$br(),

              fluidRow(
                column(
                  width = 10, offset = 1,
                  div(
                    style = "
              background: #FFFFFF;
              border: 1px solid #E5E7EB;
              border-radius: 12px;
              padding: 22px 28px 20px 28px;
              box-shadow: 0 2px 8px rgba(0,0,0,0.04);
              margin-bottom: 24px;
            ",
                    div(
                      "Introduction",
                      style = "
                font-size: 26px;
                font-weight: 700;
                color: #1F2937;
                border-left: 5px solid #2C7BE5;
                padding-left: 12px;
                margin-bottom: 18px;
                line-height: 1.2;
              "
                    ),
                    uiOutput("HOME_output_text")
                  )
                )
              ),

              fluidRow(
                column(
                  width = 10, offset = 1,
                  div(
                    style = "
              background: #FFFFFF;
              border: 1px solid #E5E7EB;
              border-radius: 12px;
              padding: 22px 28px 24px 28px;
              box-shadow: 0 2px 8px rgba(0,0,0,0.04);
              margin-bottom: 30px;
            ",
                    div(
                      "Overview of EasyProtein",
                      style = "
                font-size: 26px;
                font-weight: 700;
                color: #1F2937;
                border-left: 5px solid #2C7BE5;
                padding-left: 12px;
                margin-bottom: 20px;
                line-height: 1.2;
              "
                    ),
                    div(
                      tags$img(
                        src = "Fig1.jpg",
                        style = "
                  max-width: 70%;
                  height: auto;
                  display: block;
                  margin: 0 auto;
                  border-radius: 8px;
                "
                      ),
                      style = "text-align: center;"
                    )
                  )
                )
              )
            ),
            style = "
      background-color: #F8FAFC;
      min-height: 100vh;
      font-size: 18px;
    "
          )
        ),
        ###Data Imputation-----
        tabPanel(title = "Data Imputation",
                 icon = icon('pencil',lib = 'glyphicon'),
                 fluidPage(
                   div(
                     class = "ep-page",
                     fluidRow(
                       column(
                         width = 10, offset = 1,
                         div(
                           class = "ep-hero",
                           div(class = "ep-title", "Data Imputation"),
                           div(
                             class = "ep-subtitle",
                             "Upload a DIA-NN output TSV file (*report.pg_matrix.tsv), review parsed sample metadata, and export the processed SummarizedExperiment object in one clean workspace."
                           ),
                           tags$span(class = "ep-pill", style = "background:#EFF6FF; color:#1D4ED8; border:1px solid #BFDBFE;", icon("upload"), " Matrix upload"),
                           tags$span(class = "ep-pill", style = "background:#ECFDF5; color:#047857; border:1px solid #A7F3D0;", icon("database"), " SE output")
                         )
                       )
                     ),
                     fluidRow(
                       column(
                         width = 4, offset = 1,
                         div(
                           class = "ep-card",
                           div(class = "ep-card-title", "Input & metadata"),
                           div(class = "ep-card-subtitle", "Import a TSV file and confirm the sample annotation before generating results."),
                           div(class = "ep-soft-panel", style = "margin:16px 0 18px 0; color:#475569; line-height:1.7;",
                               tags$strong("Tip"), tags$br(),
                               "Use this page to build a clean SummarizedExperiment object for downstream analysis modules."
                           ),
                           fileInput(
                             inputId = "upload_tsv",
                             label = "Upload TSV File",
                             accept = c(".tsv"),
                             buttonLabel = "Browse...",
                             placeholder = "No file selected"
                           ),
                           uiOutput('edit_coldata_ui'),
                           div(style = "margin-top:16px;", downloadButton("download_se", "Download results"))
                         )
                       ),
                       column(
                         width = 6,
                         div(
                           class = "ep-card",
                           div(class = "ep-card-title", "Parsed object preview"),
                           div(class = "ep-card-subtitle", "Review the imported SE summary before continuing to the next analysis step."),
                           div(class = "ep-soft-panel", style = "min-height:420px; margin-top:16px; font-size:13px; background:#FFFFFF;", verbatimTextOutput("se_summary"))
                         )
                       )
                     )
                   ),style = "font-size:125%;")
        ),
        ###Quality Control----
        tabPanel(title = "Quality Control",
                 icon = icon('search',lib = 'glyphicon'),
                 fluidPage(
                   div(
                     class = "ep-page",
                     fluidRow(
                       column(
                         width = 10, offset = 1,
                         div(
                           class = "ep-hero",
                           div(class = "ep-title", "Quality Control"),
                           div(class = "ep-subtitle", "Inspect sample structure, compare intensity distributions, assess missingness, and export publication-ready QC plots from one dashboard."),
                           tags$span(class = "ep-pill", style = "background:#EEF2FF; color:#4338CA; border:1px solid #C7D2FE;", icon("bar-chart"), " PCA & density"),
                           tags$span(class = "ep-pill", style = "background:#FFF7ED; color:#C2410C; border:1px solid #FED7AA;", icon("sliders"), " Adjustable export size")
                         )
                       )
                     ),
                     fluidRow(
                       column(
                         width = 3, offset = 1,
                         div(
                           class = "ep-card",
                           div(class = "ep-card-title", "QC controls"),
                           div(class = "ep-card-subtitle", "Upload an SE object, filter samples or genes, and tune output dimensions."),
                           fileInput("se_file", "Upload SummarizedExperiment (Rds)", accept = ".Rds"),
                           uiOutput('edit_samples_UI'),
                           uiOutput('edit_genes_UI'),
                           uiOutput('download_edit_sample'),
                           sliderInput("plot_width", "Plot width (px)",
                                       min = 100, max = 3000, value = 480, step = 20),
                           sliderInput("plot_height", "Plot height (px)",
                                       min = 100, max = 3000, value = 340, step = 20),
                           div(class = "ep-soft-panel", style = "margin-top:16px; color:#475569;", textOutput("included_info"))
                         )
                       ),
                       column(
                         width = 7,
                         div(
                           class = "ep-card ep-tabs",
                           div(class = "ep-card-title", "QC visual diagnostics"),
                           div(class = "ep-card-subtitle", "Switch between PCA, density, missing-value, and protein-count views without leaving the page."),
                           div(style = "margin-top:16px;",
                               tabsetPanel(
                                 tabPanel(
                                   "PCA plot",
                                   br(),
                                   fluidRow(
                                     column(4, uiOutput("color_selector")),
                                     column(4, uiOutput("label_selector")),
                                     column(4, div(style = "display:flex; justify-content:flex-end;", downloadButton("download_pca_pdf", "Download PDF")))
                                   ),
                                   br(),
                                   plotOutput("pca_plot")
                                 ),
                                 tabPanel(
                                   "Intensity density",
                                   br(),
                                   div(style = "display:flex; justify-content:flex-end; margin-bottom:10px;", downloadButton("download_protein_density_pdf", "Download PDF")),
                                   plotOutput("intensity_density"),
                                   div(style = "display:flex; justify-content:flex-end; margin-top:10px;", downloadButton("download_protein_density_table", "Export table"))
                                 ),
                                 tabPanel(
                                   "CV density",
                                   br(),
                                   div(style = "display:flex; justify-content:flex-end; margin-bottom:10px;", downloadButton("download_cv_density_pdf", "Download PDF")),
                                   plotOutput("cv_density"),
                                   div(style = "display:flex; justify-content:flex-end; margin-top:10px;", downloadButton("download_cv_density_table", "Export table"))
                                 ),
                                 tabPanel(
                                   "Number of proteins detected",
                                   br(),
                                   div(style = "display:flex; justify-content:flex-end; margin-bottom:10px;", downloadButton("download_protein_num_pdf", "Download PDF")),
                                   plotOutput("proteins_number"),
                                   div(style = "display:flex; justify-content:flex-end; margin-top:10px;", downloadButton("download_protein_num_table", "Export table"))
                                 ),
                                 tabPanel(
                                   "Miss value statistics",
                                   br(),
                                   div(style = "display:flex; justify-content:flex-end; margin-bottom:10px;", downloadButton("download_na_pdf", "Download PDF")),
                                   plotOutput("Miss_value_statistics"),
                                   div(style = "display:flex; justify-content:flex-end; margin-top:10px;", downloadButton("download_na_table", "Export table"))
                                 )
                               ))
                         )
                       )
                     )
                   ),style = "font-size:125%;")
        ),



        ###Pattern clustering-----
        tabPanel(title = "Pattern clustering",
                 icon = icon("sitemap"),
                 fluidPage(
                   div(
                     class = "ep-page",
                     fluidRow(
                       column(
                         width = 10, offset = 1,
                         div(
                           class = "ep-hero",
                           div(class = "ep-title", "Pattern clustering"),
                           div(class = "ep-subtitle", "Cluster dynamic patterns from a SummarizedExperiment object and export both the clustered object and heatmap outputs in a compact workspace."),
                           tags$span(class = "ep-pill", style = "background:#F5F3FF; color:#6D28D9; border:1px solid #DDD6FE;", icon("sitemap"), " Pattern discovery"),
                           tags$span(class = "ep-pill", style = "background:#ECFEFF; color:#0F766E; border:1px solid #A5F3FC;", icon("download"), " Export ready")
                         )
                       )
                     ),
                     fluidRow(
                       column(
                         width = 3, offset = 1,
                         div(
                           class = "ep-card",
                           div(class = "ep-card-title", "Clustering controls"),
                           div(class = "ep-card-subtitle", "Upload an SE object, adjust clustering parameters, and export intermediate results."),
                           fileInput("matrix_file", tags$span("Upload SummarizedExperiment object", style = "white-space: nowrap;"), accept = ".Rds"),
                           div(style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap; margin-top:14px;",
                               uiOutput("reset_params_ui"),
                               uiOutput("download_ui"),
                               uiOutput('download_pattern_se_UI'))
                         )
                       ),
                       column(
                         width = 7,
                         div(
                           class = "ep-card",
                           div(class = "ep-card-title", "Pattern heatmap"),
                           div(class = "ep-card-subtitle", "Inspect clustering output and download the rendered figure when satisfied with the parameters."),
                           div(style = "margin-top:16px;", uiOutput("heatmap_pdf")),
                           div(style = "margin-top:12px;", uiOutput("download_heatmap_pdf_pattern_ui"))
                         )
                       )
                     )
                   ),style = "font-size:125%;")
        ),
        ###Differential-----
        tabPanel(title = "Differential analysis",
                 value =  "Results",
                 fluidPage(
                   div(
                     class = "ep-page",
                     fluidRow(
                       column(
                         width = 10, offset = 1,
                         div(
                           class = "ep-hero",
                           div(class = "ep-title", "Differential analysis"),
                           div(class = "ep-subtitle", "Upload a SummarizedExperiment object, generate DE results, and review the output table in a cleaner reporting layout."),
                           tags$span(class = "ep-pill", style = "background:#FEF2F2; color:#B91C1C; border:1px solid #FECACA;", icon("balance-scale"), " Differential statistics"),
                           tags$span(class = "ep-pill", style = "background:#EFF6FF; color:#1D4ED8; border:1px solid #BFDBFE;", icon("table"), " Interactive results")
                         )
                       )
                     ),
                     fluidRow(
                       column(
                         width = 10, offset = 1,
                         div(
                           class = "ep-card",
                           fluidRow(
                             column(
                               width = 8,
                               fileInput(
                                 inputId = "Upload_SE2",
                                 label = tags$span("Upload SummarizedExperiment object", style = "white-space: nowrap;"),
                                 accept = c(".Rds"),
                                 buttonLabel = "Browse...",
                                 placeholder = "No file selected"
                               )
                             ),
                             column(
                               width = 4,
                               div(style = "display:flex; justify-content:flex-end; align-items:flex-end; min-height:74px;", uiOutput('download_DEGs_ui'))
                             )
                           )
                         ),
                         div(
                           class = "ep-card",
                           div(class = "ep-card-title", "DE result table"),
                           div(class = "ep-card-subtitle", "Browse the computed differential analysis results and export them when needed."),
                           div(style = "margin-top:16px;", DTOutput("DEGs_table"))
                         )
                       )
                     )
                   ),style = "font-size:125%;"),
                 icon = icon('exchange-alt', lib = "font-awesome")


        ),

        ###Time series-------
        tabPanel(title = "Time series",
                 icon = icon('history'),
                 fluidPage(
                   div(
                     class = "ep-page",
                     fluidRow(
                       column(
                         width = 10, offset = 1,
                         div(
                           class = "ep-hero",
                           div(class = "ep-title", "Time series"),
                           div(class = "ep-subtitle", "Explore temporal fold-change trajectories, adjust export dimensions, and save the summarized SE object together with the generated trend plot."),
                           tags$span(class = "ep-pill", style = "background:#FFF7ED; color:#C2410C; border:1px solid #FED7AA;", icon("history"), " Temporal trends"),
                           tags$span(class = "ep-pill", style = "background:#EEF2FF; color:#4338CA; border:1px solid #C7D2FE;", icon("line-chart"), " FC trajectory")
                         )
                       )
                     ),
                     fluidRow(
                       column(
                         width = 3, offset = 1,
                         div(
                           class = "ep-card",
                           div(class = "ep-card-title", "Time-series controls"),
                           div(class = "ep-card-subtitle", "Upload an SE object, tune figure size, and inspect the dataset summary before exporting."),
                           fileInput("time_rds_file", tags$span("Upload SummarizedExperiment object", style = "white-space: nowrap;"), accept = ".Rds"),
                           sliderInput("FC_plot_width", "Plot width (px)", min = 400, max =1200, value = 600, step = 50),
                           sliderInput("FC_plot_height","Plot height (px)", min = 300, max = 1200, value = 800, step = 50),
                           conditionalPanel(
                             condition = "input.time_rds_file != null",
                             div(class = "ep-dark-panel", style = "min-height:160px; margin:16px 0; font-size:13px;", verbatimTextOutput("se_summary_time"))
                           ),
                           uiOutput('download_time_se_UI')
                         )
                       ),
                       column(
                         width = 7,
                         div(
                           class = "ep-card",
                           div(style = "display:flex; justify-content:space-between; align-items:flex-start; gap:12px; flex-wrap:wrap;",
                               div(
                                 div(class = "ep-card-title", "Time-course trend plot"),
                                 div(class = "ep-card-subtitle", "Render fold-change trajectories across ordered conditions and download the figure as PDF.")
                               ),
                               downloadButton("download_FC_fold_plot_pdf", "Download PDF")
                           ),
                           div(style = "margin-top:16px;", plotOutput("FC_fold_plot"))
                         )
                       )
                     )
                   ),style = "font-size:125%;")
        ),
        ###Tools -----
        tabPanel(title = "Tools",
                 icon = icon('wrench', lib = 'font-awesome'),
                 fluidPage(
                   div(
                     style = "background-color:#F8FAFC; min-height:100vh; padding:24px 0 36px 0;",

                     fluidRow(
                       column(
                         width = 10, offset = 1,
                         div(
                           style = "
                             background: linear-gradient(135deg, #FFFFFF 0%, #F8FBFF 100%);
                             border: 1px solid #E5E7EB;
                             border-radius: 16px;
                             padding: 24px 28px;
                             box-shadow: 0 8px 24px rgba(15, 23, 42, 0.06);
                             margin-bottom: 22px;
                           ",
                           div(
                             style = "display:flex; align-items:center; justify-content:space-between; gap:16px; flex-wrap:wrap;",
                             div(
                               div(
                                 "Tools",
                                 style = "font-size:28px; font-weight:800; color:#0F172A; line-height:1.2; margin-bottom:8px;"
                               ),
                               div(
                                 "Utility workflows for manipulating SummarizedExperiment objects and exploring target-gene correlation signals.",
                                 style = "font-size:15px; color:#475569; line-height:1.7; max-width:760px;"
                               )
                             ),
                             div(
                               style = "display:flex; gap:10px; flex-wrap:wrap;",
                               tags$span(
                                 icon("clone"), " SummarizedExperiment tools",
                                 style = "background:#EFF6FF; color:#1D4ED8; border:1px solid #BFDBFE; border-radius:999px; padding:8px 14px; font-size:13px; font-weight:600;"
                               ),
                               tags$span(
                                 icon("magic"), " Modern workspace",
                                 style = "background:#ECFDF5; color:#047857; border:1px solid #A7F3D0; border-radius:999px; padding:8px 14px; font-size:13px; font-weight:600;"
                               )
                             )
                           )
                         )
                       )
                     ),

                     fluidRow(
                       column(
                         width = 5, offset = 1,
                         div(
                           style = "
                             background:#FFFFFF;
                             border:1px solid #E5E7EB;
                             border-radius:16px;
                             padding:22px 24px;
                             box-shadow:0 6px 18px rgba(15, 23, 42, 0.05);
                             min-height:760px;
                             margin-bottom:20px;
                           ",
                           div(
                             style = "display:flex; align-items:center; gap:12px; margin-bottom:14px;",
                             div(
                               icon("scissors", lib = "font-awesome"),
                               style = "width:44px; height:44px; border-radius:12px; background:#EEF2FF; color:#4F46E5; display:flex; align-items:center; justify-content:center; font-size:18px;"
                             ),
                             div(
                               div("Subset SE", style = "font-size:22px; font-weight:700; color:#111827; line-height:1.2;"),
                               div("Filter samples and genes from an uploaded SummarizedExperiment object.", style = "font-size:14px; color:#6B7280; margin-top:4px;")
                             )
                           ),
                           div(
                             style = "background:#F8FAFC; border:1px solid #E2E8F0; border-radius:12px; padding:14px 16px; margin-bottom:18px; color:#475569; line-height:1.7; font-size:14px;",
                             tags$strong("Workflow"),
                             tags$br(),
                             "Upload a .rds object, open the selector modal, choose samples or genes, and export the subsetted SE."
                           ),
                           fileInput(
                             "upload_subset_se",
                             "Upload SummarizedExperiment (.rds)",
                             accept = ".rds"
                           ),
                           div(style = "margin-top:14px;", uiOutput('open_selector_UI')),
                           div(style = "margin-top:14px;", uiOutput('download_subset_se_UI')),
                           tags$hr(style = "border-top:1px solid #E5E7EB; margin:20px 0;"),
                           div(
                             style = "font-size:16px; font-weight:700; color:#1F2937; margin-bottom:10px;",
                             "Subset summary"
                           ),
                           div(
                             style = "background:#FFFFFF; color:#1F2937; border:1px solid #E5E7EB; border-radius:12px; padding:16px; min-height:260px; overflow:auto; font-size:13px;",
                             verbatimTextOutput("subset_info", placeholder = TRUE)
                           )
                         )
                       ),
                       column(
                         width = 5,
                         div(
                           style = "
                             background:#FFFFFF;
                             border:1px solid #E5E7EB;
                             border-radius:16px;
                             padding:22px 24px;
                             box-shadow:0 6px 18px rgba(15, 23, 42, 0.05);
                             min-height:760px;
                             margin-bottom:20px;
                           ",
                           div(
                             style = "display:flex; align-items:center; gap:12px; margin-bottom:14px;",
                             div(
                               icon("line-chart", lib = "font-awesome"),
                               style = "width:44px; height:44px; border-radius:12px; background:#ECFDF5; color:#059669; display:flex; align-items:center; justify-content:center; font-size:18px;"
                             ),
                             div(
                               div("Gene correlation", style = "font-size:22px; font-weight:700; color:#111827; line-height:1.2;"),
                               div("Upload an SE and retrieve genes most correlated with one or more target genes.", style = "font-size:14px; color:#6B7280; margin-top:4px;")
                             )
                           ),
                           div(
                             style = "background:#F0FDF4; border:1px solid #BBF7D0; border-radius:12px; padding:14px 16px; margin-bottom:18px; color:#166534; line-height:1.7; font-size:14px;",
                             tags$strong("Planned workflow"),
                             tags$br(),
                             "Upload a SummarizedExperiment object, enter one or more target genes, choose an assay, and inspect the most correlated genes across all samples."
                           ),
                           fileInput(
                             "upload_corr_se",
                             "Upload SummarizedExperiment (.rds)",
                             accept = ".rds"
                           ),
                           tags$hr(style = "border-top:1px solid #E5E7EB; margin:20px 0;"),
                           div(
                             style = "display:flex; justify-content:space-between; align-items:center; gap:12px; flex-wrap:wrap; margin-bottom:10px;",
                             div(
                               style = "font-size:16px; font-weight:700; color:#1F2937;",
                               "Correlation result preview"
                             ),
                             uiOutput("download_corr_gene_UI")
                           ),
                           div(
                             style = "background:#F8FAFC; border:1px solid #E2E8F0; border-radius:12px; padding:14px 16px; min-height:260px;",
                             tableOutput("corr_gene_table")
                           )
                         )
                       )
                     )
                   )
                 ),

        ),
        ###Enrichment-----
        tabPanel(
          title = "Functional enrichment",
          value = "Results",
          icon = icon("line-chart", lib = "font-awesome"),

          fluidPage(
            div(
              class = "ep-page",
              fluidRow(
                column(
                  width = 10, offset = 1,
                  div(
                    class = "ep-hero",
                    div(class = "ep-title", "Functional enrichment"),
                    div(class = "ep-subtitle", "Run GO, KEGG, GSEA, and STRING-based enrichment workflows in a cleaner tabbed workspace with consistent upload and result panels."),
                    tags$span(class = "ep-pill", style = "background:#EFF6FF; color:#1D4ED8; border:1px solid #BFDBFE;", icon("sitemap"), " GO / KEGG / GSEA"),
                    tags$span(class = "ep-pill", style = "background:#ECFDF5; color:#047857; border:1px solid #A7F3D0;", icon("share-alt"), " STRING integration")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 10, offset = 1,
                  div(
                    class = "ep-card ep-tabs",
                    tabsetPanel(
                      id = "visual_tabs",

              # --- 1. GO enrichment ---
              tabPanel(
                "GO enrichment", value = "GO_e",
                fluidRow(
                  column(
                    width = 3,
                    div(
                      class = "ep-card",
                      div(class = "ep-card-title", "GO enrichment input"),
                      div(class = "ep-card-subtitle", "Upload an Excel file containing a symbol column and optionally a group column."),
                      fileInput("enrich_upload_GO", NULL, buttonLabel = "Upload...", multiple = FALSE, accept = c(".xls", ".xlsx")),
                      uiOutput("download_enrich_data_ui")
                    )
                  ),
                  column(
                    width = 9,
                    div(
                      class = "ep-card",
                      div(class = "ep-card-title", "GO enrichment result"),
                      div(class = "ep-card-subtitle", "Review online enrichment results in an interactive table."),
                      div(style = "margin-top:16px; min-height:600px;", DT::dataTableOutput("enrichres_table"))
                    )
                  )
                )
              ),
              tabPanel(
                "KEGG enrichment", value = "KEGG_e",
                fluidRow(
                  column(
                    width = 3,
                    div(
                      class = "ep-card",
                      div(class = "ep-card-title", "KEGG enrichment input"),
                      div(class = "ep-card-subtitle", "Upload an Excel file containing a symbol column and optionally a group column."),
                      fileInput("enrich_upload_KEGG", NULL, buttonLabel = "Upload...", multiple = FALSE, accept = c(".xls", ".xlsx")),
                      uiOutput("download_kegg_enrich_data_ui")
                    )
                  ),
                  column(
                    width = 9,
                    div(
                      class = "ep-card",
                      div(class = "ep-card-title", "KEGG enrichment result"),
                      div(class = "ep-card-subtitle", "Review pathway enrichment results in an interactive table."),
                      div(style = "margin-top:16px; min-height:600px;", DT::dataTableOutput("kegg_enrichres_table"))
                    )
                  )
                )
              ),
              # --- 2. GSEA GO enrichment ---
              tabPanel(
                "GSEA GO enrichment", value = "GSEA_GO",
                fluidRow(
                  column(
                    width = 3,
                    div(
                      class = "ep-card",
                      div(class = "ep-card-title", "GSEA GO input"),
                      div(class = "ep-card-subtitle", "Upload an Excel file containing symbol and foldchange columns."),
                      fileInput("enrich_upload_gse_GO", NULL, buttonLabel = "Upload...", multiple = FALSE, accept = c(".xls", ".xlsx")),
                      uiOutput("download_gse_go_enrich_data_ui")
                    )
                  ),
                  column(
                    width = 9,
                    div(
                      class = "ep-card",
                      div(class = "ep-card-title", "GSEA GO result"),
                      div(class = "ep-card-subtitle", "Inspect enrichment scores and ranked-pathway statistics."),
                      div(style = "margin-top:16px; min-height:600px;", DT::dataTableOutput("ges_go_enrichres_table"))
                    )
                  )
                )
              ),
              tabPanel(
                "GSEA KEGG enrichment", value = "GSEA_GO",
                fluidRow(
                  column(
                    width = 3,
                    div(
                      class = "ep-card",
                      div(class = "ep-card-title", "GSEA KEGG input"),
                      div(class = "ep-card-subtitle", "Upload an Excel file containing symbol and foldchange columns."),
                      fileInput("enrich_upload_gse_kegg", NULL, buttonLabel = "Upload...", multiple = FALSE, accept = c(".xls", ".xlsx")),
                      uiOutput("download_gse_KEGG_enrich_data_ui")
                    )
                  ),
                  column(
                    width = 9,
                    div(
                      class = "ep-card",
                      div(class = "ep-card-title", "GSEA KEGG result"),
                      div(class = "ep-card-subtitle", "Inspect ranked KEGG pathway enrichment results in table form."),
                      div(style = "margin-top:16px; min-height:600px;", DT::dataTableOutput("ges_kegg_enrichres_table"))
                    )
                  )
                )
              ),
              # --- 3. STRING enrichment ---
              tabPanel(
                "STRING enrichment", value = "Stringe",
                fluidPage(
                  useShinyjs(),
                  fluidRow(
                    column(
                      5,
                      div(
                        class = "ep-card",
                        div(class = "ep-card-title", "STRING input"),
                        div(class = "ep-card-subtitle", "Upload an Excel file or paste genes directly for STRING link generation."),
                        fileInput("gene_file", "Upload gene Excel", accept = c(".xls", ".xlsx")),
                        tags$small("You can also input genes directly (newline/comma/space separated)"),
                        textAreaInput(
                          "genes", "Gene list", rows = 8,
                          value = "TP53\nMDM2\nCDKN1A\nRB1",
                          placeholder = "TP53, MDM2, CDKN1A, RB1"
                        )
                      )
                    ),
                    column(
                      7,
                      div(
                        class = "ep-card",
                        div(class = "ep-card-title", "STRING settings"),
                        div(class = "ep-card-subtitle", "Choose species and network rendering options before launching the STRING page."),
                        selectInput(
                          "species", "Species (NCBI Taxon)",
                          choices = c("Human (9606)" = 9606, "Mouse (10090)" = 10090, "Rat (10116)" = 10116),
                          selected = 9606
                        ),
                        radioButtons(
                          "flavor", "network_flavor",
                          choices = c("evidence", "confidence", "actions"),
                          selected = "evidence", inline = TRUE
                        ),
                        radioButtons(
                          "nettype", "network_type",
                          choices = c("functional", "physical"),
                          selected = "functional", inline = TRUE
                        ),
                        div(style = "margin-top:14px;", actionButton("open", "Generate STRING link", class = "btn btn-primary"))
                      )
                    )
                  )
                )
              )
                    )
                  )
                )
              )
            )
          )
        ),

        ###Visualization-----
        tabPanel(title = "Visualization",
                 value =  "Results",
                 icon = icon('chart-bar',lib="font-awesome"),
                 fluidPage(
                   div(
                     class = "ep-page",
                     fluidRow(
                       column(
                         width = 10, offset = 1,
                         div(
                           class = "ep-hero",
                           div(class = "ep-title", "Visualization"),
                           div(class = "ep-subtitle", "Create volcano plots, inspect gene expression, and render enrichment figures from uploaded result tables in a consistent plotting workspace."),
                           tags$span(class = "ep-pill", style = "background:#EEF2FF; color:#4338CA; border:1px solid #C7D2FE;", icon("bar-chart"), " Interactive plots"),
                           tags$span(class = "ep-pill", style = "background:#ECFDF5; color:#047857; border:1px solid #A7F3D0;", icon("download"), " Export ready")
                         )
                       )
                     ),
                     fluidRow(
                       column(
                         width = 10, offset = 1,
                         div(
                           class = "ep-card ep-tabs",
                           tabsetPanel(
                             id = "visual_tabs",
                             tabPanel(
                               "Volcano Plot", value = "plotA",
                               fluidRow(
                                 column(
                                   width = 4,
                                   div(
                                     class = "ep-card",
                                     div(class = "ep-card-title", "Volcano controls"),
                                     div(class = "ep-card-subtitle", "Upload a DEG table and tune thresholds for fold change, FDR, and label count."),
                                     fileInput(
                                       inputId = "Upload_DEG_table1",
                                       label = tags$span("Upload DEG table", style = "white-space: nowrap;"),
                                       accept = c(".xlsx"),
                                       buttonLabel = "Browse...",
                                       placeholder = "No file selected"
                                     ),
                                     sliderInput("Volcano_log2FC", "log2FC threshold",
                                                 min = 0, max = 3, value = 1, step = 0.1),
                                     sliderInput("Volcano_FDR", "padj threshold",
                                                 min = 0.001, max = 0.05, value = 0.05, step = 0.01),
                                     sliderInput("Volcano_topN", "Highlight gene numbers",
                                                 min = 0, max = 50, value = 5, step = 1)
                                   )
                                 ),
                                 column(
                                   width = 8,
                                   div(
                                     class = "ep-card",
                                     div(style = "display:flex; justify-content:space-between; align-items:flex-start; gap:12px; flex-wrap:wrap;",
                                         div(
                                           div(class = "ep-card-title", "Volcano plot"),
                                           div(class = "ep-card-subtitle", "Preview significant genes and export the interactive volcano figure as PDF.")
                                         ),
                                         downloadButton("download_vol_pdf", "Download PDF")
                                     ),
                                     div(style = "margin-top:16px;", girafeOutput("volcano", height = "700px"))
                                   )
                                 )
                               )
                             ),

                             tabPanel(
                               "Gene expression",
                               fluidRow(
                                 column(
                                   width = 4,
                                   div(
                                     class = "ep-card",
                                     div(class = "ep-card-title", "Gene expression controls"),
                                     div(class = "ep-card-subtitle", "Upload an SE object, choose samples and genes, then adjust export dimensions."),
                                     fileInput("gene_exp_rds", "Upload SE", accept = c(".Rds")),
                                     uiOutput('choose_samples_UI'),
                                     uiOutput("gene_selector_ui"),
                                     sliderInput("gene_exp_width", "Plot width (px)", min = 400, max = 2000, value = 600, step = 50),
                                     sliderInput("gene_exp_height","Plot height (px)", min = 300, max = 1000, value = 400, step = 50)
                                   )
                                 ),
                                 column(
                                   width = 8,
                                   div(
                                     class = "ep-card",
                                     div(style = "display:flex; justify-content:space-between; align-items:flex-start; gap:12px; flex-wrap:wrap;",
                                         div(
                                           div(class = "ep-card-title", "Gene expression plot"),
                                           div(class = "ep-card-subtitle", "Render expression profiles for selected genes and download the figure directly.")
                                         ),
                                         uiOutput("down_gene_exp_ui")
                                     ),
                                     div(style = "margin-top:16px;", plotOutput("gene_exp"))
                                   )
                                 )
                               )
                             ),
                             tabPanel(
                               "Function enrichment plot",
                               fluidRow(
                                 column(
                                   width = 4,
                                   div(
                                     class = "ep-card",
                                     div(class = "ep-card-title", "GO plot controls"),
                                     div(class = "ep-card-subtitle", "Upload GO results and adjust label wrapping, top term count, and export dimensions."),
                                     fileInput("go_file", "Upload GO results (Excel)", accept = c(".xlsx", ".xls")),
                                     uiOutput("go_entry_selector_ui"),
                                     selectInput(
                                       "go_group_by",
                                       "TopN ranking metric",
                                       choices = c("-log10(p.adjust)", "GeneRatio", "RichFactor", "Count"),
                                       selected = "-log10(p.adjust)"
                                     ),
                                     selectInput(
                                       "go_x_axis",
                                       "Dotplot X axis",
                                       choices = c("gene_ratio", "RichFactor"),
                                       selected = "gene_ratio"
                                     ),
                                     sliderInput("go_topn", "Top N terms", min = 5, max = 50, value = 10, step = 1),
                                     sliderInput("go_label_width", "Label wrap width", min = 20, max = 80, value = 30, step = 2),
                                     sliderInput("go_plot_width", "Plot width (px)", min = 400, max = 1200, value = 600, step = 50),
                                     sliderInput("go_plot_height","Plot height (px)", min = 300, max = 600, value = 400, step = 50)
                                   )
                                 ),
                                 column(
                                   width = 8,
                                   div(
                                     class = "ep-card ep-tabs",
                                     div(class = "ep-card-title", "Enrichment dot plots"),
                                     div(class = "ep-card-subtitle", "Switch between plot styles and export the current figure when needed."),
                                     div(style = "margin-top:16px;",
                                         tabsetPanel(
                                           tabPanel("Style 1",
                                                    div(style = "display:flex; justify-content:flex-end; margin:10px 0;", uiOutput("down_GO_pdf_style1")),
                                                    plotOutput("GO_enrich_plot1",height = '610px')
                                           ),
                                           tabPanel("Style 2",
                                                    div(style = "display:flex; justify-content:flex-end; margin:10px 0;", uiOutput("down_GO_pdf_style2")),
                                                    plotOutput("GO_enrich_plot2",height = '610px')
                                           )
                                         ))
                                   )
                                 )
                               )
                             ),
                             tabPanel(
                               "String enrichment plot",
                               fluidRow(
                                 column(
                                   width = 4,
                                   div(
                                     class = "ep-card",
                                     div(class = "ep-card-title", "STRING plot controls"),
                                     div(class = "ep-card-subtitle", "Upload STRING enrichment output and tune label wrapping, top terms, and figure size."),
                                     fileInput("string_go_file", "Upload string enrichment results", accept = c(".tsv",".xlsx", ".xls")),
                                     sliderInput("string_go_go_topn", "Top N terms", min = 5, max = 50, value = 10, step = 1),
                                     sliderInput("string_go_go_label_width", "Label wrap width", min = 20, max = 80, value = 30, step = 2),
                                     sliderInput("string_go_width", "Plot width (px)", min = 400, max = 1200, value = 600, step = 50),
                                     sliderInput("string_go_plot_height","Plot height (px)", min = 300, max = 600, value = 400, step = 50)
                                   )
                                 ),
                                 column(
                                   width = 8,
                                   div(
                                     class = "ep-card",
                                     div(style = "display:flex; justify-content:space-between; align-items:flex-start; gap:12px; flex-wrap:wrap;",
                                         div(
                                           div(class = "ep-card-title", "STRING enrichment plot"),
                                           div(class = "ep-card-subtitle", "Render the selected STRING enrichment figure and export it directly.")
                                         ),
                                         uiOutput("down_GO_pdf_style3")
                                     ),
                                     div(style = "margin-top:16px;", plotOutput("GO_enrich_plot3",height = '610px'))
                                   )
                                 )
                               )
                             )
                           )
                         )
                       )
                     )
                   )
              )

        ),

        ###
        tabPanel(title = "Tutorials",
                 icon = icon('bookmark',lib = 'glyphicon'),
                 p("Tutorials content.")),

        tabPanel(
          title = "Contact",
          icon = icon("envelope", lib = "glyphicon"),

          fluidPage(
            fluidRow(
              column(
                width = 10, offset = 1,

                div(
                  style = "
            background:#FFFFFF;
            border:1px solid #E5E7EB;
            border-radius:12px;
            padding:22px 28px;
            box-shadow:0 2px 8px rgba(0,0,0,0.04);
            margin-top:18px;
            margin-bottom:22px;
          ",

                  div(
                    "Contact & Support",
                    style = "
              font-size:26px;
              font-weight:700;
              color:#1F2937;
              border-left:5px solid #2C7BE5;
              padding-left:12px;
              margin-bottom:18px;
              line-height:1.2;
            "
                  ),

                  p(style="margin-bottom:12px; line-height:1.75; color:#374151;",
                    "This page provides the recommended channels for questions, bug reports, and feature requests related to EasyProtein."
                  ),

                  tags$hr(style="border-top:1px solid #E5E7EB; margin:18px 0;"),

                  div(
                    tags$h4("Project repository", style="margin:0 0 8px 0; font-weight:700; color:#111827;"),
                    p(style="margin:0; line-height:1.75; color:#374151;",
                      "Source code and documentation: ",
                      tags$a(
                        href = "https://github.com/yuanlizhanshi/EasyProtein",
                        target = "_blank",
                        "github.com/yuanlizhanshi/EasyProtein"
                      )
                    )
                  ),

                  tags$hr(style="border-top:1px solid #E5E7EB; margin:18px 0;"),

                  div(
                    tags$h4("Bug reports & feature requests", style="margin:0 0 8px 0; font-weight:700; color:#111827;"),
                    tags$ul(
                      style="margin:0; padding-left:18px; line-height:1.8; color:#374151;",
                      tags$li(
                        "Please use GitHub Issues for reproducible bug reports and feature requests: ",
                        tags$a(
                          href = "https://github.com/yuanlizhanshi/EasyProtein/issues",
                          target = "_blank",
                          "Issues"
                        )
                      ),
                      tags$li("When reporting a bug, include: a minimal example, input data format, expected vs observed results, and the console log/error message."),
                      tags$li("If the issue is related to visualization, screenshots are helpful.")
                    )
                  ),

                  tags$hr(style="border-top:1px solid #E5E7EB; margin:18px 0;"),

                  div(
                    tags$h4("User questions", style="margin:0 0 8px 0; font-weight:700; color:#111827;"),
                    p(style="margin:0; line-height:1.75; color:#374151;",
                      "For usage questions (e.g., recommended inputs, workflow steps, or interpretation of results), please open an Issue with the label ",
                      tags$code("question"),
                      " to keep the discussion searchable and reproducible."
                    )
                  ),

                  tags$hr(style="border-top:1px solid #E5E7EB; margin:18px 0;"),

                  div(
                    tags$h4("How to cite", style="margin:0 0 8px 0; font-weight:700; color:#111827;"),
                    p(style="margin:0; line-height:1.75; color:#374151;",
                      "If you use EasyProtein in a publication, please cite the project repository. A formal software manuscript and/or Zenodo DOI can be added here once available."
                    ),
                    tags$pre(
                      style="background:#F9FAFB; border:1px solid #E5E7EB; border-radius:10px; padding:12px 14px; margin-top:10px; white-space:pre-wrap;",
                      "EasyProtein. Open-source platform for downstream analysis of quantitative proteomics data.\nhttps://github.com/yuanlizhanshi/EasyProtein"
                    )
                  ),

                  tags$hr(style="border-top:1px solid #E5E7EB; margin:18px 0;"),

                  div(
                    tags$h4("Reproducibility checklist", style="margin:0 0 8px 0; font-weight:700; color:#111827;"),
                    tags$ul(
                      style="margin:0; padding-left:18px; line-height:1.8; color:#374151;",
                      tags$li("EasyProtein version (or Git commit hash)."),
                      tags$li("R version and OS (Linux/Windows/macOS)."),
                      tags$li("Key package versions: Shiny, bslib, and major analysis dependencies."),
                      tags$li("Input matrix type and preprocessing method (e.g., DIA protein matrix, LFQ/TMT output).")
                    )
                  ),

                  tags$hr(style="border-top:1px solid #E5E7EB; margin:18px 0;"),

                  div(
                    tags$h4("Maintainer", style="margin:0 0 8px 0; font-weight:700; color:#111827;"),
                    p(style="margin:0; line-height:1.75; color:#374151;",
                      "Maintained by ",
                      tags$strong("Yunhui Kong"),
                    )
                    ,p(style="margin:6px 0 0 0; line-height:1.75; color:#374151;",
                       "Email: ", tags$a(href="mailto:kongyunhui1@gmail.com", "kongyunhui1@gmail.com"))
                  )
                )
              )
            ),

            # page background + font
            div(style="height:18px;"),
            tags$style(HTML("
      body { background-color: #F8FAFC; }
    "))
          )
        ),
        nav_spacer(),
        nav_item(tags$a(shiny::icon("github"), "EasyProtein(v0.8.1)", href = "https://github.com/yuanlizhanshi/EasyProtein", target = "_blank"))
      )

    #
    )
  )
}



