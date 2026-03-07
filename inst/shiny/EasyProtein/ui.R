
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
                     fluidRow(
                       column(
                         width = 6,
                         fileInput(
                           inputId = "upload_tsv",
                           label = "Upload TSV File",
                           accept = c(".tsv"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected"
                         )
                       ),
                       column(
                         width = 6,
                         uiOutput('edit_coldata_ui')
                       )
                     ),
                     verbatimTextOutput("se_summary"),

                     downloadButton("download_se", "Download results")
                   ),style = "font-size:125%;width:80%;")
        ),
        ###Quality Control----
        tabPanel(title = "Quality Control",
                 icon = icon('search',lib = 'glyphicon'),
                 fluidPage(
                   div(
                     sidebarLayout(
                       sidebarPanel(
                         fileInput("se_file", "Upload SummarizedExperiment (Rds)", accept = ".Rds"),

                         uiOutput('edit_samples_UI'),
                         uiOutput('edit_genes_UI'),
                         uiOutput('download_edit_sample'),
                         br(), br(),
                         sliderInput("plot_width", "Plot width (px)",
                                     min = 100, max = 3000, value = 480, step = 20),
                         sliderInput("plot_height", "Plot height (px)",
                                     min = 100, max = 3000, value = 340, step = 20),
                         br(), br(),

                         textOutput("included_info")
                       ),
                       mainPanel(
                         tabsetPanel(
                           tabPanel(
                             "PCA plot",
                             br(),
                             fluidRow(
                               column(4, uiOutput("color_selector")),
                               column(4, uiOutput("label_selector")),
                               column(4, downloadButton("download_pca_pdf", "Download PDF"))
                             ),
                             br(),
                             plotOutput("pca_plot"),

                           ),
                           tabPanel(
                             "Intensity density",
                             br(),
                             downloadButton("download_protein_density_pdf", "Download PDF"),
                             br(),
                             plotOutput("intensity_density")

                           ),
                           tabPanel(
                             "CV density",
                             br(),
                             downloadButton("download_cv_density_pdf", "Download PDF"),
                             br(),
                             plotOutput("cv_density")

                           ),
                           tabPanel(
                             "Number of proteins detected",
                             br(),
                             downloadButton("download_protein_num_pdf", "Download PDF"),
                             br(),
                             plotOutput("proteins_number")


                           ),
                           tabPanel(
                             "Miss value statistics",
                             br(),
                             downloadButton("download_na_pdf", "Download PDF"),
                             br(),
                             plotOutput("Miss_value_statistics")

                           )
                         )
                       )
                     )
                   ),style = "font-size:125%;width:80%;")
        ),



        ###Pattern clustering-----
        tabPanel(title = "Pattern clustering",
                 icon = icon("project-diagram"),
                 fluidPage(
                   div(
                     sidebarLayout(
                       sidebarPanel(
                         fileInput("matrix_file", tags$span("Upload SummarizedExperiment object", style = "white-space: nowrap;"), accept = ".Rds"),
                         div(
                           style = "display:flex; align-items:center; gap:10px;",
                           uiOutput("reset_params_ui"),
                           uiOutput("download_ui"),
                           uiOutput('download_pattern_se_UI')

                         )
                       ),
                       mainPanel(
                         uiOutput("heatmap_pdf"),
                         uiOutput("download_heatmap_pdf_pattern_ui")
                       )
                     )
                   ),style = "font-size:125%;width:90%;")
        ),
        ###Differential-----
        tabPanel(title = "Differential analysis",
                 value =  "Results",
                 fluidPage(div(
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
                       uiOutput('download_DEGs_ui')
                     )
                   )
                 ),
                 div(
                   DTOutput("DEGs_table")
                 )
                 ,style = "font-size:125%;width:80%;"),
                 icon = icon('exchange-alt', lib = "font-awesome")


        ),

        ###Time series-------
        tabPanel(title = "Time series",
                 icon = icon('history'),
                 fluidPage(
                   div(
                     sidebarLayout(
                       sidebarPanel(
                         fileInput("time_rds_file", tags$span("Upload SummarizedExperiment object", style = "white-space: nowrap;"), accept = ".Rds"),
                         sliderInput("FC_plot_width", "Plot width (px)", min = 400, max =1200, value = 600, step = 50),
                         sliderInput("FC_plot_height","Plot height (px)", min = 300, max = 1200, value = 800, step = 50),

                         div(
                           style = "display:flex; align-items:center; gap:10px;",
                           verbatimTextOutput("se_summary_time")
                         ),

                         uiOutput('download_time_se_UI')
                       ),
                       mainPanel(
                         br(),
                         downloadButton("download_FC_fold_plot_pdf", "Download PDF"),
                         br(),
                         plotOutput("FC_fold_plot")
                       )
                     )
                   ),style = "font-size:125%;")
        ),
        ###Tools -----
        tabPanel(title = "Tools",
                 icon = icon('wrench', lib = 'font-awesome'),
                 tabPanel("Subset SE", value = "se",
                          fluidPage(
                            sidebarPanel(
                              fileInput("upload_subset_se", "Upload SummarizedExperiment (.rds)",
                                        accept = ".rds"),
                              tags$hr(),
                              uiOutput('open_selector_UI'),
                              tags$hr(),
                              uiOutput('download_subset_se_UI'),
                              br(),
                              verbatimTextOutput("subset_info"),

                            ),
                            mainPanel(
                              # h4("SE Dimension Preview"),
                              # tableOutput("se_dim_table")
                            )

                          )
                 ),

        ),
        ###Enrichment-----
        tabPanel(
          title = "Functional enrichment",
          value = "Results",
          icon = icon("chart-line", lib = "font-awesome"),

          fluidPage(
            tags$head(
              tags$style(HTML("
  /* Control tab width and content width */
        .nav-tabs > li > a {
          min-width: 150px;
        }
        .tab-content {
          margin: 0 auto;
        }
      "))
            ),

            tabsetPanel(
              id = "visual_tabs",

              # --- 1. GO enrichment ---
              tabPanel(
                "GO enrichment", value = "GO_e",
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    card(
                      card_header("Online GO enrichment tools"),
                      h3(HTML("Please upload an Excel file (xls or xlsx) with a column <b>symbol</b> (Autodetect species)  and </b>group</b> column (Optional)")),
                      fileInput("enrich_upload_GO", NULL, buttonLabel = "Upload...", multiple = FALSE, accept = c(".xls", ".xlsx")),
                      uiOutput("download_enrich_data_ui")
                    )
                  ),
                  mainPanel(
                    width = 9,
                    div(
                      br(),
                      DT::dataTableOutput("enrichres_table"), style = "height: 600px;")
                  )
                )
              ),
              tabPanel(
                "KEGG enrichment", value = "KEGG_e",
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    card(
                      card_header("Online KEGG enrichment tools"),
                      h3(HTML("Please upload an Excel file (xls or xlsx) with a column <b>symbol</b> (Autodetect species)  and </b>group</b> column (Optional)")),
                      fileInput("enrich_upload_KEGG", NULL, buttonLabel = "Upload...", multiple = FALSE, accept = c(".xls", ".xlsx")),
                      uiOutput("download_kegg_enrich_data_ui")
                    )
                  ),
                  mainPanel(
                    width = 9,
                    div(
                      br(),
                      DT::dataTableOutput("kegg_enrichres_table"), style = "height: 600px;")
                  )
                )
              ),
              # --- 2. GSEA GO enrichment ---
              tabPanel(
                "GSEA GO enrichment", value = "GSEA_GO",
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    card(
                      card_header("Online GSEA GO enrichment"),
                      h3(HTML("Please upload an Excel file (xls or xlsx) with columns <b>symbol</b> and <b>foldchange</b>.")),
                      fileInput("enrich_upload_gse_GO", NULL, buttonLabel = "Upload...", multiple = FALSE, accept = c(".xls", ".xlsx")),
                      uiOutput("download_gse_go_enrich_data_ui")
                    )
                  ),
                  mainPanel(
                    width = 9,
                    div(
                      br(),
                      DT::dataTableOutput("ges_go_enrichres_table"), style = "height: 600px;"
                      )
                  )
                )
              ),
              tabPanel(
                "GSEA KEGG enrichment", value = "GSEA_GO",
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    card(
                      card_header("Online GSEA KEGG enrichment"),
                      h3(HTML("Please upload an Excel file (xls or xlsx) with columns <b>symbol</b> and <b>foldchange</b>.")),
                      fileInput("enrich_upload_gse_kegg", NULL, buttonLabel = "Upload...", multiple = FALSE, accept = c(".xls", ".xlsx")),
                      uiOutput("download_gse_KEGG_enrich_data_ui")
                    )
                  ),
                  mainPanel(
                    width = 9,
                    div(
                      br(),
                      DT::dataTableOutput("ges_kegg_enrichres_table"), style = "height: 600px;"
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
                      6,
                      tags$br(),
                      fileInput("gene_file", "Upload gene Excel", accept = c(".xls", ".xlsx")),
                      tags$small("You can also input genes directly (newline/comma/space separated)"),
                      textAreaInput(
                        "genes", "Gene list", rows = 8,
                        value = "TP53\nMDM2\nCDKN1A\nRB1",
                        placeholder = "TP53, MDM2, CDKN1A, RB1"
                      )
                    ),
                    column(
                      6,
                      tags$br(),
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
                      br(),
                      actionButton("open", "Generate STRING link", class = "btn btn-primary")
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
                   tags$head(
                     tags$style(HTML("
                        /* Control tabPanel width under nav-tabs */
                        .nav-tabs > li > a {
                          min-width: 200px;   /* Minimum width per tab */
                          text-align: center; /* Center text */
                        }
                      "))
                   ),
                   tabsetPanel(
                     id = "visual_tabs",
                     tabPanel(
                       "Volcano Plot", value = "plotA",
                       sidebarLayout(

                         sidebarPanel(
                           width = 4,
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
                         ),


                         mainPanel(
                           width = 8,
                           div(style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                               downloadButton("download_vol_pdf", "Download PDF")
                           ),
                           girafeOutput("volcano", height = "700px")
                         )
                       )
                     ),

                     tabPanel("Gene expression",
                              fluidPage(
                                sidebarLayout(
                                  sidebarPanel(
                                    fileInput("gene_exp_rds", "Upload SE", accept = c(".Rds")),
                                    uiOutput('choose_samples_UI'),
                                    uiOutput("gene_selector_ui"),
                                    br(),
                                    br(),
                                    sliderInput("gene_exp_width", "Plot width (px)", min = 400, max = 2000, value = 600, step = 50),
                                    sliderInput("gene_exp_height","Plot height (px)", min = 300, max = 1000, value = 400, step = 50)
                                  ),
                                  mainPanel(
                                    uiOutput("down_gene_exp_ui"),
                                    br(),
                                    plotOutput("gene_exp")

                                  )
                                )
                              )
                     ),
                     tabPanel(
                       "Function enrichment plot",
                       #icon = icon('dots-vertical', lib = 'glyphicon'),
                       fluidPage(
                         sidebarLayout(
                           sidebarPanel(
                             fileInput("go_file", "Upload GO results (Excel)", accept = c(".xlsx", ".xls")),
                             sliderInput("go_topn", "Top N terms", min = 5, max = 50, value = 10, step = 1),
                             sliderInput("go_label_width", "Label wrap width", min = 20, max = 80, value = 30, step = 2),
                             sliderInput("go_plot_width", "Plot width (px)", min = 400, max = 1200, value = 600, step = 50),
                             sliderInput("go_plot_height","Plot height (px)", min = 300, max = 600, value = 400, step = 50)
                           ),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Style 1",
                                        br(),
                                        plotOutput("GO_enrich_plot1",height = '610px'),
                                        uiOutput("down_GO_pdf_style1")
                               ),
                               tabPanel("Style 2",
                                        br(),
                                        plotOutput("GO_enrich_plot2",height = '610px'),
                                        uiOutput("down_GO_pdf_style2")
                               )
                             )
                           )
                         )
                       )
                     ),
                     tabPanel("String enrichment plot",
                              fluidPage(
                                sidebarLayout(
                                  sidebarPanel(
                                    fileInput("string_go_file", "Upload string enrichment results", accept = c(".tsv",".xlsx", ".xls")),
                                    sliderInput("string_go_go_topn", "Top N terms", min = 5, max = 50, value = 10, step = 1),
                                    sliderInput("string_go_go_label_width", "Label wrap width", min = 20, max = 80, value = 30, step = 2),
                                    sliderInput("string_go_width", "Plot width (px)", min = 400, max = 1200, value = 600, step = 50),
                                    sliderInput("string_go_plot_height","Plot height (px)", min = 300, max = 600, value = 400, step = 50)
                                  ),
                                  mainPanel(
                                    uiOutput("down_GO_pdf_style3"),
                                    br(),
                                    plotOutput("GO_enrich_plot3",height = '610px')

                                  )
                                )
                              )
                     )
                     # tabPanel("Plot D", value = "plotD",
                     #          fluidPage(
                     #            h4("Place Plot D here"),
                     #            plotOutput("plot_D")
                     #          )
                     # ),
                     # tabPanel("Plot E", value = "plotE",
                     #          fluidPage(
                     #            h4("Place Plot E here"),
                     #            plotOutput("plot_E")
                     #          )
                     # )
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
        nav_item(tags$a(shiny::icon("github"), "Kongmou", href = "https://github.com/yuanlizhanshi", target = "_blank"))
      )

    #
    )
  )
}



