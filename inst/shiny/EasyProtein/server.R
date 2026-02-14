source("server/mod_home_server.R")
source("server/mod_upload_server.R")
source("server/mod_QC_server.R")
source("server/mod_deg_server.R")
source("server/mod_pattern_server.R")
source("server/mod_time_series.R")
source("server/mod_enrich_server.R")
source("server/mod_visualization_server.R")
source("server/mod_tools_server.R")
server <- function(input, output, session) {

  rv <- reactiveValues(
    se = NULL,                 # main SummarizedExperiment
    se_filtered = NULL,        # filtered SE
    DEGs = NULL,               # differential analysis results
    genes_keep = NULL,         # user-uploaded gene keep list
    removed_samples = NULL,    # user-removed samples
    enriched_data = NULL,      # enrichment data (GO/KEGG/GSEA)
    pattern_matrix = NULL,     # pattern discovery matrix
    row_info = NULL,           # clustering results (rows)
    col_info = NULL,           # clustering results (columns)
    pdf_path = NULL,           # heatmap PDF path
    string_url = NULL          # STRING network link
  )

  
  mod_home_server(input, output, session)
  
  mod_upload_server(input, output, session, rv)
  
  mod_QC_server(input, output, session)
  
  mod_deg_server(input, output, session, rv)

  mod_pattern_server(input, output, session)
  
  mod_time_series_server(input, output, session)
  
  mod_enrich_server(input, output, session)
  # 
  mod_visualization_server(input, output, session)
  # 
  mod_tools_server(input, output, session)

  # 
  # observe({
  #   shinyjs::addClass(selector = "body", class = "easyprotein-app")
  # })
  # 
  # output$app_version <- renderText({
  #   paste0("Easy Protein, Version 0.1 (Build date: ", Sys.Date(), ")")
  # })
  
}
