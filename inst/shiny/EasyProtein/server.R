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
    se = NULL,                 # 主 SummarizedExperiment
    se_filtered = NULL,        # 过滤后的 SE
    DEGs = NULL,               # 差异分析结果
    genes_keep = NULL,         # 用户上传的保留基因列表
    removed_samples = NULL,    # 用户移除的样本
    enriched_data = NULL,      # 富集分析数据（GO/KEGG/GSEA）
    pattern_matrix = NULL,     # 模式识别矩阵
    row_info = NULL,           # 聚类结果（行）
    col_info = NULL,           # 聚类结果（列）
    pdf_path = NULL,           # 热图 PDF 路径
    string_url = NULL          # STRING 网络链接
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
