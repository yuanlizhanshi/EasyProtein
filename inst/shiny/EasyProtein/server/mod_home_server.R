mod_home_server <- function(input, output, session) {
  output$HOME_output_text <- renderUI({
    HTML(
      paste0(
        "<p style='margin-bottom:14px; line-height:1.75; color:#374151;'>",
        "EasyProtein is an open-source platform for downstream analysis of quantitative proteomics data.",
        "</p>",

        "<p style='margin-bottom:14px; line-height:1.75; color:#374151;'>",
        "The framework integrates an R package with a Shiny-based web interface to provide a reproducible environment for data exploration and analysis.",
        "</p>",

        "<p style='margin-bottom:14px; line-height:1.75; color:#374151;'>",
        "EasyProtein implements modules for data preprocessing, statistical testing, clustering analysis, and visualization of proteomics datasets.",
        "</p>",

        "<p style='margin-bottom:14px; line-height:1.75; color:#374151;'>",
        "The platform is designed to facilitate the analysis and interpretation of large-scale proteomics experiments through an interactive workflow and standardized analysis procedures.",
        "</p>",

        "<p style='margin-bottom:0px; line-height:1.75; color:#374151;'>",
        "Source code and documentation are available at: ",
        "<a href='https://github.com/yuanlizhanshi/EasyProtein' target='_blank'>",
        "https://github.com/yuanlizhanshi/EasyProtein</a>",
        "</p>"
      )
    )
  })
}
