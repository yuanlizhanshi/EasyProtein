mod_home_server <- function(input, output, session) {
  output$HOME_output_text <- renderUI({
    div(
      p("Easy Protein, Version 0.1 (Build date: 2025-09-23)"),
      p("Easy Protein, Version 0.2 (Build date: 2025-11-29)"),
      p("Easy Protein, Version 0.2 (Build date: 2025-12-21)")
    )
  })
}