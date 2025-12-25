#' Launch EasyProtein Shiny App
#'
#' @description
#' Launch the EasyProtein interactive Shiny application.
#'
#' @param ... Passed to shiny::runApp().
#'
#' @importFrom shiny runApp
#' @export
launch_EasyProtein <- function(...) {
  app_dir <- system.file(
    "shiny",
    "easyprotein",
    package = "EasyProtein"
  )

  if (app_dir == "") {
    stop(
      "EasyProtein Shiny app not found.\n",
      "Please make sure the package is installed correctly."
    )
  }

  runApp(
    appDir = app_dir,
    launch.browser = TRUE,
    ...
  )
}
