#' Launch EasyProtein Shiny App
#'
#' @description
#' Launch the EasyProtein interactive Shiny application.
#'
#' @param install_missing Logical. Whether to install missing packages automatically.
#' @param ... Passed to shiny::runApp().
#'
#' @importFrom shiny runApp
#' @export
launch_EasyProtein <- function(install_missing = TRUE, ...) {

  ## --------------------------------------------------
  ## 1. Check required Shiny-related packages
  ## --------------------------------------------------
  required_pkgs <- c(
    "bslib",
    "DT",
    "htmltools",
    "httr",
    "reactable",
    "shiny",
    "shinyjqui",
    "shinyjs",
    "shinydashboard",
    "shinythemes",
    "shinyWidgets",
    "ggiraph"
  )

  missing_pkgs <- required_pkgs[!vapply(
    required_pkgs,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )]

  if (length(missing_pkgs) > 0) {
    msg <- paste(
      "The following packages are required to run the EasyProtein Shiny app:\n",
      paste(missing_pkgs, collapse = ", ")
    )

    if (!install_missing) {
      stop(msg, call. = FALSE)
    }

    message(msg)
    message("Installing missing packages...")

    install.packages(missing_pkgs)
  }

  ## --------------------------------------------------
  ## 2. Locate Shiny app directory
  ## --------------------------------------------------
  app_dir <- system.file(
    "shiny",
    "easyprotein",
    package = "EasyProtein"
  )

  if (app_dir == "") {
    stop(
      "EasyProtein Shiny app not found.\n",
      "Please make sure the package is installed correctly.",
      call. = FALSE
    )
  }

  ## --------------------------------------------------
  ## 3. Launch Shiny app
  ## --------------------------------------------------
  shiny::runApp(
    appDir = app_dir,
    launch.browser = TRUE,
    ...
  )
}
