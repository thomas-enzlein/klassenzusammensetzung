#' Start the Klassenzusammensetzung Shiny App
#'
#' @description Launches the interactive Shiny application for algorithmic class composition.
#' 
#' @import shiny bslib shinycssloaders shinyhelper
#' @importFrom DT DTOutput renderDT datatable formatRound
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "Klassenzusammensetzung")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `Klassenzusammensetzung`.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
