#' Run the fgShiny app
#'
#' @export
#' @import shiny
runfgShiny <- function() {
  appDir <- system.file("app", package = "fgShinyApp")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `fgShinyApp`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
