#' Run the kraken app
#'
#' @export
#' @import shiny
runKraken <- function() {
  appDir <- system.file("app", package = "krakenApp")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `krakenApp`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
