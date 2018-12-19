#' Run Shiny App demonstrating the package
#'
#' @param ... passed to runApp, e.g. port, launch.browser
#'
#' @examples
#' \donttest{
#' bpbounds::runExample()
#' }
#'
#' @export
runExample <- function(...) {
  appDir <- system.file("shiny-examples", "myapp", package = "bpbounds")
  if (appDir == "") {
    stop("Could not find example app.")
  }
  shiny::runApp(appDir, ...)
}
