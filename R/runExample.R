#' Run Shiny App demonstrating the package
#'
#' @param ... passed to [shiny::runApp()], e.g. `port`, `launch.browser`
#'
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   bpbounds::runExample()
#' }
#' @export
runExample <- function(...) {
  appDir <- system.file("shiny-examples", "myapp", package = "bpbounds")
  if (appDir == "") {
    stop("Could not find example app.")
  }

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "Package \"shiny\" must be installed to use this function.",
      call. = FALSE
    )
  }
  shiny::runApp(appDir, ...)
}
