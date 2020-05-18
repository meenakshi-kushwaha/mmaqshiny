
#' @title mmaqshiny: R-Shiny package to explore air quality mobile monitoring data
#' @description The R-Shiny package we present here is for analysing, visualising and spatial-mapping of high-resolution air quality data collected by specific devices installed on a moving platform.
#' @keywords mmaqshiny, air quality, shiny, mobile monitoring
#' @examples
#' \dontrun{
#' library(mmaqshiny)
#' mmaqshiny::mmaqshiny_run()
#' }
#' @export
mmaqshiny_run <- function() {
  Directory <- system.file("shiny", package = "mmaqshiny")
  if (Directory == "") {
    stop("Try reinstalling the package `mmaqshiny`.", call. = FALSE)
  }

  shiny::runApp(Directory, display.mode = "normal")

}
