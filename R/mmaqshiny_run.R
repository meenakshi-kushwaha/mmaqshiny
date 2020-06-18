
#' @title mmaqshiny: Explore Air Quality Mobile Monitoring Data
#' @description The package is for analysing, visualising and spatial plotting of
#' high-resolution air quality data collected by specific devices installed on
#' a moving platform.
#' @keywords mmaqshiny
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
