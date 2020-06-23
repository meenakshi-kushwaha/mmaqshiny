
#' @title mmaqshiny: Explore Air-Quality Mobile-Monitoring Data
#' @description Mobile-monitoring or “sensors on a mobile platform”, is an increasingly
#' popular approach to measure high-resolution pollution data at the street level.
#' Coupled with location data, spatial visualisation of air-quality parameters
#' helps detect localized areas of high air-pollution, also called hotspots.
#' In this approach, portable sensors are mounted on a vehicle and driven on
#' predetermined routes to collect high frequency data (1 Hz). The package is
#' for analysing, visualising and spatial maps of high-resolution air-quality
#' data collected by specific devices installed on a moving platform.
#' @keywords mmaqshiny
#' @examples
#' \dontrun{
#' library(mmaqshiny)
#' mmaqshiny::mmaqshiny_run()
#' }
#'
#' @export
mmaqshiny_run <- function() {
  Directory <- system.file("shiny", package = "mmaqshiny")
  if (Directory == "") {
    stop("Try reinstalling the package `mmaqshiny`.", call. = FALSE)
  }

  shiny::runApp(Directory, display.mode = "normal")

}
