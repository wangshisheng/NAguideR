#' Run the NAguideR Shiny web application locally.
#' @export
NAguideR_app <- function() {
  shiny::runApp(system.file('NAguideRapp', package='NAguideR'),
                host=getOption("0.0.0.0"), port =getOption("8989"))
}
