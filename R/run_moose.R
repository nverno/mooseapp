##' Run the moose app.
##' @title Run the interactive Moosilauke app.
##' @importFrom shiny runApp
##' @export
run_moose <- function() {
  shiny::runApp(system.file('inst/app', package='mooseapp'))
}
