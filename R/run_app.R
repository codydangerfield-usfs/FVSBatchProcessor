#' Launch the FVS Batch Processor app
#'
#' @param launch.browser Logical; passed to shiny::runApp.
#' @param ... Additional arguments passed to shiny::runApp.
#' @export
run_app <- function(launch.browser = TRUE, ...) {
  if (!requireNamespace("rFVS", quietly = TRUE)) {
    stop(
      "Package 'rFVS' is required. Install it with:\n",
      "remotes::install_github('USDAForestService/ForestVegetationSimulator-Interface', subdir = 'rFVS')",
      call. = FALSE
    )
  }

  shiny::runApp(
    shiny::shinyApp(ui = ui, server = server),
    launch.browser = launch.browser,
    ...
  )
}
