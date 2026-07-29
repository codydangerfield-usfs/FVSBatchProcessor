#' Launch the FVS Batch Processor app
#'
#' @param launch.browser Logical; passed to shiny::runApp.
#' @param ... Additional arguments passed to shiny::runApp.
#' @export
run_app <- function(launch.browser = TRUE, ...) {
  options(shiny.maxRequestSize = 10000 * 1024^2)

  if (!requireNamespace("rFVS", quietly = TRUE)) {
    stop(
      "Package 'rFVS' is required. Install it with:\n",
      "remotes::install_github('USDAForestService/ForestVegetationSimulator-Interface', subdir = 'rFVS')",
      call. = FALSE
    )
  }

  if (exists("register_workflow_assets", mode = "function")) {
    register_workflow_assets()
  }

  shiny::runApp(
    shiny::shinyApp(ui = ui, server = server),
    launch.browser = launch.browser,
    ...
  )
}
