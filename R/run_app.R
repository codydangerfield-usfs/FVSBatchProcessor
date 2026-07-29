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

  if (exists("register_workflow_assets", mode = "function")) {
    register_workflow_assets()
  }

  # Increase maximum upload size to 10GB for very large database/file transfers
  opt <- options(shiny.maxRequestSize = 10000 * 1024^2)
  on.exit(options(opt), add = TRUE)

  shiny::runApp(
    shiny::shinyApp(ui = ui, server = server),
    launch.browser = launch.browser,
    ...
  )
}
