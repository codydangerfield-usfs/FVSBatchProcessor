#' Launch the FVS Batch Processor app
#'
#' @param launch.browser Logical; passed to shiny::runApp.
#' @param ... Additional arguments passed to shiny::runApp.
#' @export
run_app <- function(launch.browser = getOption("shiny.launch.browser", interactive()), ...) {
  if (!requireNamespace("rFVS", quietly = TRUE)) {
    stop(
      "Package 'rFVS' is required. Install it with:\n",
      "remotes::install_github('USDAForestService/ForestVegetationSimulator-Interface', subdir = 'rFVS')",
      call. = FALSE
    )
  }

  app_dir <- system.file("app", package = "FVSBatchProcessor")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `FVSBatchProcessor`.", call. = FALSE)
  }

  # Forward the user's current working directory into the Shiny app options
  shinyOptions(FVS_USER_WD = getwd())

  # Increase maximum upload size to 10GB for very large database/file transfers
  opt <- options(shiny.maxRequestSize = 10000 * 1024^2)
  on.exit(options(opt), add = TRUE)

  shiny::runApp(
    app_dir,
    launch.browser = launch.browser,
    ...
  )
}
