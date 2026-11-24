#' Launch the FVS Batch Processor app
#'
#' Open the Shiny interface for configuring FVS scenarios, generating stand
#' keyfiles, running simulations through rFVS, and consolidating SQLite outputs.
#'
#' @param launch.browser Logical; whether to open the app in a browser. Passed
#'   to [shiny::runApp()]. Defaults to `TRUE`.
#' @param ... Additional arguments passed to [shiny::runApp()], such as `port`
#'   and `host`. Do not supply `appDir`, which is provided internally.
#'
#' @details
#' The working directory at launch supplies the initial project root in the
#' interface. Set it to your project directory before calling this function,
#' or change Root Folder Path in the app. The inventory database and KCP catalog
#' must be accessible to the R process. Simulation also requires separately
#' installed FVS variant binaries; select their directory on Tab 4.
#'
#' The launcher temporarily raises Shiny's upload limit to `10000 * 1024^2`
#' bytes and restores the previous option when the app exits.
#'
#' @return This function is called for its side effect of running the app.
#'   Like [shiny::runApp()], it normally does not return until the app stops.
#' @examples
#' if (interactive()) {
#'   fvsRunBatch()
#' }
#' @export
fvsRunBatch <- function(launch.browser = TRUE, ...) {
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

  # Forward the user's current working directory into the Shiny app options
  shinyOptions(FVS_USER_WD = getwd())

  # Increase maximum upload size to 10GB for very large database/file transfers
  opt <- options(shiny.maxRequestSize = 10000 * 1024^2)
  on.exit(options(opt), add = TRUE)

  shiny::runApp(
    shiny::shinyApp(ui = ui, server = server),
    launch.browser = launch.browser,
    ...
  )
}
