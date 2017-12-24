#' Launch radiant.design in default browser or Rstudio Viewer
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @param run Run radiant.design in an external browser ("browser") or in the Rstudio viewer ("viewer")
#'
#' @export
radiant.design <- function(run = "browser") {
  if (!"package:radiant.design" %in% search()) {
    if (!sshhr(require(radiant.design))) {
      stop("\nCalling radiant.design start function but radiant.design is not installed.")
    }
  }
  run <- if (run == "viewer") {
    message("\nStarting radiant.design in Rstudio Viewer ...")
    rstudioapi::viewer
  } else {
    message("\nStarting radiant.design in default browser ...\n\nUse radiant.design::radiant.design(\"viewer\") to open radiant.design in Rstudio Viewer")
    TRUE
  }
  suppressPackageStartupMessages(
    shiny::runApp(system.file("app", package = "radiant.design"), launch.browser = run)
  )
}
