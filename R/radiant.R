#' Launch radiant.design in the default browser
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.design()
#' }
#' @export
radiant.design <- function(state, ...) radiant.data::launch(package = "radiant.design", run = "browser", state, ...)

#' Launch radiant.design in an Rstudio window
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.design_window()
#' }
#' @export
radiant.design_window <- function(state, ...) radiant.data::launch(package = "radiant.design", run = "window", state, ...)

#' Launch radiant.design in the Rstudio viewer
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.design_viewer()
#' }
#' @export
radiant.design_viewer <- function(state, ...) radiant.data::launch(package = "radiant.design", run = "viewer", state, ...)
