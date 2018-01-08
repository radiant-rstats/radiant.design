#' Launch radiant.design in default browser
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.design()
#' }
#' @export
radiant.design <- function() radiant.data::launch(package = "radiant.design", run = "browser")

#' Launch radiant.design in the Rstudio viewer
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.design_viewer()
#' }
#' @export
radiant.design_viewer <- function() radiant.data::launch(package = "radiant.design", run = "viewer")
