#' Launch Radiant in the default browser
#'
#' @details See \url{http://vnijs.github.io/radiant} for documentation and tutorials
#'
#' @export
radiant.design <- function() {
  if (!"package:radiant.design" %in% search())
    if (!require(radiant.design)) stop("Calling radiant.design start function but radiant.design is not installed.")
  runApp(system.file("app", package = "radiant.design"), launch.browser = TRUE)
}
