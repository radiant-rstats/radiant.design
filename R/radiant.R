#' Launch Radiant in the default browser
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @export
radiant.design <- function() {
  if (!"package:radiant.design" %in% search())
    if (!require(radiant.design)) stop("Calling radiant.design start function but radiant.design is not installed.")
  runApp(system.file("app", package = "radiant.design"), launch.browser = TRUE)
}
