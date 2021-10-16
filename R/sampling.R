#' Simple random sampling
#'
#' @details See \url{https://radiant-rstats.github.io/docs/design/sampling.html} for an example in Radiant
#'
#' @param dataset Dataset to sample from
#' @param vars The variables to sample
#' @param sample_size Number of units to select
#' @param seed Random seed to use as the starting point
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param na.rm Remove rows with missing values (FALSE or TRUE)
#' @param envir Environment to extract data from
#'
#' @return A list  of class 'sampling' with all variables defined in the sampling function
#'
#' @examples
#' sampling(rndnames, "Names", 10)
#'
#' @seealso \code{\link{summary.sampling}} to summarize results
#' @export
sampling <- function(
  dataset, vars, sample_size,
  seed = 1234, data_filter = "",
  na.rm = FALSE,  envir = parent.frame()
) {

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter, na.rm = na.rm, envir = envir)
  if (is_not(sample_size)) return(add_class("Please select a sample size of 1 or greater", "sampling"))

  ## use seed if provided
  seed %>% gsub("[^0-9]", "", .) %>%
    {if (!radiant.data::is_empty(.)) set.seed(.)}

  rnd_number <- data.frame(rnd_number = runif(nrow(dataset), min = 0, max = 1))
  dataset <- bind_cols(rnd_number, dataset)
  seldat <- arrange(dataset, desc(rnd_number)) %>%
    .[seq_len(max(1, sample_size)), , drop = FALSE]

  # removing unneeded arguments
  rm(envir)

  as.list(environment()) %>% add_class("sampling")
}

#' Summary method for the sampling function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/design/sampling.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{sampling}}
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @importFrom dplyr distinct
#'
#' @examples
#' sampling(rndnames, "Names", 10) %>% summary()
#'
#' @seealso \code{\link{sampling}} to generate the results
#'
#' @export
summary.sampling <- function(object, dec = 3, ...) {
  cat("Sampling (simple random)\n")
  cat("Data       :", object$df_name, "\n")
  if (!radiant.data::is_empty(object$data_filter)) {
    cat("Filter     :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Variables  :", object$var, "\n")
  if (!radiant.data::is_empty(object$seed)) {
    cat("Random seed:", object$seed, "\n")
  }
  if (radiant.data::is_empty(object$sample_size) || object$sample_size < 1) {
    cat("Sample size: 1 (invalid input provided)\n")
  } else {
    cat("Sample size:", object$sample_size, "\n")
  }

  is_unique <- object$dataset[, -1, drop = FALSE] %>%
    {ifelse(nrow(.) > nrow(distinct(.)), "Based on selected variables some duplicate rows exist", "Based on selected variables, no duplicate rows exist")}
  cat("Duplicates :", is_unique, "\n\n")
}
