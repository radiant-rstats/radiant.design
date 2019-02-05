#' Simple random sampling
#'
#' @details See \url{https://radiant-rstats.github.io/docs/design/sampling.html} for an example in Radiant
#'
#' @param dataset Dataset to sample from
#' @param var The variable to sample
#' @param sample_size Number of units to select
#' @param seed Random seed to use as the starting point
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of variables defined in sampling as an object of class sampling
#'
#' @examples
#' sampling(rndnames, "Names", 10)
#'
#' @seealso \code{\link{summary.sampling}} to summarize results
#' @export
sampling <- function(
  dataset, var, sample_size,
  seed = NA, data_filter = ""
) {

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, var, filt = data_filter)
  if (is_not(sample_size)) return(add_class("Please select a sample size of 1 or greater", "sampling"))

  ## use seed if provided
  seed %>% gsub("[^0-9]", "", .) %>%
    {if (!is_empty(.)) set.seed(seed)}

  dataset$rnd_number <- runif(nrow(dataset), min = 0, max = 1)
  seldat <- arrange(dataset, desc(rnd_number)) %>%
    .[seq_len(max(1, sample_size)), , drop = FALSE]

  as.list(environment()) %>% add_class("sampling")
}

#' Summary method for the sampling function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/design/sampling.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{sampling}}
#' @param prn Print full sampling frame. Default is FALSE
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' set.seed(1234)
#' sampling(rndnames, "Names", 10) %>% summary()
#'
#' @seealso \code{\link{sampling}} to generate the results
#'
#' @export
summary.sampling <- function(object, prn = FALSE, dec = 3, ...) {
  cat("Sampling (simple random)\n")
  cat("Data       :", object$df_name, "\n")
  if (!is_empty(object$data_filter)) {
    cat("Filter     :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("ID variable:", object$var, "\n")
  if (!is.null(object$seed) && !is.na(object$seed)) {
    cat("Random seed:", object$seed, "\n")
  }
  cat("Sample size:", object$sample_size, "\n\n")
  cat("Selected:\n")
  as.data.frame(object$seldat, stringsAsFactors = FALSE) %>%
      format_df(dec = dec) %>%
      print(row.names = FALSE)
  if (prn) {
    cat("\nSampling frame:\n")
    as.data.frame(object$dataset, stringsAsFactors = FALSE) %>%
      format_df(dec = dec) %>%
      print(row.names = FALSE)
  }
}
