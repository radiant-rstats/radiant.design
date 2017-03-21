#' Simple random sampling
#'
#' @details See \url{https://radiant-rstats.github.io/docs/design/sampling.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param var The variable to sample from
#' @param sample_size Number of units to select
#' @param seed Random seed to use as the starting point
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of variables defined in sampling as an object of class sampling
#'
#' @examples
#' result <- sampling("rndnames","Names",10)
#'
#' @seealso \code{\link{summary.sampling}} to summarize results
#' @export
sampling <- function(dataset, var, sample_size,
                     seed = NA,
                     data_filter = "") {

  dat <- getdata(dataset, var, filt = data_filter)
  if (!is_string(dataset)) dataset <- deparse(substitute(dataset)) %>% set_attr("df", TRUE)

  if (is_not(sample_size)) return(add_class("Please select a sample size of 1 or greater", "sampling"))

  ## use seed if provided
  seed %>% gsub("[^0-9]","",.) %>% { if (!is_empty(.)) set.seed(seed) }

	## example list of names obtained from http://listofrandomnames.com
  # dat$rnd_number <- runif(nrow(dat), min = 0, max = 1) %>% round(3)
  dat$rnd_number <- runif(nrow(dat), min = 0, max = 1)
  seldat <- dat %>%
    arrange(desc(rnd_number)) %>%
    .[1:sample_size,, drop = FALSE]

  as.list(environment()) %>% add_class("sampling")
}

#' Summary method for the sampling function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/design/sampling.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{sampling}}
#' @param prn Print full sampling frame. Default is TRUE
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' set.seed(1234)
#' result <- sampling("rndnames", "Names", 10)
#' summary(result)
#'
#' @seealso \code{\link{sampling}} to generate the results
#'
#' @export
summary.sampling <- function(object, prn = TRUE, ...) {

  cat("Sampling (simple random)\n")
  cat("Data       :", object$dataset, "\n")
  if (object$data_filter %>% gsub("\\s","",.) != "")
    cat("Filter     :", gsub("\\n","", object$data_filter), "\n")
  cat("ID variable:", object$var, "\n")
  if (!is.null(object$seed) && !is.na(object$seed))
    cat("Random seed:", object$seed,"\n")
  cat("Sample size:", object$sample_size, "\n\n")
  cat("Selected:\n")
	print(formatdf(object$seldat, dec = 3), row.names = FALSE)
  if (prn) {
    cat("\nSampling frame:\n")
    print(formatdf(object$dat, dec = 3), row.names = FALSE)
  }
}
