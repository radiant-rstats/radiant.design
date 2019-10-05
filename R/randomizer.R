#' Randomize cases into experimental conditions
#'
#' @details Wrapper for the complete_ra and block_ra from the randomizr package. See \url{https://radiant-rstats.github.io/docs/design/randomizer.html} for an example in Radiant
#'
#' @param dataset Dataset to sample from
#' @param vars The variables to sample
#' @param conditions Conditions to assign to
#' @param blocks A vector to use for blocking or a data.frame from which to construct a blocking vector
#' @param probs A vector of assignment probabilities for each treatment conditions. By default each condition is assigned with equal probability
#' @param seed Random seed to use as the starting point
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#'
#' @return A list of variables defined in randomizer as an object of class randomizer
#' 
#' @importFrom randomizr complete_ra block_ra
#' @importFrom dplyr select_at bind_cols
#'
#' @examples
#' randomizer(rndnames, "Names", conditions = c("test", "control")) %>% str()
#'
#' @seealso \code{\link{summary.sampling}} to summarize results
#' @export
randomizer <- function(
  dataset, vars,
  conditions = c("A", "B"), 
  blocks = NULL, probs = NULL,
  seed = 1234, data_filter = "",
  envir = parent.frame()
) {

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))

  if (!is_empty(blocks)) {
    vars <- c(vars, blocks)
  }

  dataset <- get_data(dataset, vars, filt = data_filter, envir = envir)

  ## use seed if provided
  seed <- gsub("[^0-9]", "", seed)
  if (!is_empty(seed)) set.seed(seed)

  if (is_empty(probs)) {
    probs <- length(conditions) %>% {rep(1/., .)}
  } 
  if (length(probs) != length(conditions)) {
    probs <- NULL
  }

  if (length(blocks) > 0) {
    blocks_vct <- do.call(paste, c(select_at(dataset, .vars = blocks), sep = "-"))
    .conditions <- data.frame(.conditions = randomizr::block_ra(blocks = blocks_vct, conditions = conditions, prob_each = probs))
  } else {
    .conditions <- data.frame(.conditions = randomizr::complete_ra(N = nrow(dataset), conditions = conditions, prob_each = probs))
  }

  dataset <- bind_cols(.conditions, dataset)

  # removing unneeded arguments
  rm(envir)

  as.list(environment()) %>% add_class("randomizer")
}

#' Summary method for the randomizer function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/design/randomizer.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{randomizer}}
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @importFrom stats addmargins
#' @importFrom dplyr distinct
#' 
#' @examples
#' randomizer(rndnames, "Names", conditions = c("test", "control")) %>% summary()
#'
#' @seealso \code{\link{randomizer}} to generate the results
#'
#' @export
summary.randomizer <- function(object, dec = 3, ...) {
  if (is_empty(object$blocks)) {
    cat("Random assignment (simple random)\n")
  } else {
    cat("Random assignment (blocking)\n")
  }
  cat("Data         :", object$df_name, "\n")
  if (!is_empty(object$data_filter)) {
    cat("Filter       :", gsub("\\n", "", object$data_filter), "\n")
  }
  if (!is_empty(object$blocks)) {
    cat("Variables    :", setdiff(object$vars, object$blocks), "\n")
    cat("Blocks       :", object$blocks, "\n")
  } else {
    cat("Variables    :", object$vars, "\n")
  }
  cat("Conditions   :", object$conditions, "\n")
  cat("Probabilities:", round(object$probs, dec), "\n")
  if (!is_empty(object$seed)) {
    cat("Random seed  :", object$seed, "\n")
  }
  is_unique <- object$dataset[, -1, drop = FALSE] %>%
    {ifelse(nrow(.) > nrow(distinct(.)), "Based on selected variables some duplicate rows exist", "Based on selected variables, no duplicate rows exist")}
  cat("Duplicates   :", is_unique, "\n\n")

  cat("Assigment frequencies:\n")
  if (is_empty(object$blocks_vct)) {
    tab <- table(object$dataset$.conditions)
  } else {
    tab <- table(object$blocks_vct, object$dataset$.conditions)
  }
  tab %>% addmargins() %>% print()

  cat("\nAssigment proportions:\n")
  tab %>% prop.table() %>% round(dec) %>% print()
}
