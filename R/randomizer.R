#' Randomize cases into experimental conditions
#'
#' @details Wrapper for the complete_ra and block_ra from the randomizr package. See \url{https://radiant-rstats.github.io/docs/design/randomizer.html} for an example in Radiant
#'
#' @param dataset Dataset to sample from
#' @param vars The variables to sample
#' @param conditions Conditions to assign to
#' @param blocks A vector to use for blocking or a data.frame from which to construct a blocking vector
#' @param probs A vector of assignment probabilities for each treatment conditions. By default each condition is assigned with equal probability
#' @param label Name to use for the generated condition variable
#' @param seed Random seed to use as the starting point
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param na.rm Remove rows with missing values (FALSE or TRUE)
#' @param envir Environment to extract data from
#'
#' @return A list of variables defined in randomizer as an object of class randomizer
#'
#' @importFrom randomizr complete_ra block_ra
#' @importFrom dplyr select_at bind_cols
#' @importFrom magrittr set_colnames
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
  label = ".conditions",
  seed = 1234, data_filter = "",
  na.rm = FALSE,
  envir = parent.frame()
) {

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))

  if (!radiant.data::is_empty(blocks)) {
    vars <- c(vars, blocks)
  }

  dataset <- get_data(dataset, vars, filt = data_filter, na.rm = na.rm, envir = envir)

  ## use seed if provided
  seed <- gsub("[^0-9]", "", seed)
  if (!radiant.data::is_empty(seed)) set.seed(seed)

  if (radiant.data::is_empty(probs)) {
    probs <- length(conditions) %>% {rep(1/., .)}
  } else if (length(probs) == 1) {
    probs <- rep(probs, length(conditions))
  } else if (length(probs) != length(conditions)) {
    probs <- NULL
  }

  if (length(blocks) > 0) {
    blocks_vct <- do.call(paste, c(select_at(dataset, .vars = blocks), sep = "-"))
    cond <- randomizr::block_ra(blocks = blocks_vct, conditions = conditions, prob_each = probs) %>%
      as.data.frame() %>%
      set_colnames(label)
  } else {
    cond <- randomizr::complete_ra(N = nrow(dataset), conditions = conditions, prob_each = probs) %>%
      as.data.frame() %>%
      set_colnames(label)
  }

  dataset <- bind_cols(cond, dataset)

  # removing unneeded arguments
  rm(cond, envir)

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
  if (radiant.data::is_empty(object$blocks)) {
    cat("Random assignment (simple random)\n")
  } else {
    cat("Random assignment (blocking)\n")
  }
  cat("Data         :", object$df_name, "\n")
  if (!radiant.data::is_empty(object$data_filter)) {
    cat("Filter       :", gsub("\\n", "", object$data_filter), "\n")
  }
  if (!radiant.data::is_empty(object$blocks)) {
    cat("Variables    :", setdiff(object$vars, object$blocks), "\n")
    cat("Blocks       :", object$blocks, "\n")
  } else {
    cat("Variables    :", object$vars, "\n")
  }
  cat("Conditions   :", object$conditions, "\n")
  cat("Probabilities:", round(object$probs, dec), "\n")
  if (!radiant.data::is_empty(object$seed)) {
    cat("Random seed  :", object$seed, "\n")
  }
  is_unique <- object$dataset[, -1, drop = FALSE] %>%
    {ifelse(nrow(.) > nrow(distinct(.)), "Based on selected variables some duplicate rows exist", "Based on selected variables, no duplicate rows exist")}
  cat("Duplicates   :", is_unique, "\n\n")

  cat("Assigment frequencies:\n")
  if (radiant.data::is_empty(object$blocks_vct)) {
    tab <- table(object$dataset[[object$label]])
  } else {
    tab <- table(object$blocks_vct, object$dataset[[object$label]])
  }
  tab %>% addmargins() %>% print()

  cat("\nAssigment proportions:\n")
  tab %>% prop.table() %>% round(dec) %>% print()
}
