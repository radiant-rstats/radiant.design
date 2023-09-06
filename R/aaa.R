# to avoid 'no visible binding for global variable' NOTE
globalVariables(c(".", "rnd_number"))

#' radiant.design
#'
#' @name radiant.design
#' @import radiant.data shiny
#' @importFrom dplyr %>% arrange arrange_at desc
#' @importFrom magrittr %<>%
#' @importFrom stats as.formula cor na.omit power.prop.test power.t.test qnorm runif coef lm
#' @importFrom import from
NULL

#' 100 random names
#' @details A list of 100 random names.  Description provided in attr(rndnames,"description")
#' @docType data
#' @keywords datasets
#' @name rndnames
#' @usage data(rndnames)
#' @format A data frame with 100 rows and 2 variables
NULL
