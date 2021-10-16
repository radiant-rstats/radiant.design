#' Sample size calculation for comparisons
#'
#' @details See \url{https://radiant-rstats.github.io/docs/design/sample_size_comp.html} for an example in Radiant
#'
#' @param type Choose "mean" or "proportion"
#' @param n1 Sample size for group 1
#' @param n2 Sample size for group 2
#' @param p1 Proportion 1 (only used when "proportion" is selected)
#' @param p2 Proportion 2 (only used when "proportion" is selected)
#' @param delta Difference in means between two groups (only used when "mean" is selected)
#' @param sd Standard deviation (only used when "mean" is selected)
#' @param conf_lev Confidence level
#' @param power Power
#' @param ratio Sampling ratio (n1 / n2)
#' @param alternative Two or one sided test
#'
#' @return A list of variables defined in sample_size_comp as an object of class sample_size_comp
#'
#' @seealso \code{\link{summary.sample_size_comp}} to summarize results
#'
#' @examples
#' sample_size_comp(
#'   type = "proportion", p1 = 0.1, p2 = 0.15,
#'   conf_lev = 0.95, power = 0.8
#' )
#'
#' @importFrom pwr pwr.2p.test pwr.2p2n.test ES.h pwr.t.test pwr.t2n.test
#'
#' @export
sample_size_comp <- function(
 type, n1 = NULL, n2 = NULL, p1 = NULL, p2 = NULL, delta = NULL,
 sd = NULL, conf_lev = NULL, power = NULL, ratio = 1,
 alternative = "two.sided"
) {

  if (!is.null(n1) && is.na(n1)) n1 <- NULL
  if (!is.null(n2) && is.na(n2)) n2 <- NULL
  if (!is.null(power) && is.na(power)) power <- NULL
  if (!is.null(conf_lev) && is.na(conf_lev)) conf_lev <- NULL
  sig.level <- if (radiant.data::is_empty(conf_lev)) NULL else 1 - conf_lev
  adj <- ifelse(alternative == "two.sided", 2, 1)

  if (type == "mean") {
    if (!is.null(delta) && is.na(delta)) delta <- NULL
    if (!is.null(delta)) delta <- abs(delta)
    if (!is.null(sd) && is.na(sd)) sd <- NULL

    if (!radiant.data::is_empty(sd) && sd <= 0) {
      return("The standard deviation must be larger than 0" %>% add_class("sample_size_comp"))
    }

    nr_null <- any(is.null(n2), is.null(n1)) + is.null(delta) + is.null(sd) + is.null(power) + is.null(conf_lev)
    if (nr_null == 0 || nr_null > 1) {
      return("Exactly one of 'Sample size', 'Delta', 'Std. deviation',\n'Confidence level', and 'Power' must be blank or NULL" %>% add_class("sample_size_comp"))
    }

    if (is.null(power) || is.null(sig.level)) {
      res <- try(pwr::pwr.t2n.test(n1 = as.numeric(n1), n2 = as.numeric(n2), d = delta / sd, sig.level = sig.level, power = power, alternative = alternative), silent = TRUE)
    } else if (is.null(n1) && is.null(n2)) {
      res <- try(pwr::pwr.t.test(d = delta / sd, sig.level = sig.level, power = power, alternative = alternative), silent = TRUE)
      if (!inherits(res, "try-error"))  n1 <- n2 <- res$n
    } else if (is.null(n1)) {
      res <- try(pwr::pwr.t2n.test(n2 = as.numeric(n2), d = delta / sd, sig.level = sig.level, power = power, alternative = alternative), silent = TRUE)
      if (!inherits(res, "try-error")) n1 <- res$n1
    } else if (is.null(n2)) {
      res <- try(pwr::pwr.t2n.test(n1 = as.numeric(n1), d = delta / sd, sig.level = sig.level, power = power, alternative = alternative), silent = TRUE)
      if (!inherits(res, "try-error")) n2 <- res$n2
    } else {
      res <- try(pwr::pwr.t2n.test(n1 = as.numeric(n1), n2 = as.numeric(n2), sig.level = sig.level, power = power, alternative = alternative), silent = TRUE)
      if (!inherits(res, "try-error")) {
        if (is.null(delta)) {
          delta <- res$d * sd
        } else {
          sd <- abs(delta / res$d)
        }
      }
    }
  } else {
    if (!is.null(p1) && is.na(p1)) p1 <- NULL
    if (!is.null(p2) && is.na(p2)) p2 <- NULL

    if (!is.null(p1) && !is.null(p2)) {
      if (p1 == p2) {
        return("Proportion 1 and 2 should not be set equal. Please change the proportion values" %>% add_class("sample_size_comp"))
      } else if (p1 > p2 && alternative == "less") {
        return("Proportion 1 must be smaller than proportion 2 if the alternative\n hypothesis is 'p1 less than p2'" %>% add_class("sample_size_comp"))
      } else if (p1 < p2 && alternative == "greater") {
        return("Proportion 1 must be larger than proportion 2 if the alternative\nhypothesis is 'p1 greater than p2'" %>% add_class("sample_size_comp"))
      }
    }

    if (!radiant.data::is_empty(p1) && (p1 < 0 || p1 > 1)) {
      return("Proportion 1 must be between 0 and 1" %>% add_class("sample_size_comp"))
    }
    if (!radiant.data::is_empty(p2) && (p2 < 0 || p2 > 1)) {
      return("Proportion 2 must be between 0 and 1" %>% add_class("sample_size_comp"))
    }

    nr_null <- any(is.null(n2), is.null(n1)) + is.null(power) + is.null(p1) + is.null(p2) + is.null(conf_lev)
    if (nr_null == 0 || nr_null > 1) {
      return("Exactly one of 'Sample size', 'Proportion 1', 'Proportion 2',\n'Confidence level', and 'Power' must be blank or NULL" %>% add_class("sample_size_comp"))
    }

    backout.ES.h <- function(h, p) {
      sort(
        c(
          sin((h - 2*asin(sqrt(p)))/2)^2,
          sin((-h - 2*asin(sqrt(p)))/2)^2
        ),
        decreasing = TRUE
      )
    }

    if (is.null(power) || is.null(sig.level)) {
      res <- try(pwr::pwr.2p2n.test(n1 = as.numeric(n1), n2 = as.numeric(n2), h = pwr::ES.h(p1 = p1, p2 = p2), sig.level = sig.level, power = power, alternative = alternative), silent = TRUE)
    } else if (is.null(n1) && is.null(n2)) {
      res <- try(pwr::pwr.2p.test(h = pwr::ES.h(p1 = p1, p2 = p2), sig.level = sig.level, power = power, alternative = alternative), silent = TRUE)
      if (!inherits(res, "try-error"))  n1 <- n2 <- res$n
    } else if (is.null(n1)) {
      res <- try(pwr::pwr.2p2n.test(n2 = as.numeric(n2), h = pwr::ES.h(p1 = p1, p2 = p2), sig.level = sig.level, power = power, alternative = alternative), silent = TRUE)
      if (!inherits(res, "try-error"))  n1 <- res$n1
    } else if (is.null(n2)) {
      res <- try(pwr::pwr.2p2n.test(n1 = as.numeric(n1), h = pwr::ES.h(p1 = p1, p2 = p2), sig.level = sig.level, power = power, alternative = alternative), silent = TRUE)
      if (!inherits(res, "try-error"))  n2 <- res$n2
    } else {
      res <- try(pwr::pwr.2p2n.test(n1 = as.numeric(n1), n2 = as.numeric(n2), sig.level = sig.level, power = power, alternative = alternative), silent = TRUE)
      if (!inherits(res, "try-error")) {
        if (is.null(p1)) {
          p1 <- backout.ES.h(res$h, p2)
          if (alternative != "two.sided") {
            p1 <- ifelse(alternative == "less", p1[2], p1[1])
          }
        } else {
          p2 <- backout.ES.h(res$h, p1)
          if (alternative != "two.sided") {
            p2 <- ifelse(alternative == "less", p2[1], p2[2])
          }
        }
      }
    }
  }

  as.list(environment()) %>% add_class("sample_size_comp")
}

#' Summary method for the sample_size_comp function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/design/sample_size_comp.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{sample_size_comp}}
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{sample_size_comp}} to generate the results
#'
#' @examples
#' sample_size_comp(
#'   type = "proportion", p1 = 0.1, p2 = 0.15,
#'   conf_lev = 0.95, power = 0.8
#' ) %>% summary()
#'
#' @importFrom pwr ES.h
#'
#' @export
summary.sample_size_comp <- function(object, ...) {
  if (is.character(object)) return(object)
  if (inherits(object$res, "try-error")) return("Provided input does not generate valid results. Update input values ...")

  cat("Sample size calculation for comparison of",  ifelse(object$type == "proportion", "proportions", "means"), "\n")
  cat(paste0("Sample size 1    : ", format_nr(ceiling(object$n1), dec = 0), "\n"))
  cat(paste0("Sample size 2    : ", format_nr(ceiling(object$n2), dec = 0), "\n"))
  cat(paste0("Total sample size: ", format_nr(ceiling(object$n1) + ceiling(object$n2), dec = 0), "\n"))

  if (object$type == "mean") {
    cat("Delta            :", object$delta, "\n")
    cat("Std. deviation   :", object$sd, "\n")
    cat("Effect size      :", object$delta / object$sd, "\n")
  } else {
    cat("Proportion 1     :", object$p1, "\n")
    cat("Proportion 2     :", object$p2, "\n")
    cat("Effect size      :", pwr::ES.h(p1 = object$p1[1], p2 = object$p2[1]) %>% abs(), "\n")
  }
  cat("Confidence level :", 1 - object$res$sig.level, "\n")
  cat("Power            :", object$res$power, "\n")
  cat("Alternative      :", object$alternative, "\n\n")
}

#' Plot method for the sample_size_comp function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/design/sample_size_comp.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{sample_size_comp}}
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{sample_size_comp}} to generate the results
#'
#' @examples
#' sample_size_comp(
#'   type = "proportion", p1 = 0.1, p2 = 0.15,
#'   conf_lev = 0.95, power = 0.8
#' ) %>% plot()
#'
#' @importFrom pwr plot.power.htest
#'
#' @export
plot.sample_size_comp <- function(x, ...) {
  if (is.character(x) || inherits(x$res, "try-error")) return("  ")
  pwr::plot.power.htest(x$res)
}
