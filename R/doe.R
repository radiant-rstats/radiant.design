#' Create (partial) factorial design
#'
#' @details See \url{https://radiant-rstats.github.io/docs/design/doe.html} for an example in Radiant
#'
#' @param factors Categorical variables used as input for design
#' @param int Vector of interaction terms to consider when generating design
#' @param trials Number of trials to create. If NA then all feasible designs will be considered until a design with perfect D-efficiency is found
#' @param seed Random seed to use as the starting point
#'
#' @return A list with all variables defined in the function as an object of class doe
#'
#' @examples
#' doe(c("price; $10; $13; $16", "food; popcorn; gourmet; no food"))
#' doe(
#'   c("price; $10; $13; $16", "food; popcorn; gourmet; no food"),
#'   int = "price:food", trials = 9, seed = 1234
#' )
#'
#' @seealso \code{\link{summary.doe}} to summarize results
#'
#' @importFrom AlgDesign optFederov
#' @importFrom mvtnorm pmvnorm
#' @importFrom polycor hetcor
#' @importFrom dplyr right_join
#'
#' @export
doe <- function(factors, int = "", trials = NA, seed = NA) {
  df_list <- gsub("[ ]{2,}", " ", paste0(factors, collapse = "\n")) %>%
    gsub("/", "", .) %>%
    gsub("\\\\n", "\n", .) %>%
    gsub("[ ]*;[ ]*", ";", .) %>%
    gsub(";{2,}", ";", .) %>%
    gsub("[;]+[ ]{0,}\n", "\n", .) %>%
    gsub("[ ]{1,}\n", "\n", .) %>%
    gsub("\n[ ]+", "\n", .) %>%
    gsub("[\n]{2,}", "\n", .) %>%
    gsub("[ ]+", "_", .) %>%
    strsplit(., "\n") %>%
    .[[1]] %>%
    strsplit(";\\s*")

  df_names <- c()
  if (length(df_list) < 2) {
    return("DOE requires at least two factors" %>% add_class("doe"))
  }

  for (i in seq_len(length(df_list))) {
    dt <- df_list[[i]] %>% gsub("^\\s+|\\s+$", "", .)
    df_names <- c(df_names, dt[1])
    df_list[[i]] <- dt[-1]
  }
  names(df_list) <- df_names
  model <- paste0("~ ", paste0(df_names, collapse = " + "))
  nInt <- 0
  if (!is.empty(int)) {
    model <- paste0(model, " + ", paste0(int, collapse = " + "))
    nInt <- length(int)
  }

  part_fac <- function(df, model = ~., int = 0, trials = NA, seed = 172110) {
    full <- expand.grid(df)

    ###############################################
    # eliminate combinations from full
    # by removing then from the variable _experiment_
    # http://stackoverflow.com/questions/18459311/creating-a-fractional-factorial-design-in-r-without-prohibited-pairs?rq=1
    ###############################################

    levs <- sapply(df, length)
    nr_levels <- sum(levs)
    min_trials <- nr_levels - length(df) + 1
    max_trials <- nrow(full)

    ## make sure the number of trials set by the user is within an appropriate range
    if (!is.empty(trials)) {
      max_trials <- min_trials <- max(min(trials, max_trials), min_trials)
    }

    ## define a data.frame that will store design spec
    eff <- data.frame(
      Trials = min_trials:max_trials,
      "D-efficiency" = NA,
      "Balanced" = NA,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    for (i in min_trials:max_trials) {
      seed %>%
        gsub("[^0-9]", "", .) %>%
        (function(x) if (!is.empty(x)) set.seed(seed))
      design <- try(AlgDesign::optFederov(
        model,
        data = full, nRepeats = 50,
        nTrials = i, maxIteration = 1000,
        approximate = FALSE
      ), silent = TRUE)

      if (inherits(design, "try-error")) next
      ind <- which(eff$Trials %in% i)
      eff[ind, "D-efficiency"] <- design$Dea
      eff[ind, "Balanced"] <- all(i %% levs == 0)

      if (design$Dea == 1) break
    }

    if (!inherits(design, "try-error")) {
      cor_mat <- sshhr(polycor::hetcor(design$design, std.err = FALSE)$correlations)
    }

    if (exists("cor_mat")) {
      detcm <- det(cor_mat)

      full <- arrange_at(full, .vars = names(df)) %>%
        data.frame(trial = 1:nrow(full), ., stringsAsFactors = FALSE)

      part <- arrange_at(design$design, .vars = names(df)) %>%
        (function(x) suppressMessages(dplyr::right_join(full, x)))

      list(
        df = df,
        cor_mat = cor_mat,
        detcm = detcm,
        Dea = design$Dea,
        part = part,
        full = full,
        eff = na.omit(eff),
        seed = seed
      )
    } else if (!is.na(trials)) {
      "No solution exists for the selected number of trials"
    } else {
      "No solution found"
    }
  }

  part_fac(df_list, model = as.formula(model), int = nInt, trials = trials, seed = seed) %>%
    add_class("doe")
}

#' Summary method for doe function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/design/doe.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{doe}}
#' @param eff If TRUE print efficiency output
#' @param part If TRUE print partial factorial
#' @param full If TRUE print full factorial
#' @param est If TRUE print number of effects that will be estimable using the partial factorial design
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods.
#'
#' @seealso \code{\link{doe}} to calculate results
#'
#' @examples
#' c("price; $10; $13; $16", "food; popcorn; gourmet; no food") %>%
#'   doe() %>%
#'   summary()
#'
#' @export
summary.doe <- function(object, eff = TRUE, part = TRUE, full = TRUE, est = TRUE, dec = 3, ...) {
  if (!is.list(object)) {
    return(object)
  }

  cat("Experimental design\n")
  cat("# trials for partial factorial:", nrow(object$part), "\n")
  cat("# trials for full factorial   :", nrow(object$full), "\n")
  if (!is.empty(object$seed)) {
    cat("Random seed                   :", object$seed, "\n")
  }

  cat("\nAttributes and levels:\n")
  nl <- names(object$df)
  for (i in nl) {
    cat(paste0(i, ":"), paste0(object$df[[i]], collapse = ", "), "\n")
  }

  if (eff) {
    cat("\nDesign efficiency:\n")
    format_df(object$eff, dec = dec) %>%
      print(row.names = FALSE)

    cat("\nPartial factorial design correlations:\n")
    cat("** Note: Variables are assumed to be ordinal **\n")
    round(object$cor_mat, ifelse(object$detcm == 1, 0, dec)) %>%
      print(row.names = FALSE)
  }

  if (part) {
    cat("\nPartial factorial design:\n")
    print(object$part, row.names = FALSE)
  }

  if (est) {
    cat("\nEstimable effects from partial factorial design:\n\n")
    cat(paste(" ", estimable(object), collapse = "\n"), "\n")
  }

  if (full) {
    cat("\nFull factorial design:\n")
    print(object$full, row.names = FALSE)
  }
}

#' Determine coefficients that can be estimated based on a partial factorial design
#'
#' @description A function to determine which coefficients can be estimated based on a partial factorial design. Adapted from a function written by Blakeley McShane at https://github.com/fzettelmeyer/mktg482/blob/master/R/expdesign.R
#'
#' @param design An experimental design generated by the doe function that includes a partial and full factorial design
#' @examples
#' design <- doe(c("price; $10; $13; $16", "food; popcorn; gourmet; no food"), trials = 6)
#' estimable(design)
#'
#' @export
estimable <- function(design) {
  if (!inherits(design, "doe")) {
    return(add_class("The estimable function requires input of type 'doe'. Please use the ratiant.design::doe function to generate an appropriate design", "doe"))
  }

  full <- design$full
  fm <- as.formula(paste("trial ~ ", paste(colnames(full)[-1], collapse = "*")))
  mod1 <- lm(fm, data = full)
  coef1 <- coef(mod1)

  mod2 <- lm(fm, data = design$part)
  coef2 <- coef(mod2)
  coef2 <- coef2[!is.na(coef2)]

  ## format levels
  hasLevs <- sapply(full[, -1, drop = FALSE], function(x) is.factor(x) || is.logical(x) || is.character(x))
  if (sum(hasLevs) > 0) {
    for (i in names(hasLevs[hasLevs])) {
      names(coef2) %<>% gsub(paste0("^", i), paste0(i, "|"), .) %>%
        gsub(paste0(":", i), paste0(":", i, "|"), .)
    }
  }

  names(coef2[-1])
}
