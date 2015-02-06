#' @include model-zelig.R
#' @include model-probit.R

ztobitbayes <- setRefClass("Zelig-tobit-bayes",
                           contains = c("Zelig-bayes",
                                        "Zelig-tobit"))

ztobitbayes$methods(
  initialize = function() {
    callSuper()
    .self$name <- "tobit-bayes"
    .self$year <- 2013
    .self$category <- "dichotomous"
    .self$authors <- "Ben Goodrich, Ying Lu"
    .self$description = "Bayesian Tobit Regression for a Censored Dependent Variable"
    .self$fn <- quote(MCMCpack::MCMCtobit)
    # JSON from parent
    .self$wrapper <- "tobit.bayes"
  }
)

ztobitbayes$methods(
  param = function(z.out) {
    if (length(.self$below) == 0)
      .self$below <- 0
    if (length(.self$above) == 0)
      .self$above <- Inf
    simparam.local <- list()
    simparam.local$simparam <- z.out[, 1:(ncol(z.out) - 1)]
    simparam.local$simalpha <- sqrt(z.out[, ncol(z.out)])
    return(simparam.local)
  }
)
