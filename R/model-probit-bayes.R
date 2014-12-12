#' @include model-zelig.R
#' @include model-probit.R

zprobitbayes <- setRefClass("Zelig-probit-bayes",
                             contains = c("Zelig-bayes",
                                          "Zelig-probit"))

zprobitbayes$methods(
  initialize = function() {
    callSuper()
    .self$name <- "probit-bayes"
    .self$family <- "binomial"
    .self$link <- "probit"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$year <- 2013
    .self$category <- "dichotomous"
    .self$authors <- "Ben Goodrich, Ying Lu"
    .self$description = "Bayesian Probit Regression for Dichotomous Dependent Variables"
    .self$fn <- quote(MCMCpack::MCMCprobit)
    # JSON from parent
  }
)
