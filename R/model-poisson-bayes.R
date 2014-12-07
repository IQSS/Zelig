#' @include model-zelig.R
#' @include model-bayes.R
#' @include model-poisson

zpoissonbayes <- setRefClass("Zelig-poisson-bayes",
                             contains = c("Zelig-bayes",
                                          "Zelig-poisson"))

zpoissonbayes$methods(
  initialize = function() {
    callSuper()
    .self$name <- "poissonbayes"
    .self$family <- "poisson"
    .self$link <- "log"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$year <- 2013
    .self$category <- "continuous"
    .self$authors <- "Ben Goodrich, Ying Lu"
    .self$description = "Bayesian Poisson Regression"
    .self$fn <- quote(MCMCpack::MCMCpoisson)
    # JSON from parent
    .self$wrapper <- "poisson.bayes"
  }
)
