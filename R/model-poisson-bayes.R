#' Bayesian Poisson Regression
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-poissonbayes.html}
#' @import methods
#' @export Zelig-poisson-bayes
#' @exportClass Zelig-poisson-bayes
#'
#' @include model-zelig.R
#' @include model-bayes.R
#' @include model-poisson.R

zpoissonbayes <- setRefClass("Zelig-poisson-bayes",
                             contains = c("Zelig-bayes",
                                          "Zelig-poisson"))

zpoissonbayes$methods(
  initialize = function() {
    callSuper()
    .self$name <- "poisson-bayes"
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


zpoissonbayes$methods(
  mcfun = function(x, b0=0, b1=1, ..., sim=TRUE){
    lambda <- exp(b0 + b1 * x)
    if(sim){
        y <- rpois(n=length(x), lambda=lambda)
        return(y)
    }else{
        return(lambda)
    }
  }
)
