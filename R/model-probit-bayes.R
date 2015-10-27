#' Bayesian Probit Regression
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-probitbayes.html}
#' @import methods
#' @export Zelig-probit-bayes
#' @exportClass Zelig-probit-bayes
#'
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
    .self$wrapper <- "probit.bayes"
  }
)

zprobitbayes$methods(
  mcfun = function(x, b0=0, b1=1, ..., sim=TRUE){
    mu <- pnorm(b0 + b1 * x)
    if(sim){
        y <- rbinom(n=length(x), size=1, prob=mu)
        return(y)
    }else{
        return(mu)
    }
  }
)