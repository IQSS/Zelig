#' Bayesian Logit Regression
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-logitbayes.html}
#' @import methods
#' @export Zelig-logit-bayes
#' @exportClass Zelig-logit-bayes
#' 
#' @include model-zelig.R
#' @include model-bayes.R
#' @include model-logit.R

zlogitbayes <- setRefClass("Zelig-logit-bayes",
                             contains = c("Zelig-bayes",
                                          "Zelig-logit"))

zlogitbayes$methods(
  initialize = function() {
    callSuper()
    .self$name <- "logit-bayes"
    .self$family <- "binomial"
    .self$link <- "logit"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$year <- 2013
    .self$category <- "dichotomous"
    .self$authors <- "Ben Goodrich, Ying Lu"
    .self$description = "Bayesian Logistic Regression for Dichotomous Dependent Variables"
    .self$fn <- quote(MCMCpack::MCMClogit)
    # JSON from parent
    .self$wrapper <- "logit.bayes"
  }
)

zlogitbayes$methods(
  mcfun = function(x, b0=0, b1=1, ..., sim=TRUE){
    mu <- 1/(1 + exp(-b0 - b1 * x))
    if(sim){
        y <- rbinom(n=length(x), size=1, prob=mu)
        return(y)
    }else{
        return(mu)
    }
  }
)
