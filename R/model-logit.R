#' Logistic Regression for Dichotomous Dependent Variables
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-logit.html}
#' @import methods
#' @export Zelig-logit
#' @exportClass Zelig-logit
#' 
#' @include model-zelig.R
#' @include model-gee.R
#' @include model-gamma.R
#' @include model-zelig.R
#' @include model-glm.R
#' @include model-binchoice.R

zlogit <- setRefClass("Zelig-logit",
                      contains = "Zelig-binchoice")
  
zlogit$methods(
  initialize = function() {
    callSuper()
    .self$name <- "logit"
    .self$link <- "logit"
    .self$description = "Logistic Regression for Dichotomous Dependent Variables"
    .self$packageauthors <- "R Core Team"
    .self$wrapper <- "logit"
  }
)


zlogit$methods(
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

