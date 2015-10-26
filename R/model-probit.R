#' Probit Regression for Dichotomous Dependent Variables
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-probit.html}
#' @import methods
#' @export Zelig-probit
#' @exportClass Zelig-probit
#'
#' @include model-zelig.R
#' @include model-glm.R
#' @include model-binchoice.R
  
zprobit <- setRefClass("Zelig-probit",
                       contains = "Zelig-binchoice")

zprobit$methods(
  initialize = function() {
    callSuper()
    .self$name <- "probit"
    .self$link <- "probit"
    .self$description = "Probit Regression for Dichotomous Dependent Variables"
    .self$packageauthors <- "R Core Team"
    .self$wrapper <- "probit"
  }
)

zprobit$methods(
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



