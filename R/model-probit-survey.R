#' Probit Regression with Survey Weights
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-probitsurvey.html}
#' @import methods
#' @export Zelig-probit-survey
#' @exportClass Zelig-probit-survey
#'
#' @include model-zelig.R
#' @include model-binchoice-survey.R

zprobitsurvey <- setRefClass("Zelig-probit-survey",
                          contains = c("Zelig-binchoice-survey"))

zprobitsurvey$methods(
  initialize = function() {
    callSuper()
    .self$name <- "probit-survey"
    .self$link <- "probit"
    .self$description <- "Probit Regression with Survey Weights"
    .self$wrapper <- "probit.survey"
  }
)

zprobitsurvey$methods(
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