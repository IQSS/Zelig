#' Logit Regression with Survey Weights
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-logitsurvey.html}
#' @import methods
#' @export Zelig-logit-survey
#' @exportClass Zelig-logit-survey
#' 
#' @include model-zelig.R
#' @include model-binchoice-survey.R

zlogitsurvey <- setRefClass("Zelig-logit-survey",
                           contains = c("Zelig-binchoice-survey"))

zlogitsurvey$methods(
  initialize = function() {
    callSuper()
    .self$name <- "logit-survey"
    .self$link <- "logit"
    .self$description <- "Logistic Regression with Survey Weights"
    .self$wrapper <- "logit.survey"
  }
)


zlogitsurvey$methods(
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