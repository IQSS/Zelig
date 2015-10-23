#' Survey-Weighted Logistic Regression for Dichotomous Dependent Variables
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-logitsurvey.html}
#' @import methods
#' @export Zelig-logit-survey
#' @exportClass Zelig-logit-survey
#' 
#' @include model-zelig.R
#' @include model-survey.R
#' @include model-binchoice-survey.R

zlogitsurvey <- setRefClass("Zelig-logit-survey",
                      contains = "Zelig-binchoice-survey")
  
zlogitsurvey$methods(
  initialize = function() {
    callSuper()
    .self$name <- "logit-survey"
    .self$link <- "logit"
    .self$description = "Survey-Weighted Logistic Regression for Dichotomous Dependent Variables"
    .self$wrapper <- "logit.survey"
  }
)

