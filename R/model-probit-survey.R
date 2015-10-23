#' Survey-Weighted Probit Regression for Dichotomous Dependent Variables
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-probitsurvey.html}
#' @import methods
#' @export Zelig-probit-survey
#' @exportClass Zelig-probit-survey
#'
#' @include model-zelig.R
#' @include model-survey.R
#' @include model-binchoice-survey.R
  
zprobitsurvey <- setRefClass("Zelig-probit-survey",
                       contains = "Zelig-binchoice-survey")

zprobitsurvey$methods(
  initialize = function() {
    callSuper()
    .self$name <- "probit-survey"
    .self$link <- "probit"
    .self$description = "Survey-Weighted Probit Regression for Dichotomous Dependent Variables"
    .self$wrapper <- "probit.survey"
  }
)


