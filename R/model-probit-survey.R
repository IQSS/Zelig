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