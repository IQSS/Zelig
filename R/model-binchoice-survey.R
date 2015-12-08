#' Object for Binary Choice outcomes with Survey Weights
#' for inheritance across models in Zelig
#'
#' @import methods
#' @export Zelig-binchoice-survey
#' @exportClass Zelig-binchoice-survey
#'
#' @include model-zelig.R
#' @include model-binchoice.R
#' @include model-survey.R
zbinchoicesurvey <- setRefClass("Zelig-binchoice-survey",
                           contains = c("Zelig-survey",
                                        "Zelig-binchoice"))

zbinchoicesurvey$methods(
  initialize = function() {
    callSuper()
    .self$family <- "binomial"
    .self$category <- "continuous"
    # JSON from parent
  }
)

