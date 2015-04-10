#' Generalized Estimating Equation for Logit Regression
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-logitgee.html}
#' @import methods
#' @export Zelig-logit-gee
#' @exportClass Zelig-logit-gee
#' 
#' @include model-zelig.R
#' @include model-binchoice-gee.R

zlogitgee <- setRefClass("Zelig-logit-gee",
                           contains = c("Zelig-binchoice-gee"))

zlogitgee$methods(
  initialize = function() {
    callSuper()
    .self$name <- "logit-gee"
    .self$link <- "logit"
    .self$description <- "General Estimating Equation for Logistic Regression"
    .self$wrapper <- "logit.gee"
  }
)