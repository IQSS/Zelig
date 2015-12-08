#' Generalized Estimating Equation for Probit Regression
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-probitgee.html}
#' @import methods
#' @export Zelig-probit-gee
#' @exportClass Zelig-probit-gee
#'
#' @include model-zelig.R
#' @include model-binchoice-gee.R

zprobitgee <- setRefClass("Zelig-probit-gee",
                          contains = c("Zelig-binchoice-gee"))

zprobitgee$methods(
  initialize = function() {
    callSuper()
    .self$name <- "probit-gee"
    .self$link <- "probit"
    .self$description <- "General Estimating Equation for Probit Regression"
    .self$wrapper <- "probit.gee"
  }
)