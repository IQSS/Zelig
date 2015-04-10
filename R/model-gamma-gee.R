#' Generalized Estimating Equation for Gamma Regression
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-gammagee.html}
#' @import methods
#' @export Zelig-gamma
#' @exportClass Zelig-gamma
#' 
#' @include model-zelig.R
#' @include model-gee.R
#' @include model-gamma.R

zgammagee <- setRefClass("Zelig-gamma-gee",
                           contains = c("Zelig-gee", "Zelig-gamma"))

zgammagee$methods(
  initialize = function() {
    callSuper()
    .self$name <- "gamma-gee"
    .self$family <- "Gamma"
    .self$link <- "inverse"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$year <- 2011
    .self$category <- "continuous"
    .self$authors <- "Patrick Lam"
    .self$description = "General Estimating Equation for Gamma Regression"
    .self$fn <- quote(geepack::geeglm)
    # JSON from parent
    .self$wrapper <- "gamma.gee"
  }
)
