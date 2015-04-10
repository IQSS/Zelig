#' Generalized Estimating Equation for Normal Regression
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-normalgee.html}
#' @import methods
#' @export Zelig-normal-gee
#' @exportClass Zelig-normal-gee
#'  
#' @include model-zelig.R
#' @include model-gee.R
#' @include model-normal.R

znormalgee <- setRefClass("Zelig-normal-gee",
                           contains = c("Zelig-gee", "Zelig-normal"))

znormalgee$methods(
  initialize = function() {
    callSuper()
    .self$name <- "normal-gee"
    .self$family <- "gaussian"
    .self$link <- "identity"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$year <- 2011
    .self$category <- "continuous"
    .self$authors <- "Patrick Lam"
    .self$description = "General Estimating Equation for Normal Regression"
    .self$fn <- quote(geepack::geeglm)
    # JSON from parent
    .self$wrapper <- "normal.gee"
  }
)
