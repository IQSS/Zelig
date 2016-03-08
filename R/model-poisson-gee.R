#' Generalized Estimating Equation for Poisson Regression
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-poissongee.html}
#' @import methods
#' @export Zelig-poisson-gee
#' @exportClass Zelig-poisson-gee
#'
#' @include model-zelig.R
#' @include model-gee.R
#' @include model-poisson.R

zpoissongee <- setRefClass("Zelig-poisson-gee",
                           contains = c("Zelig-gee", "Zelig-poisson"))

zpoissongee$methods(
  initialize = function() {
    callSuper()
    .self$name <- "poisson-gee"
    .self$family <- "poisson"
    .self$link <- "log"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$year <- 2011
    .self$category <- "continuous"
    .self$authors <- "Patrick Lam"
    .self$description = "General Estimating Equation for Poisson Regression"
    .self$fn <- quote(geepack::geeglm)
    # JSON from parent
    .self$wrapper <- "poisson.gee"
  }
)


zpoissongee$methods(
  param = function(z.out, method="mvn") {
    simparam.local <- callSuper(z.out, method=method)
    return(simparam.local$simparam) # no ancillary parameter
  }
)

