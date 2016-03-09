#' Object for Binary Choice outcomes in Generalized Estimating Equations 
#' for inheritance across models in Zelig
#'
#' @import methods
#' @export Zelig-binchoice-gee
#' @exportClass Zelig-binchoice-gee
#'
#' @include model-zelig.R
#' @include model-binchoice.R
#' @include model-gee.R
zbinchoicegee <- setRefClass("Zelig-binchoice-gee",
                           contains = c("Zelig-gee",
                                        "Zelig-binchoice"))

zbinchoicegee$methods(
  initialize = function() {
    callSuper()
    .self$family <- "binomial"
    .self$year <- 2011
    .self$category <- "continuous"
    .self$authors <- "Patrick Lam"
    .self$fn <- quote(geepack::geeglm)
    # JSON from parent
  }
)

zbinchoicegee$methods(
  param = function(z.out, method="mvn") {
    simparam.local <- callSuper(z.out, method=method)
    return(simparam.local$simparam) # no ancillary parameter
  }
)
