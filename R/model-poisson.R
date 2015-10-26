#' Poisson Regression for Event Count Dependent Variables
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-poisson.html}
#' @import methods
#' @export Zelig-poisson
#' @exportClass Zelig-poisson
#'
#' @include model-zelig.R
#' @include model-glm.R

zpoisson <- setRefClass("Zelig-poisson",
                        contains = "Zelig-glm",
                        fields = list(theta = "ANY"))

zpoisson$methods(
  initialize = function() {
    callSuper()
    .self$name <- "poisson"
    .self$family <- "poisson"
    .self$link <- "log"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$year <- 2007
    .self$category <- "count"
    .self$description <- "Poisson Regression for Event Count Dependent Variables"
    # JSON
    .self$outcome <- "discrete"
    .self$wrapper <- "poisson"
  }
)

zpoisson$methods(
  qi = function(simparam, mm) {
    eta <- simparam %*% t(mm)
    theta.local <- matrix(.self$linkinv(eta), nrow = nrow(simparam))
    ev <- theta.local
    pv <- matrix(NA, nrow = nrow(theta.local), ncol = ncol(theta.local))
    for (i in 1:ncol(theta.local))
      pv[, i] <- rpois(nrow(theta.local), lambda = theta.local[, i])
    return(list(ev = ev, pv = pv))
  }
)

zpoisson$methods(
  mcfun = function(x, b0=0, b1=1, ..., sim=TRUE){
    lambda <- exp(b0 + b1 * x)
    if(sim){
        y <- rpois(n=length(x), lambda=lambda)
        return(y)
    }else{
        return(lambda)
    }
  }
)
