#' Poisson Regression with Survey Weights
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-poissonsurvey.html}
#' @import methods
#' @export Zelig-poisson-gee
#' @exportClass Zelig-poisson-gee
#'
#' @include model-zelig.R
#' @include model-survey.R
#' @include model-poisson.R

zpoissonsurvey <- setRefClass("Zelig-poisson-survey",
                           contains = c("Zelig-survey", "Zelig-poisson"))

zpoissonsurvey$methods(
  initialize = function() {
    callSuper()
    .self$name <- "poisson-survey"
    .self$family <- "poisson"
    .self$link <- "log"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$category <- "continuous"
    .self$description = "Poisson Regression with Survey Weights"
    # JSON from parent
    .self$wrapper <- "poisson.survey"
  }
)

zpoissonsurvey$methods(
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

zpoissonsurvey$methods(
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
