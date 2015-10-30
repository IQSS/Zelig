#' Bayesian Normal Linear Regression
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-normalbayes.html}
#' @import methods
#' @export Zelig-normal-bayes
#' @exportClass Zelig-normal-bayes
#'  
#' @include model-zelig.R
#' @include model-bayes.R
#' @include model-normal.R

znormalbayes <- setRefClass("Zelig-normal-bayes",
                             contains = c("Zelig-bayes",
                                          "Zelig-normal"))

znormalbayes$methods(
  initialize = function() {
    callSuper()
    .self$name <- "normal-bayes" # CC: should't it be lsbayes?
    .self$year <- 2013
    .self$category <- "continuous"
    .self$authors <- "Ben Goodrich, Ying Lu"
    .self$description = "Bayesian Normal Linear Regression"
    .self$fn <- quote(MCMCpack::MCMCregress)
    # JSON from parent
    .self$wrapper <- "normal.bayes"
  }
)

znormalbayes$methods(
  qi = function(simparam, mm) {
    # Extract simulated parameters and get column names
    coef <- simparam
    cols <- colnames(coef)
    # Place the simulated variances in their own vector
    sigma2 <- coef[, ncol(coef)]
    # Remove the "sigma2" (variance) parameter
    # which should already be placed
    # in the simulated parameters
    cols <- cols[ ! "sigma2" == cols ]
    coef <- coef[, cols]
    ev <- coef %*% t(mm)
    pv <- matrix(rnorm(nrow(ev), ev, sqrt(sigma2)))
    return(list(ev = ev, pv = pv))
  }
)

znormalbayes$methods(
  mcfun = function(x, b0=0, b1=1, alpha=1, sim=TRUE){
    y <- b0 + b1*x + sim * rnorm(n=length(x), sd=alpha)
    return(y)
  }
)