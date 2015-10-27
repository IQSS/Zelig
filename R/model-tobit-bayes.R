#' Bayesian Tobit Regression
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-tobitbayes.html}
#' @import methods
#' @export Zelig-tobit-bayes
#' @exportClass Zelig-tobit-bayes
#'
#' @include model-zelig.R
#' @include model-bayes.R
#' @include model-tobit.R

ztobitbayes <- setRefClass("Zelig-tobit-bayes",
                           contains = c("Zelig-bayes",
                                        "Zelig-tobit"))

ztobitbayes$methods(
  initialize = function() {
    callSuper()
    .self$name <- "tobit-bayes"
    .self$year <- 2013
    .self$category <- "dichotomous"
    .self$authors <- "Ben Goodrich, Ying Lu"
    .self$description = "Bayesian Tobit Regression for a Censored Dependent Variable"
    .self$fn <- quote(MCMCpack::MCMCtobit)
    # JSON from parent
    .self$wrapper <- "tobit.bayes"
  }
)

ztobitbayes$methods(
  param = function(z.out) {
    if (length(.self$below) == 0)
      .self$below <- 0
    if (length(.self$above) == 0)
      .self$above <- Inf
    simparam.local <- list()
    simparam.local$simparam <- z.out[, 1:(ncol(z.out) - 1)]
    simparam.local$simalpha <- sqrt(z.out[, ncol(z.out)])
    return(simparam.local)
  }
)

ztobitbayes$methods(
  mcfun = function(x, b0=0, b1=1, alpha=1, sim=TRUE){
    mu <- b0 + b1 * x
    ystar <- rnorm(n=length(x), mean=mu, sd=alpha)
    if(sim){
        y <- (ystar>0) * ystar  # censoring from below at zero
        return(y)
    }else{
        y.uncensored.hat.tobit<- mu + dnorm(mu, mean=0, sd=alpha)/pnorm(mu, mean=0, sd=alpha)
        y.hat.tobit<- y.uncensored.hat.tobit * (1- pnorm(0, mean=mu, sd=alpha) )  # expected value of censored outcome
        return(y.hat.tobit)
    }
  }
)