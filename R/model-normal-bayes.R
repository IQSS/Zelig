#' @include model-zelig.R
#' @include model-bayes.R
#' @include model-normal

znormalbayes <- setRefClass("Zelig-normal-bayes",
                             contains = c("Zelig-bayes",
                                          "Zelig-normal"))

znormalbayes$methods(
  initialize = function() {
    callSuper()
    .self$name <- "normalbayes" # CC: should't it be lsbayes?
    .self$year <- 2013
    .self$category <- "continuous"
    .self$authors <- "Ben Goodrich, Ying Lu"
    .self$description = "Bayesian Normal Linear Regression"
    .self$fn <- quote(MCMCpack::MCMCregress)
    # JSON from parent
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

