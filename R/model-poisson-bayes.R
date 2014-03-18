zpoissonbayes <- setRefClass("Zelig-poisson-bayes",
                             contains="Zelig")

zpoissonbayes$methods(
  initialize = function() {
    callSuper()
    .self$model <- "poissonbayes"
    .self$authors <- "Ben Goodrich, Ying Lu"
    .self$year <- 2013
    .self$category <- "count"
    .self$text <- "Bayesian Poisson Regression"
    .self$fn <- quote(MCMCpack::MCMCpoisson)
  }
)

zpoissonbayes$methods(
  zelig = function(formula, burnin = 1000, mcmc = 10000, verbose = 0, ..., data) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    callSuper(formula=formula, data=data, ...)
    .self$model.call$burnin <- burnin
    .self$model.call$mcmc <- mcmc
    .self$model.call$verbose <- verbose
    .self$zelig.out <- eval(.self$model.call)
  }
)

zpoissonbayes$methods(
  param = function(num, ...) {
  }
)

zpoissonbayes$methods(
  qi = function(x) {
    #   # If either of the parameters are invalid,
    #   # Then return NA for both qi's
      if (is.null(x) || is.na(x))
        return(list(ev=NA, pv=NA))
      # Extract inverse-link and simulated parameters (respectively)
      eta <- .self$zelig.out %*% t(x)
      # Give matrix identical rows/columns to the simulated parameters
      ev <- pv <- matrix(NA, nrow(eta), ncol(eta))
      dimnames(ev) <- dimnames(pv) <- dimnames(eta)
      # Compute Expected Values
      ev <- poisson()$linkinv(eta)
      # Compute Predicted Values
      for (i in 1:ncol(ev))
        pv[, i] <- rpois(length(ev[, i]), ev[, i])
    return(list("Expected Values: E(Y|X)"  = ev,
                "Predicted Values: Y|X"    = pv))
  }
)
