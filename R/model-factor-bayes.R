#' @include model-zelig.R

zfactorbayes <- setRefClass("Zelig-factor-bayes",
                            contains = c("Zelig"))

zfactorbayes$methods(
  initialize = function() {
    callSuper()
    .self$name <- "factorbayes"
    .self$year <- 2013
    .self$authors <- "Ben Goodrich, Ying Lu"
    .self$description = "Bayesian Factor Analysis"
    .self$fn <- quote(MCMCpack::MCMCfactanal)
    # JSON from parent
  }
)

zfactorbayes$methods(
  zelig = function(formula, 
                   factors = 2,
                   burnin = 1000, mcmc = 20000, 
                   verbose = 0, 
                   ..., 
                   data,
                   by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    if (missing(verbose))
      verbose <- round((mcmc + burnin) / 10)
    if (factors < 2)
      stop("Number of factors needs to be at least 2")
    .self$model.call$verbose <- verbose
    .self$model.call$x <- formula
    .self$model.call$factors <- factors
    callSuper(formula = formula, data = data,..., by = by)
  }
)

zfactorbayes$methods(
  qi = function() {
    return(NULL)
  }
)
