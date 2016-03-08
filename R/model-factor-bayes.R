#' Bayesian Factor Analysis
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-factorbayes.html}
#' @import methods
#' @export Zelig-factor-bayes
#' @exportClass Zelig-factor-bayes
#' 
#' @include model-zelig.R

zfactorbayes <- setRefClass("Zelig-factor-bayes",
                            contains = c("Zelig"))

zfactorbayes$methods(
  initialize = function() {
    callSuper()
    .self$name <- "factor-bayes"
    .self$year <- 2013
    .self$authors <- "Ben Goodrich, Ying Lu"
    .self$packageauthors <- "Andrew D. Martin, Kevin M. Quinn, and Jong Hee Park"
    .self$description = "Bayesian Factor Analysis"
    .self$fn <- quote(MCMCpack::MCMCfactanal)
    # JSON from parent
    .self$wrapper <- "factor.bayes"
  }
)

zfactorbayes$methods(
  zelig = function(formula, 
                   factors = 2,
                   burnin = 1000, mcmc = 20000, 
                   verbose = 0, 
                   ..., 
                   data,
                   by = NULL,
                   bootstrap = FALSE) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    if (missing(verbose))
      verbose <- round((mcmc + burnin) / 10)
    if (factors < 2)
      stop("Number of factors needs to be at least 2")
    .self$model.call$verbose <- verbose
    .self$model.call$x <- formula
    .self$model.call$factors <- factors
    callSuper(formula = formula, data = data,..., by = by, bootstrap = bootstrap)
  }
)

zfactorbayes$methods(
  qi = function() {
    return(NULL)
  }
)
