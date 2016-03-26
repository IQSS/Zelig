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
    if(!identical(bootstrap,FALSE)){
      stop("Error: The bootstrap is not available for Markov chain Monte Carlo (MCMC) models.")
    }
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    if (missing(verbose))
      verbose <- round((mcmc + burnin) / 10)
    if (factors < 2)
      stop("Number of factors needs to be at least 2")
    .self$model.call$verbose <- verbose
    .self$model.call$x <- formula
    .self$model.call$factors <- factors
    callSuper(formula = formula, data = data,..., by = by, bootstrap = FALSE)
  }
)

zfactorbayes$methods(
  qi = function() {
    return(NULL)
  }
)

# The following diagnostics are also in Zelig-bayes, which unfortunately Zelig-factor-bayes does not currently inherit.
zfactorbayes$methods(
  geweke.diag = function() {
    diag <- lapply(.self$zelig.out$z.out, coda::geweke.diag)
    # Collapse if only one list element for prettier printing
    if(length(diag)==1){
        diag<-diag[[1]]
    }


    if(!citation("coda") %in% .self$refs){
      .self$refs<-c(.self$refs,citation("coda"))
    }
    ref1<-bibentry(
            bibtype="InCollection",
            title = "Evaluating the accuracy of sampling-based approaches to calculating posterior moments.",
            booktitle = "Bayesian Statistics 4",
            author = person("John", "Geweke"),
            year = 1992,
            publisher = "Clarendon Press",
            address = "Oxford, UK",
            editor = c(person("JM", "Bernado"), person("JO", "Berger"), person("AP", "Dawid"), person("AFM", "Smith")) 
            )
    .self$refs<-c(.self$refs,ref1)
    return(diag)
  } 
)

zfactorbayes$methods(
  heidel.diag = function() {
    diag <- lapply(.self$zelig.out$z.out, coda::heidel.diag)
    # Collapse if only one list element for prettier printing
    if(length(diag)==1){
        diag<-diag[[1]]
    }


    if(!citation("coda") %in% .self$refs){
      .self$refs<-c(.self$refs,citation("coda"))
    }
    ref1<-bibentry(
            bibtype="Article",
            title = "Simulation run length control in the presence of an initial transient.",
            author = c(person("P", "Heidelberger"), person("PD", "Welch")),
            journal = "Operations Research",
            volume = 31,
            year = 1983,
            pages = "1109--44")
    .self$refs<-c(.self$refs,ref1)
    return(diag)
  } 
)

zfactorbayes$methods(
  raftery.diag = function() {
    diag <- lapply(.self$zelig.out$z.out, coda::raftery.diag)
    # Collapse if only one list element for prettier printing
    if(length(diag)==1){
        diag<-diag[[1]]
    }


    if(!citation("coda") %in% .self$refs){
      .self$refs<-c(.self$refs,citation("coda"))
    }
    ref1<-bibentry(
            bibtype="Article",
            title = "One long run with diagnostics: Implementation strategies for Markov chain Monte Carlo.",
            author = c(person("Adrian E", "Raftery"), person("Steven M", "Lewis")),
            journal = "Statistical Science",
            volume = 31,
            year = 1992,
            pages = "1109--44")
    ref2<-bibentry(
            bibtype="InCollection",
            title = "The number of iterations, convergence diagnostics and generic Metropolis algorithms.",
            booktitle = "Practical Markov Chain Monte Carlo",
            author = c(person("Adrian E", "Raftery"), person("Steven M", "Lewis")),
            year = 1995,
            publisher = "Chapman and Hall",
            address = "London, UK",
            editor = c(person("WR", "Gilks"), person("DJ", "Spiegelhalter"), person("S", "Richardson")) 
            )
    .self$refs<-c(.self$refs,ref1,ref2)
    return(diag)
  } 
)

