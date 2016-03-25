#' Bayes Model object for inheritance across models in Zelig
#'
#' @import methods
#' @export Zelig-bayes
#' @exportClass Zelig-bayes
#'
#' @include model-zelig.R
zbayes <- setRefClass("Zelig-bayes",
                      contains = "Zelig")

zbayes$methods(
  initialize = function() {
    callSuper()
    .self$packageauthors <- "Andrew D. Martin, Kevin M. Quinn, and Jong Hee Park"
    .self$modelauthors <- "Ben Goodrich, and Ying Lu"
  }
)

zbayes$methods(
  zelig = function(formula, 
                   burnin = 1000, mcmc = 10000, 
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
#     .self$model.call$family <- call(.self$family, .self$link)
    .self$model.call$verbose <- verbose
    .self$num <- mcmc # CC: check
    callSuper(formula = formula, data = data, ..., by = by, bootstrap = FALSE)
  }
)

zbayes$methods(
  param = function(z.out) {
    return(z.out)
  }
)

zbayes$methods(
  getcoef = function() {
    "Get estimated model coefficients"
    return(.self$zelig.out$z.out[[1]])
  } 
)

zbayes$methods(
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

zbayes$methods(
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

zbayes$methods(
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
