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
    diag <- coda::geweke.diag(.self$getcoef())
    if(!citation("coda") %in% .self$refs){
      .self$refs<-c(.self$refs,citation("coda"))
    }
    return(diag)
  } 
)

zbayes$methods(
  heidel.diag = function() {
    diag <- coda::heidel.diag(.self$getcoef())
    if(!citation("coda") %in% .self$refs){
      .self$refs<-c(.self$refs,citation("coda"))
    }
    return(diag)
  } 
)

zbayes$methods(
  raftery.diag = function() {
    diag <- coda::raftery.diag(.self$getcoef())
    if(!citation("coda") %in% .self$refs){
      .self$refs<-c(.self$refs,citation("coda"))
    }
    return(diag)
  } 
)
