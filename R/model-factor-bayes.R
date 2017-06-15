#' Bayesian Factor Analysis
#'
#'
#' @param formula a symbolic representation of the model to be
#'   estimated, in the form \code{\~\ Y1 + Y2 + Y3}, where Y1, Y2, and Y3 are variables
#'   of interest in factor analysis (manifest variables), assumed to be
#'   normally distributed. The model requires a minimum of three manifest
#'   variables contained in the
#'   same dataset. The \code{+} symbol means ``inclusion'' not
#'   ``addition.''
#' @param factors number of the factors to be fitted (defaults to 2).
#' @param model the name of a statistical model to estimate.
#'   For a list of supported models and their documentation see:
#'   \url{http://docs.zeligproject.org/articles/}.
#' @param data the name of a data frame containing the variables
#'   referenced in the formula or a list of multiply imputed data frames
#'   each having the same variable names and row numbers (created by
#'   \code{Amelia} or \code{\link{to_zelig_mi}}).
#' @param ... additional arguments passed to \code{zelig},
#'   relevant for the model to be estimated.
#' @param by a factor variable contained in \code{data}. If supplied,
#'   \code{zelig} will subset
#'   the data frame based on the levels in the \code{by} variable, and
#'   estimate a model for each subset. This can save a considerable amount of
#'   effort. For example, to run the same model on all fifty states, you could
#'   use: \code{z.out <- zelig(y ~ x1 + x2, data = mydata, model = 'ls',
#'   by = 'state')} You may also use \code{by} to run models using MatchIt
#'   subclasses.
#' @param cite If is set to 'TRUE' (default), the model citation will be printed
#'   to the console.
#'
#' @details
#' In addition, zelig() accepts the following additional arguments for model specification:
#' \itemize{
#'      \item lambda.constraints: list containing the equality or
#'      inequality constraints on the factor loadings. Choose from one of the following forms:
#'      \item `varname = list()``: by default, no constraints are imposed.
#'      \item varname = list(d, c): constrains the dth loading for the
#'            variable named varname to be equal to c.
#'      \item varname = list(d, +): constrains the dd th loading for the variable named varname to be positive;
#'      \item varname = list(d, -): constrains the dd th loading for the variable named varname to be negative.
#'      \item std.var: defaults to FALSE (manifest variables are rescaled to
#'      zero mean, but retain observed variance). If TRUE, the manifest
#'      variables are rescaled to be mean zero and unit variance.
#' }
#'
#' In addition, zelig() accepts the following additional inputs for bayes.factor:
#' \itemize{
#'     \item burnin: number of the initial MCMC iterations to be discarded (defaults to 1,000).
#'     \item mcmc: number of the MCMC iterations after burnin (defaults to 20,000).
#'     \item thin: thinning interval for the Markov chain. Only every thin-th
#'         draw from the Markov chain is kept. The value of mcmc must be divisible
#'         by this value. The default value is 1.
#'     \item verbose: defaults to FALSE. If TRUE, the
#'     progress of the sampler (every 10%10%) is printed to the screen.
#'     \item seed: seed for the random number generator. The default is NA which
#'     corresponds to a random seed 12345.
#'     \item Lambda.start: starting values of the factor loading matrix Λ, either a
#'     scalar (all unconstrained loadings are set to that value), or a matrix with
#'     compatible dimensions. The default is NA, where the start value are set to
#'     be 0 for unconstrained factor loadings, and 0.5 or −− 0.5 for constrained
#'     factor loadings (depending on the nature of the constraints).
#'     \item Psi.start: starting values for the uniquenesses, either a scalar
#'     (the starting values for all diagonal elements of Ψ are set to be this value),
#'     or a vector with length equal to the number of manifest variables. In the latter
#'     case, the starting values of the diagonal elements of Ψ take the values of
#'     Psi.start. The default value is NA where the starting values of the all the
#'     uniquenesses are set to be 0.5.
#'     \item store.lambda: defaults to TRUE, which stores the posterior draws of the factor loadings.
#'     \item store.scores: defaults to FALSE. If TRUE, stores the posterior draws of the
#'     factor scores. (Storing factor scores may take large amount of memory for a large
#'     number of draws or observations.)
#' }
#'
#' The model also accepts the following additional arguments to specify prior parameters:
#' \itemize{
#'     \item l0: mean of the Normal prior for the factor loadings, either a scalar or a
#'     matrix with the same dimensions as ΛΛ. If a scalar value, that value will be the
#'     prior mean for all the factor loadings. Defaults to 0.
#'     \item L0: precision parameter of the Normal prior for the factor loadings, either
#'     a scalar or a matrix with the same dimensions as ΛΛ. If L0 takes a scalar value,
#'     then the precision matrix will be a diagonal matrix with the diagonal elements
#'     set to that value. The default value is 0, which leads to an improper prior.
#'     \item a0: the shape parameter of the Inverse Gamma prior for the uniquenesses
#'     is a0/2. It can take a scalar value or a vector. The default value is 0.001.
#'     \item b0: the scale parameter of the Inverse Gamma prior for the uniquenesses
#'     is b0/2. It can take a scalar value or a vector. The default value is 0.001.
#' }
#'
#' Additional parameters avaialable to many models include:
#' \itemize{
#'   \item weights: vector of weight values or a name of a variable in the dataset
#'   by which to weight the model. For more information see:
#'   \url{http://docs.zeligproject.org/articles/weights.html}.
#'   \item bootstrap: logical or numeric. If \code{FALSE} don't use bootstraps to
#'   robustly estimate uncertainty around model parameters due to sampling error.
#'   If an integer is supplied, the number of boostraps to run.
#'   For more information see:
#'   \url{http://docs.zeligproject.org/articles/bootstraps.html}.
#' }
#'
#' @return Depending on the class of model selected, \code{zelig} will return
#'   an object with elements including \code{coefficients}, \code{residuals},
#'   and \code{formula} which may be summarized using
#'   \code{summary(z.out)} or individually extracted using, for example,
#'   \code{coef(z.out)}. See
#'   \url{http://docs.zeligproject.org/articles/getters.html} for a list of
#'   functions to extract model components. You can also extract whole fitted
#'   model objects using \code{\link{from_zelig_model}}.
#'
#'
#' @seealso Vignette: \url{http://docs.zeligproject.org/articles/zelig_factorbayes.html}
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
