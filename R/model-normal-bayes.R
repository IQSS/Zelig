#' Bayesian Normal Linear Regression
#'
#' @param formula a symbolic representation of the model to be
#'   estimated, in the form \code{y \~\, x1 + x2}, where \code{y} is the
#'   dependent variable and \code{x1} and \code{x2} are the explanatory
#'   variables, and \code{y}, \code{x1}, and \code{x2} are contained in the
#'   same dataset. (You may include more than two explanatory variables,
#'   of course.) The \code{+} symbol means ``inclusion'' not
#'   ``addition.'' You may also include interaction terms and main
#'   effects in the form \code{x1*x2} without computing them in prior
#'   steps; \code{I(x1*x2)} to include only the interaction term and
#'   exclude the main effects; and quadratic terms in the form
#'   \code{I(x1^2)}.
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
#' @examples
#' data(macro)
#' z.out <- zelig(unem ~ gdp + capmob + trade, model = "normal.bayes", data = macro, verbose = FALSE)
#'
#' @details
#' Additional parameters avaialable to many models include:
#' \itemize{
#'   \item \code{weights}: vector of weight values or a name of a variable in the dataset
#'   by which to weight the model. For more information see:
#'   \url{http://docs.zeligproject.org/articles/weights.html}.
#'   \item \code{burnin}: number of the initial MCMC iterations to be discarded (defaults to 1,000).
#'   \item \code{mcmc}: number of the MCMC iterations after burnin (defaults to 10,000).
#'   \item \code{thin}: thinning interval for the Markov chain. Only every thin-th draw from the Markov chain is kept. The value of mcmc must be divisible by this value. The default value is 1.
#'   \item \code{verbose}: defaults to FALSE. If TRUE, the progress of the sampler (every 10\%) is printed to the screen.
#'   \item \code{seed}: seed for the random number generator. The default is \code{NA} which corresponds to a random seed of 12345.
#'   \item \code{beta.start}: starting values for the Markov chain, either a scalar or vector with length equal to the number of estimated coefficients. The default is \code{NA}, such that the maximum likelihood estimates are used as the starting values.
#' }
#' Use the following parameters to specify the model's priors:
#' \itemize{
#'     \item \code{b0}: prior mean for the coefficients, either a numeric vector or a scalar. If a scalar value, that value will be the prior mean for all the coefficients. The default is 0.
#'     \item \code{B0}: prior precision parameter for the coefficients, either a square matrix (with the dimensions equal to the number of the coefficients) or a scalar. If a scalar value, that value times an identity matrix will be the prior precision parameter. The default is 0, which leads to an improper prior.
#'     \item \code{c0}: c0/2 is the shape parameter for the Inverse Gamma prior on the variance of the disturbance terms.
#'     \item \code{d0}: d0/2 is the scale parameter for the Inverse Gamma prior on the variance of the disturbance terms.
#' }
#' @return Depending on the class of model selected, \code{zelig} will return
#'   an object with elements including \code{coefficients}, \code{residuals},
#'   and \code{formula} which may be summarized using
#'   \code{summary(z.out)} or individually extracted using, for example,
#'   \code{coef(z.out)}. See
#'   \url{http://docs.zeligproject.org/articles/getters.html} for a list of
#'   functions to extract model components. You can also extract whole fitted
#'   model objects using \code{\link{from_zelig_model}}.
#'
#' Use the following arguments to monitor the convergence of the Markov chain:
#' @param burnin: number of the initial MCMC iterations to be discarded (defaults to 1,000).
#' @param mcmc: number of the MCMC iterations after burnin (defaults to 10,000).
#' @param thin: thinning interval for the Markov chain. Only every thin-th draw from the Markov chain is kept. The value of mcmc must be divisible by this value. The default value is 1.
#' @param verbose: defaults to FALSE. If TRUE, the progress of the sampler (every 10%10%) is printed to the screen.
#' @param seed: seed for the random number generator. The default is NA, which corresponds to a random seed of 12345.
#' @param beta.start: starting values for the Markov chain, either a scalar or vector with length equal to the number of estimated coefficients. The default is NA, which uses the least squares estimates as the starting values.
#'
#' Use the following arguments to specify the model’s priors:
#'
#' @param b0: prior mean for the coefficients, either a numeric vector or a scalar. If a scalar, that value will be the prior mean for all the coefficients. The default is 0.
#' @param B0: prior precision parameter for the coefficients, either a square matrix (with the dimensions equal to the number of the coefficients) or a scalar. If a scalar, that value times an identity matrix will be the prior precision parameter. The default is 0, which leads to an improper prior.
#' @param c0: c0/2 is the shape parameter for the Inverse Gamma prior on the variance of the disturbance terms.
#' @param d0: d0/2 is the scale parameter for the Inverse Gamma prior on the variance of the disturbance terms.
#'
#' @examples
#'
#' data(macro)
#' z.out <- zelig(unem ~ gdp + capmob + trade, model = "normal.bayes",
#' data = macro, verbose = FALSE)
#'
#' z.out$geweke.diag()
#' z.out$heidel.diag()
#' z.out$raftery.diag()
#' summary(z.out)
#'
#' x.out <- setx(z.out)
#' s.out1 <- sim(z.out, x = x.out)
#' summary(s.out1)
#'
#' x.high <- setx(z.out, trade = quantile(macro$trade, prob = 0.8))
#' x.low <- setx(z.out, trade = quantile(macro$trade, prob = 0.2))
#'
#' s.out2 <- sim(z.out, x = x.high, x1 = x.low)
#' summary(s.out2)
#'
#' @seealso Vignette: \url{http://docs.zeligproject.org/articles/zelig_normalbayes.html}
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
