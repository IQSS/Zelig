#' Gamma Regression with Survey Weights
#'
#'@param formula a symbolic representation of the model to be
#'   estimated, in the form \code{y ~ x1 + x2}, where \code{y} is the
#'   dependent variable and \code{x1} and \code{x2} are the explanatory
#'   variables, and \code{y}, \code{x1}, and \code{x2} are contained in the
#'   same dataset. (You may include more than two explanatory variables,
#'   of course.) The \code{+} symbol means ``inclusion'' not
#'   ``addition.'' You may also include interaction terms and main
#'   effects in the form \code{x1*x2} without computing them in prior
#'   steps; \code{I(x1*x2)} to include only the interaction term and
#'   exclude the main effects; and quadratic terms in the form
#'   \code{I(x1^2)}.
#'@param model the name of a statistical model to estimate.
#'   For a list of other supported models and their documentation see:
#'   \url{http://docs.zeligproject.org/articles/}.
#'@param data the name of a data frame containing the variables
#'   referenced in the formula or a list of multiply imputed data frames
#'   each having the same variable names and row numbers (created by
#'   \code{Amelia} or \code{\link{to_zelig_mi}}).
#'@param ... additional arguments passed to \code{zelig},
#'   relevant for the model to be estimated.
#'@param by a factor variable contained in \code{data}. If supplied,
#'   \code{zelig} will subset
#'   the data frame based on the levels in the \code{by} variable, and
#'   estimate a model for each subset. This can save a considerable amount of
#'   effort. You may also use \code{by} to run models using MatchIt
#'   subclasses.
#'@param cite If is set to 'TRUE' (default), the model citation will be printed
#'   to the console.
#'
#' @details
#' Additional parameters avaialable to this model include:
#' \itemize{
#'   \item \code{weights}: vector of weight values or a name of a variable in the dataset
#'   by which to weight the model. For more information see:
#'   \url{http://docs.zeligproject.org/articles/weights.html}.
#'   \item \code{bootstrap}: logical or numeric. If \code{FALSE} don't use bootstraps to
#'   robustly estimate uncertainty around model parameters due to sampling error.
#'   If an integer is supplied, the number of boostraps to run.
#'   For more information see:
#'   \url{http://docs.zeligproject.org/articles/bootstraps.html}.
#' }
#' @return Depending on the class of model selected, \code{zelig} will return
#'   an object with elements including \code{coefficients}, \code{residuals},
#'   and \code{formula} which may be summarized using
#'   \code{summary(z.out)} or individually extracted using, for example,
#'   \code{coef(z.out)}. See
#'   \url{http://docs.zeligproject.org/articles/getters.html} for a list of
#'   functions to extract model components. You can also extract whole fitted
#'   model objects using \code{\link{from_zelig_model}}.
#'@examples
#' library(Zelig)
#' data(api, package="survey")
#' z.out1 <- zelig(api00 ~ meals + yr.rnd, model = "gamma.survey",
#' weights = ~pw, data = apistrat)
#' summary(z.out1)
#'
#' @seealso Vignette: \url{http://docs.zeligproject.org/articles/zelig_gammasurvey.html}
#' @import methods
#' @export Zelig-gamma
#' @exportClass Zelig-gamma
#'
#' @include model-zelig.R
#' @include model-survey.R
#' @include model-gamma.R

zgammasurvey <- setRefClass("Zelig-gamma-survey",
                           contains = c("Zelig-survey", "Zelig-gamma"))

zgammasurvey$methods(
  initialize = function() {
    callSuper()
    .self$name <- "gamma-survey"
    .self$family <- "Gamma"
    .self$link <- "inverse"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$category <- "continuous"
    .self$description = "Gamma Regression with Survey Weights"
    # JSON from parent
    .self$wrapper <- "gamma.survey"
  }
)

zgammasurvey$methods(
  param = function(z.out, method="mvn") {
    shape <- MASS::gamma.shape(z.out)
    if(identical(method,"mvn")){
      simalpha <- rnorm(n = .self$num, mean = shape$alpha, sd = shape$SE)
      simparam.local <- mvrnorm(n = .self$num, mu = coef(z.out), Sigma = vcov(z.out))
      simparam.local <- list(simparam = simparam.local, simalpha = simalpha)
      return(simparam.local)
    } else if(identical(method,"point")){
      return(list(simparam = t(as.matrix(coef(z.out))), simalpha = shape$alpha))
    }
  }
)

zgammasurvey$methods(
  mcfun = function(x, b0=0, b1=1, alpha=1, sim=TRUE){
    lambda <- 1/(b0 + b1 * x)
    if(sim){
        y <- rgamma(n=length(x), shape=alpha, scale = lambda)
        return(y)
    }else{
        return(alpha * lambda)
    }
  }
)
