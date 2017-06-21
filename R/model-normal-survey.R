#' Normal Regression for Continuous Dependent Variables with Survey Weights
#'@param formula a symbolic representation of the model to be
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
#'@param model the name of a statistical model to estimate.
#'   For a list of supported models and their documentation see:
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
#'   effort. For example, to run the same model on all fifty states, you could
#'   use: \code{z.out <- zelig(y ~ x1 + x2, data = mydata, model = 'ls',
#'   by = 'state')} You may also use \code{by} to run models using MatchIt
#'   subclasses.
#'@param cite If is set to 'TRUE' (default), the model citation will be printed
#'   to the console.
#'
#' @details
#' Additional parameters avaialable to this model include:
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
#' @return Depending on the class of model selected, \code{zelig} will return
#'   an object with elements including \code{coefficients}, \code{residuals},
#'   and \code{formula} which may be summarized using
#'   \code{summary(z.out)} or individually extracted using, for example,
#'   \code{coef(z.out)}. See
#'   \url{http://docs.zeligproject.org/articles/getters.html} for a list of
#'   functions to extract model components. You can also extract whole fitted
#'   model objects using \code{\link{from_zelig_model}}.
#'
#' @examples
#' library(Zelig)
#' data(api, package = "survey")
#' z.out1 <- zelig(api00 ~ meals + yr.rnd, model = "normal.survey",eights = ~pw, data = apistrat)
#' summary(z.out1)
#'
#' @seealso Vignette: \url{http://docs.zeligproject.org/articles/zelig_normalsurvey.html}
#' @import methods
#' @export Zelig-normal
#' @exportClass Zelig-normal
#'
#' @include model-zelig.R
#' @include model-survey.R
#' @include model-normal.R


znormalsurvey <- setRefClass("Zelig-normal-survey",
                       contains = c("Zelig-survey"),
                       fields = list(family = "character",
                                  link = "character",
                                  linkinv = "function"))
                                  #, "Zelig-normal"))

znormalsurvey$methods(
  initialize = function() {
    callSuper()
    .self$name <- "normal-survey"
    .self$family <- "gaussian"
    .self$link <- "identity"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$category <- "continuous"
    .self$description <- "Normal Regression for Continuous Dependent Variables with Survey Weights"
    .self$outcome <- "continuous"
    # JSON
    .self$wrapper <- "normal.survey"
  }
)

znormalsurvey$methods(
  param = function(z.out, method="mvn") {
    degrees.freedom <- z.out$df.residual
    sig2 <- base::summary(z.out)$dispersion # not to call class summary method
    simalpha <- sqrt(degrees.freedom * sig2
                     / rchisq(.self$num, degrees.freedom))

    if(identical(method,"mvn")){
      simparam.local <- mvrnorm(n = .self$num,
                              mu = coef(z.out),
                              Sigma = vcov(z.out))
      simparam.local <- list(simparam = simparam.local, simalpha = simalpha)
      return(simparam.local)
    } else if(identical(method,"point")){
      return(list(simparam = t(as.matrix(coef(z.out))), simalpha = simalpha))
    }

  }
)

znormalsurvey$methods(
  qi = function(simparam, mm) {
    theta <- matrix(simparam$simparam %*% t(mm),
                    nrow = nrow(simparam$simparam))
    ev <- theta
    pv <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))
    for (j in 1:nrow(ev))
      pv[j, ] <- rnorm(ncol(ev),
                       mean = ev[j, ],
                       sd = simparam$simalpha[j])
    return(list(ev = ev, pv = pv))
  }
)

znormalsurvey$methods(
  mcfun = function(x, b0=0, b1=1, alpha=1, sim=TRUE){
    y <- b0 + b1*x + sim * rnorm(n=length(x), sd=alpha)
    return(y)
  }
)
