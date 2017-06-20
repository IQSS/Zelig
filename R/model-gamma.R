#' Gamma Regression for Continuous, Positive Dependent Variables
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
#' data(coalition)
#' z.out <- zelig(duration ~ fract + numst2, model = "gamma", data = coalition)
#' summary(z.out)
#'
#' @seealso Vignette: \url{http://docs.zeligproject.org/articles/zelig_gamma.html}
#' @import methods
#' @export Zelig-gamma
#' @exportClass Zelig-gamma
#'
#' @include model-zelig.R
#' @include model-glm.R
zgamma <- setRefClass("Zelig-gamma",
                      contains = "Zelig-glm")

zgamma$methods(
  initialize = function() {
    callSuper()
    .self$name <- "gamma"
    .self$family <- "Gamma"
    .self$link <- "inverse"
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$year <- 2007
    .self$category <- "bounded"
    .self$description <- "Gamma Regression for Continuous, Positive Dependent Variables"
    # JSON
    .self$outcome <- "continous"
    .self$wrapper <- "gamma"
  }
)

zgamma$methods(
  param = function(z.out, method="mvn") {
    shape <- MASS::gamma.shape(z.out)
    if(identical(method, "mvn")){
      simalpha <- rnorm(n = .self$num, mean = shape$alpha, sd = shape$SE)
      simparam.local <- mvrnorm(n = .self$num, mu = coef(z.out), Sigma = vcov(z.out))
      simparam.local <- list(simparam = simparam.local, simalpha = simalpha)
      return(simparam.local)
    } else if(identical(method,"point")){
      return(list(simparam = t(as.matrix(coef(z.out))), simalpha = shape$alpha ))
    }
  }
)

zgamma$methods(
  qi = function(simparam, mm) {
    coeff <- simparam$simparam
    eta <- (coeff %*% t(mm) ) * simparam$simalpha  # JH need to better understand this parameterization.  Coefs appear parameterized so E(y_i) = 1/ (x_i\hat{\beta})
    theta <- matrix(1 / eta, nrow = nrow(coeff), ncol=1)
    ev <- theta * simparam$simalpha
    pv<- matrix(rgamma(nrow(ev), shape = simparam$simalpha, scale = theta), nrow=nrow(ev), ncol=1)
    return(list(ev = ev, pv = pv))
  }
)

zgamma$methods(
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

