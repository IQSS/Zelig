#' Normal Regression for Continuous Dependent Variables
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
#'
#' @return Depending on the class of model selected, \code{zelig} will return
#'   an object with elements including \code{coefficients}, \code{residuals},
#'   and \code{formula} which may be summarized using
#'   \code{summary(z.out)} or individually extracted using, for example,
#'   \code{coef(z.out)}. See
#'   \url{http://docs.zeligproject.org/articles/getters.html} for a list of
#'   functions to extract model components. You can also extract whole fitted
#'   model objects using \code{\link{from_zelig_model}}.
#'@param below (defaults to 0) The point at which the dependent variable is censored from below. If any values in the dependent variable are observed to be less than the censoring point, it is assumed that that particular observation is censored from below at the observed value. (See for a Bayesian implementation that supports both left and right censoring.)
#'@param robust defaults to FALSE. If TRUE, zelig() computes robust standard errors based on sandwich estimators (see and ) and the options selected in cluster.
#'@param if robust = TRUE, you may select a variable to define groups of correlated observations. Let x3 be a variable that consists of either discrete numeric values, character strings, or factors that define strata. Then
#'z.out <- zelig(y ~ x1 + x2, robust = TRUE, cluster = "x3", model = "tobit", data = mydata)
#'means that the observations can be correlated within the strata defined by the variable x3, and that robust standard errors should be calculated according to those clusters. If robust = TRUE but cluster is not specified, zelig() assumes that each observation falls into its own cluster.
#'@param formula a model fitting formula
#'
#'@examples
#' data(macro)
#' z.out1 <- zelig(unem ~ gdp + capmob + trade, model = "normal",
#' data = macro)
#' summary(z.out1)
#' x.high <- setx(z.out1, trade = quantile(macro$trade, 0.8))
#' x.low <- setx(z.out1, trade = quantile(macro$trade, 0.2))
#' s.out1 <- sim(z.out1, x = x.high, x1 = x.low)
#' summary(s.out1)
#' plot(s.out1)
#'
#'
#'@seealso Vignette: \url{http://docs.zeligproject.org/articles/zelig_normal.html}
#' @import methods
#' @export Zelig-normal
#' @exportClass Zelig-normal
#'
#' @include model-zelig.R
#' @include model-glm.R

znormal <- setRefClass("Zelig-normal",
                       contains = "Zelig-glm")

znormal$methods(
  initialize = function() {
    callSuper()
    .self$name <- "normal"
    .self$family <- "gaussian"
    .self$link <- "identity"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$year <- 2008
    .self$category <- "continuous"
    .self$description <- "Normal Regression for Continuous Dependent Variables"
    # JSON
    .self$outcome <- "continuous"
    .self$wrapper <- "normal"
  }
)

znormal$methods(
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

znormal$methods(
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

znormal$methods(
  mcfun = function(x, b0=0, b1=1, alpha=1, sim=TRUE){
    y <- b0 + b1*x + sim * rnorm(n=length(x), sd=alpha)
    return(y)
  }
)
