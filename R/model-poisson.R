#' Poisson Regression for Event Count Dependent Variables
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
#'@param id: where id is a variable which identifies the clusters. The data should be sorted by id and should be ordered within each cluster when appropriate
#'@param corstr: character string specifying the correlation structure: "independence", "exchangeable", "ar1", "unstructured" and "userdefined"
#'@param geeglm: See geeglm in package geepack for other function arguments
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
#'
#' @examples
#' library(Zelig)
#' data(sanction)
#' z.out <- zelig(num ~ target + coop, model = "poisson", data = sanction)
#' summary(z.out)
#'
#' @seealso Vignette: \url{http://docs.zeligproject.org/articles/zelig_poisson.html}
#' @import methods
#' @export Zelig-poisson
#' @exportClass Zelig-poisson
#'
#' @include model-zelig.R
#' @include model-glm.R
zpoisson <- setRefClass("Zelig-poisson",
                        contains = "Zelig-glm",
                        fields = list(theta = "ANY"))

zpoisson$methods(
  initialize = function() {
    callSuper()
    .self$name <- "poisson"
    .self$family <- "poisson"
    .self$link <- "log"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$year <- 2007
    .self$category <- "count"
    .self$description <- "Poisson Regression for Event Count Dependent Variables"
    # JSON
    .self$outcome <- "discrete"
    .self$wrapper <- "poisson"
  }
)

zpoisson$methods(
  qi = function(simparam, mm) {
    eta <- simparam %*% t(mm)
    theta.local <- matrix(.self$linkinv(eta), nrow = nrow(simparam))
    ev <- theta.local
    pv <- matrix(NA, nrow = nrow(theta.local), ncol = ncol(theta.local))
    for (i in 1:ncol(theta.local))
      pv[, i] <- rpois(nrow(theta.local), lambda = theta.local[, i])
    return(list(ev = ev, pv = pv))
  }
)

zpoisson$methods(
  mcfun = function(x, b0=0, b1=1, ..., sim=TRUE){
    lambda <- exp(b0 + b1 * x)
    if(sim){
        y <- rpois(n=length(x), lambda=lambda)
        return(y)
    }else{
        return(lambda)
    }
  }
)
