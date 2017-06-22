#' Log-Normal Regression for Duration Dependent Variables
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
#' @param robust defaults to FALSE. If TRUE, zelig() computes robust standard errors based
#' on sandwich estimators (see and ) based on the options in cluster.
#' @param cluster  if robust = TRUE, you may select a variable to define groups of correlated
#' observations. Let x3 be a variable that consists of either discrete numeric values, character
#' strings, or factors that define strata. Then
#   'z.out <- zelig(y ~ x1 + x2, robust = TRUE, cluster = "x3", model = "exp", data = mydata)
#'  means that the observations can be correlated within the strata defined by the variable x3,
#'  and that robust standard errors should be calculated according to those clusters.
#'  If robust = TRUE but cluster is not specified, zelig() assumes that each observation falls
#'  into its own cluster.
#'
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
#'@examples
#' library(Zelig)
#' data(coalition)
#' z.out <- zelig(Surv(duration, ciep12) ~ fract + numst2, model ="lognorm",  data = coalition)
#' summary(z.out)
#'
#' @seealso Vignette: \url{http://docs.zeligproject.org/articles/zelig_lognorm.html}
#' @import methods
#' @export Zelig-lognorm
#' @exportClass Zelig-lognorm
#'
#' @include model-zelig.R

zlognorm <- setRefClass("Zelig-lognorm",
                        contains ="Zelig",
                        fields = list(linkinv = "function"))

zlognorm$methods(
  initialize = function() {
    callSuper()
    .self$name <- "lognorm"
    .self$authors <- "Matthew Owen, Olivia Lau, Kosuke Imai, Gary King"
    .self$packageauthors <- "Terry M Therneau, and Thomas Lumley"
    .self$year <- 2007
    .self$description <- "Log-Normal Regression for Duration Dependent Variables"
    .self$fn <- quote(survival::survreg)
    .self$linkinv <- survreg.distributions[["lognormal"]]$itrans
    # JSON
    .self$outcome <- "discrete"
    .self$wrapper <- "lognorm"
    .self$acceptweights <- TRUE
  }
)

zlognorm$methods(
  zelig = function(formula, ..., robust = FALSE, cluster = NULL, data, weights = NULL, by = NULL, bootstrap = FALSE) {

    localFormula <- formula # avoids CRAN warning about deep assignment from formula existing separately as argument and field
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    if (!(is.null(cluster) || robust))
      stop("If cluster is specified, then `robust` must be TRUE")
    # Add cluster term
    if (robust || !is.null(cluster))
      localFormula <- cluster.formula(localFormula, cluster)
    .self$model.call$dist <- "lognormal"
    .self$model.call$model <- FALSE
    callSuper(formula = localFormula, data = data, ..., robust = robust,
              cluster = cluster, weights = weights, by = by, bootstrap = bootstrap)

    if(!robust){
      fn2 <- function(fc, data) {
        fc$data <- data
        return(fc)
      }
      robust.model.call <- .self$model.call
      robust.model.call$robust <- TRUE

      robust.zelig.out <- .self$data %>%
      group_by_(.self$by) %>%
      do(z.out = eval(fn2(robust.model.call, quote(as.data.frame(.))))$var )

      .self$test.statistics<- list(robust.se = robust.zelig.out$z.out)
    }
  }
)

zlognorm$methods(
  param = function(z.out, method="mvn") {
    if(identical(method,"mvn")){
      coeff <- coef(z.out)
      mu <- c(coeff, log(z.out$scale))
      cov <- vcov(z.out)
      simulations <- mvrnorm(.self$num, mu = mu, Sigma = cov)
      simparam.local <- as.matrix(simulations[, 1:length(coeff)])
      simalpha <- as.matrix(simulations[, -(1:length(coeff))])
      simparam.local <- list(simparam = simparam.local, simalpha = simalpha)
      return(simparam.local)
    } else if(identical(method,"point")){
      return(list(simparam = t(as.matrix(coef(z.out))), simalpha = log(z.out$scale) ))
    }
  }
)

zlognorm$methods(
  qi = function(simparam, mm) {
    alpha <- simparam$simalpha
    beta <- simparam$simparam
    coeff <- simparam$simparam
    eta <- coeff %*% t(mm)
    theta <- as.matrix(apply(eta, 2, linkinv))
    ev <- exp(log(theta) + 0.5 * (exp(alpha))^2)
    pv <- as.matrix(rlnorm(n=length(ev), meanlog=log(theta), sdlog=exp(alpha)), nrow=length(ev), ncol=1)
    dimnames(ev) <- dimnames(theta)
    return(list(ev = ev, pv = pv))
  }
)

zlognorm$methods(
  mcfun = function(x, b0=0, b1=1, alpha=1, sim=TRUE){
    .self$mcformula <- as.Formula("Surv(y.sim, event) ~ x.sim")

    mu <- b0 + b1 * x
    event <- rep(1, length(x))
    y.sim <- rlnorm(n=length(x), meanlog=mu, sdlog=alpha)
    y.hat <- exp(mu + 0.5*alpha^2)

    if(sim){
        mydata <- data.frame(y.sim=y.sim, event=event, x.sim=x)
        return(mydata)
    }else{
        mydata <- data.frame(y.hat=y.hat, event=event, x.seq=x)
        return(mydata)
    }
  }
)
