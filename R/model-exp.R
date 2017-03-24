#' Exponential Regression for Duration Dependent Variables
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-exp.html}
#' @import methods
#' @export Zelig-exp
#' @exportClass Zelig-exp
#'
#' @include model-zelig.R

zexp <- setRefClass("Zelig-exp",
                        contains = "Zelig",
                        fields = list(simalpha = "list",
                                      linkinv = "function"))

zexp$methods(
  initialize = function() {
    callSuper()
    .self$name <- "exp"
    .self$authors <- "Olivia Lau, Kosuke Imai, Gary King"
    .self$packageauthors <- "Terry M. Therneau, and Thomas Lumley"
    .self$year <- 2011
    .self$description <- "Exponential Regression for Duration Dependent Variables"
    .self$fn <- quote(survival::survreg)
    .self$linkinv <- survreg.distributions[["exponential"]]$itrans
    # JSON
    .self$outcome <- "continous"
    .self$wrapper <- "exp"
    .self$acceptweights <- TRUE
  }
)

zexp$methods(
  zelig = function(formula, ..., robust = FALSE, cluster = NULL, data, weights = NULL, by = NULL, bootstrap = FALSE) {

    localFormula <- formula # avoids CRAN warning about deep assignment from formula existing separately as argument and field
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    if (!(is.null(cluster) || robust))
      stop("If cluster is specified, then `robust` must be TRUE")
    # Add cluster term
    if (robust || !is.null(cluster))
      localFormula <- cluster.formula(localFormula, cluster)
    .self$model.call$dist <- "exponential"
    .self$model.call$model <- FALSE
    callSuper(formula = localFormula, data = data, ..., robust = robust,
              cluster = cluster,  weights = weights, by = by, bootstrap = bootstrap)
    rse<- lapply(.self$zelig.out$z.out, (function(x) vcovHC(x, type = "HC0")))
    .self$test.statistics<- list(robust.se = rse)
  }
)

zexp$methods(
  qi = function(simparam, mm) {
    eta <- simparam %*% t(mm)
    ev <- as.matrix(apply(eta, 2, linkinv))
    pv <- as.matrix(rexp(length(ev), rate = 1 / ev))
    return(list(ev = ev, pv = pv))
  }
)

zexp$methods(
  mcfun = function(x, b0=0, b1=1, alpha=1, sim=TRUE){
    .self$mcformula <- as.Formula("Surv(y.sim, event) ~ x.sim")

    lambda <-exp(b0 + b1 * x)
    event <- rep(1, length(x))
    y.sim <- rexp(n=length(x), rate=lambda)
    y.hat <- 1/lambda

    if(sim){
        mydata <- data.frame(y.sim=y.sim, event=event, x.sim=x)
        return(mydata)
    }else{
        mydata <- data.frame(y.hat=y.hat, event=event, x.seq=x)
        return(mydata)
    }
  }
)
