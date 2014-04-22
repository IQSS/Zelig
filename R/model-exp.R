zexp <- setRefClass("Zelig-lognorm",
                        contains="Zelig",
                        fields = list(simalpha = "matrix",
                                      linkinv = "function"))

zexp$methods(
  initialize = function() {
    callSuper()
    .self$model <- "exp"
    .self$authors <- "Olivia Lau, Kosuke Imai, Gary King"
    .self$year <- 2011
    .self$text = "Exponential Regression for Duration Dependent Variables"
    .self$fn <- quote(survival::survreg)
    .self$linkinv = survreg.distributions[["exponential"]]$itrans
  }
)

zexp$methods(
  zelig = function(formula, ..., robust = FALSE, cluster = NULL, data) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    if (!(is.null(cluster) || robust))
      stop("If cluster is specified, then `robust` must be TRUE")
    # Add cluster term
    if (robust || !is.null(cluster))
      formula <- cluster.formula(formula, cluster)
    callSuper(formula=formula, data=data, ..., robust=robust, cluster=cluster)
    .self$model.call$dist <- "exponential"
    .self$model.call$model <- FALSE
    .self$zelig.out <- eval(.self$model.call)
  }
)

zexp$methods(
  param = function(num) {
    mu <- coef(.self$zelig.out)
    cov <- vcov(.self$zelig.out)
    .self$simparam = mvrnorm(num, mu=mu, Sigma=cov)
  }
)

zexp$methods(
  qi = function(x) {
    coef <- .self$simparam
    eta <- coef %*% t(x)
    ev <- as.matrix(apply(eta, 2, linkinv))
    pv <- as.matrix(rexp(length(ev), rate = 1 / ev))
    return(list("Expected Values: E(Y|X)"  = ev,
                "Predicted Values: Y|X"    = pv))
  }
)


