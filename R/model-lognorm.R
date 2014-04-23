zlognorm <- setRefClass("Zelig-lognorm",
                        contains="Zelig",
                        fields = list(simalpha = "matrix",
                                      linkinv = "function"))

zlognorm$methods(
  initialize = function() {
    callSuper()
    .self$model <- "lognorm"
    .self$authors <- "Matthew Owen, Olivia Lau, Kosuke Imai, Gary King"
    .self$year <- 2007
    .self$text = "Log-Normal Regression for Duration Dependent Variables"
    .self$fn <- quote(survival::survreg)
    .self$linkinv <- survreg.distributions[["lognormal"]]$itrans
  }
)

zlognorm$methods(
  zelig = function(formula, ..., robust = FALSE, cluster = NULL, data) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    if (!(is.null(cluster) || robust))
      stop("If cluster is specified, then `robust` must be TRUE")
    # Add cluster term
    if (robust || !is.null(cluster))
      formula <- cluster.formula(formula, cluster)
    callSuper(formula=formula, data=data, ..., robust=robust, cluster=cluster)
    .self$model.call$dist <- "lognormal"
    .self$model.call$model <- FALSE
    .self$zelig.out <- eval(.self$model.call)
  }
)

zlognorm$methods(
  param = function(num) {
    coef <- coef(.self$zelig.out)
    mu <- c(coef, log(.self$zelig.out$scale))
    cov <- vcov(.self$zelig.out)
    simulations <- mvrnorm(num, mu=mu, Sigma=cov)
    .self$simparam = as.matrix(simulations[, 1:length(coef)])
    .self$simalpha = as.matrix(simulations[, -(1:length(coef))])
  }
)

zlognorm$methods(
  qi = function(x) {
    alpha <- .self$simalpha
    beta <- .self$simparam
    coef <- .self$simparam
    eta <- coef %*% t(x)
    theta <- as.matrix(apply(eta, 2, linkinv))
    ev <- exp(log(theta) + 0.5 * (exp(alpha))^2)
    dimnames(ev) <- dimnames(theta)
    return(list("Expected Values: E(Y|X)"  = ev))
#                 "Predicted Values: Y|X"    = ev))
  }
)
