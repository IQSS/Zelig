#' @include model-zelig.R
zlognorm <- setRefClass("Zelig-lognorm",
                        contains ="Zelig",
                        fields = list(simalpha = "matrix",
                                      linkinv = "function"))

zlognorm$methods(
  initialize = function() {
    callSuper()
    .self$name <- "lognorm"
    .self$authors <- "Matthew Owen, Olivia Lau, Kosuke Imai, Gary King"
    .self$year <- 2007
    .self$description <- "Log-Normal Regression for Duration Dependent Variables"
    .self$fn <- quote(survival::survreg)
    .self$linkinv <- survreg.distributions[["lognormal"]]$itrans
    # JSON
    .self$outcome <- "discrete"
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
    callSuper(formula = formula, data = data, ..., robust = robust, cluster = cluster)
    .self$model.call$dist <- "lognormal"
    .self$model.call$model <- FALSE
    .self$zelig.out <- eval(.self$model.call)
  }
)

zlognorm$methods(
  param = function(num) {
    coeff<- coef(.self$zelig.out)
    mu <- c(coeff, log(.self$zelig.out$scale))
    cov <- vcov(.self$zelig.out)
    simulations <- mvrnorm(num, mu = mu, Sigma = cov)
    .self$simparam = as.matrix(simulations[, 1:length(coeff)])
    .self$simalpha = as.matrix(simulations[, -(1:length(coeff))])
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
    return(list(ev = ev, pv = NA))
  }
)
