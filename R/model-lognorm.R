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
  }
)

zlognorm$methods(
  zelig = function(formula, ..., robust = FALSE, cluster = NULL, data, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    if (!(is.null(cluster) || robust))
      stop("If cluster is specified, then `robust` must be TRUE")
    # Add cluster term
    if (robust || !is.null(cluster))
      formula <- cluster.formula(formula, cluster)
    .self$model.call$dist <- "lognormal"
    .self$model.call$model <- FALSE
    callSuper(formula = formula, data = data, ..., robust = robust,
              cluster = cluster, by = by)
  }
)

zlognorm$methods(
  param = function(z.out) {
    coeff <- coef(z.out)
    mu <- c(coeff, log(z.out$scale))
    cov <- vcov(z.out)
    simulations <- mvrnorm(.self$num, mu = mu, Sigma = cov)
    simparam.local <- as.matrix(simulations[, 1:length(coeff)])
    simalpha <- as.matrix(simulations[, -(1:length(coeff))])
    simparam.local <- list(simparam = simparam.local, simalpha = simalpha)
    return(simparam.local)
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
    dimnames(ev) <- dimnames(theta)
    return(list(ev = ev, pv = ev))
  }
)
