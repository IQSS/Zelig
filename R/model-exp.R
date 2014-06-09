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
    .self$year <- 2011
    .self$description <- "Exponential Regression for Duration Dependent Variables"
    .self$fn <- quote(survival::survreg)
    .self$linkinv <- survreg.distributions[["exponential"]]$itrans
    # JSON
    .self$outcome <- "continous"
  }
)

zexp$methods(
  zelig = function(formula, ..., robust = FALSE, cluster = NULL, data) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    if (!(is.null(cluster) || robust))
      stop("If cluster is specified, then `robust` must be TRUE")
    # Add cluster term
    if (robust || !is.null(cluster))
      formula <- cluster.formula(formula, cluster)
    .self$model.call$dist <- "exponential"
    .self$model.call$model <- FALSE
    callSuper(formula = formula, data = data, ..., robust = robust, cluster = cluster)
  }
)

zexp$methods(
  param = function(i) {
    .self$simparam[[i]] = mvrnorm(.self$num, mu = coef(.self$zelig.out[[i]]),
                                  Sigma = vcov(.self$zelig.out[[i]]))
  }
)

zexp$methods(
  qi = function(i, x) {
    eta <- .self$simparam[[i]] %*% t(x)
    ev <- as.matrix(apply(eta, 2, linkinv))
    pv <- as.matrix(rexp(length(ev), rate = 1 / ev))
    return(list(ev = ev, pv = pv))
  }
)


