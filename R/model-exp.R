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
  }
)

zexp$methods(
  zelig = function(formula, ..., robust = FALSE, cluster = NULL, data, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    if (!(is.null(cluster) || robust))
      stop("If cluster is specified, then `robust` must be TRUE")
    # Add cluster term
    if (robust || !is.null(cluster))
      formula <- cluster.formula(formula, cluster)
    .self$model.call$dist <- "exponential"
    .self$model.call$model <- FALSE
    callSuper(formula = formula, data = data, ..., robust = robust,
              cluster = cluster,  by = by)
    rse<-plyr::llply(.self$zelig.out$z.out, (function(x) vcovHC(x,type="HC0")))
    .self$test.statistics<- list(robust.se = rse)
  }
)

zexp$methods(
  param = function(z.out) {
    return(mvrnorm(.self$num, coef(z.out), vcov(z.out)))
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
  test = function(b0 = -1, b1 = 1, nsim = 1000, minx = -1, maxx = 1) {
    
    x.init <- mcunit.init(nsim, minx, maxx)
    
    time.sim = rexp(nsim, rate = exp(-b0 - b1 * x.init[,1]))
    #     C = rweibull(n, shape=1, scale=lambdaC)   #censoring time
    event = time.sim==time.sim   # set to 1 if event is observed
    time.true = rexp(nsim, rate = exp(-b0 - b1 * x.init[,2]))
    data = data.frame(cbind(x.init, time.sim, event, time.true))
    
    z <- zexp$new()
    callSuper(z, data)
    
  }
)


