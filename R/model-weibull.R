#' Weibull Regression for Duration Dependent Variables
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-weibull.html}
#' @import methods
#' @export Zelig-tobit-bayes
#' @exportClass Zelig-tobit-bayes
#'
#' @include model-zelig.R

zweibull <- setRefClass("Zelig-weibull",
                        contains = "Zelig",
                        fields = list(simalpha = "list",
                                      linkinv = "function",
                                      lambda = "ANY"))
  
zweibull$methods(
  initialize = function() {
    callSuper()
    .self$name <- "weibull"
    .self$authors <- "Olivia Lau, Kosuke Imai, Gary King"
    .self$packageauthors <- "Terry M Therneau, and Thomas Lumley"
    .self$year <- 2007
    .self$description <- "Weibull Regression for Duration Dependent Variables"
    .self$fn <- quote(survival::survreg)
    .self$linkinv <- survreg.distributions[["weibull"]]$itrans
    # JSON
    .self$outcome <- "bounded"
    .self$wrapper <- "weibull"
  }
)

zweibull$methods(
  zelig = function(formula, ..., robust = FALSE, cluster = NULL, data, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    if (!(is.null(cluster) || robust))
      stop("If cluster is specified, then `robust` must be TRUE")
    # Add cluster term
    if (robust || !is.null(cluster))
      formula <- cluster.formula(formula, cluster)
    .self$model.call$dist <- "weibull"
    .self$model.call$model <- FALSE
    callSuper(formula = formula, data = data, ..., robust = robust,
              cluster = cluster,  by = by)
  }
)

zweibull$methods(
  param = function(z.out) {
    coeff <- coef(z.out)
    mu <- c(coeff, z.out$scale)
    cov <- vcov(z.out)
    simulations <- mvrnorm(.self$num, mu = mu, Sigma = cov)
    simparam.local <- as.matrix(simulations[, 1:length(coeff)])
    simalpha.local <- as.matrix(simulations[, -(1:length(coeff))])
    simparam.local <- list(simparam = simparam.local, simalpha = simalpha.local)
    return(simparam.local)
  }
)

zweibull$methods(
  qi = function(simparam, mm) {
    eta <- simparam$simparam %*% t(mm)
    theta <- as.matrix(apply(eta, 2, linkinv))
    ev <- theta * gamma(1 + exp(simparam$simalpha))
    pv <- as.matrix(rweibull(length(ev), 
                             shape = 1 / exp(simparam$simalpha), 
                             scale = theta))
    return(list(ev = ev, pv = pv))
  }
)

zweibull$methods(
  test = function(b0 = -1, b1 = 1, nsim = 1000, minx = -1, maxx = 1) {
    
    x.init <- mcunit.init(nsim, minx, maxx)
    
    
    time.sim = rweibull(nsim, shape=1, scale=exp(b0 + b1 * x.init[,1])) 
    #     censor = rweibull(n, shape=1, scale=lambdaC)   #censoring time
    #     time = pmin(time, censor)
    event = time.sim==time.sim   # set to 1 if event is observed
    time.true = rweibull(nsim, shape=1, scale=exp(b0 + b1 * x.init[,2]))
    data = data.frame(cbind(x.init, time.sim, event, time.true))
    
    z <- zweibull$new()
    callSuper(z, data)
  }
)

# if (model %in% c("weibull", "Weibull", "lognorm", "exp"))
#   link <- survreg.distributions[[object$dist]]$itrans
# else if (model == "tobit")
#   link <- function(x) x
# ev.surv <- function(model, sim.coef, sim.scale, x, link) {
#   eta <- sim.coef %*% t(x)
#   theta <- as.matrix(apply(eta, 2, link))
#   if (model == "lognorm") {
#     ev <- exp(log(theta) + 0.5*(exp(sim.scale))^2)
#     dimnames(ev) <- dimnames(theta)
#   }
#   else if (model %in% c("weibull", "Weibull")) {
#     ev <- theta * gamma(1 + exp(sim.scale))
#     dimnames(ev) <- dimnames(theta)
#   }
#   else if (model %in% c("exp", "tobit")) {
#     ev <- theta
#   }
#   
# 
# 
