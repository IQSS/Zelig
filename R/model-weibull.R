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
    simparam <- as.matrix(simulations[, 1:length(coeff)])
    simalpha <- as.matrix(simulations[, -(1:length(coeff))])
    simparam <- list(simparam = simparam, simalpha = simalpha)
    return(simparam)
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
