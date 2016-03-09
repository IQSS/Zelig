#' Log-Normal Regression for Duration Dependent Variables
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-lognorm.html}
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
    .self$mcformula <- as.formula("Surv(y.sim, event) ~ x.sim")
    
    mu <- b0 + b1 * x
    event <- rep(1, length(x))
    y.sim <- rlnorm(n=length(x), meanlog=mu, sdlog=alpha)
    y.hat <- exp(mu + 0.5*alpha^2)
    
    if(sim){
        data <- data.frame(y.sim=y.sim, event=event, x.sim=x)
        return(data)
    }else{
        data <- data.frame(y.hat=y.hat, event=event, x.seq=x)
        return(data)
    }
  }
)
