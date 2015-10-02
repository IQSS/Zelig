#' Negative Binomial Regression for Event Count Dependent Variables
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-negbin.html}
#' @import methods
#' @export Zelig-negbin
#' @exportClass Zelig-negbin
#'
#' @include model-zelig.R

znegbin <- setRefClass("Zelig-negbin",
                         contains = "Zelig",
                         field = list(simalpha = "list" # ancillary parameters
                         ))

znegbin$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(MASS::glm.nb)
    .self$name <- "negbin"
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$packageauthors <- "William N. Venables, and Brian D. Ripley"
    .self$year <- 2008
    .self$category <- "count"
    .self$description <- "Negative Binomial Regression for Event Count Dependent Variables"
    # JSON
    .self$outcome <- "discrete"
    .self$wrapper <- "negbin"
    .self$acceptweights <- TRUE
  }
)

znegbin$methods(
  zelig = function(formula, data, ..., weights=NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    callSuper(formula=formula, data=data, ..., weights=weights, by = by)
    rse<-plyr::llply(.self$zelig.out$z.out, (function(x) vcovHC(x,type="HC0")))
    .self$test.statistics<- list(robust.se = rse)
  }
)

znegbin$methods(
  param = function(z.out) {
    simalpha.local <- z.out$theta
    simparam.local <- mvrnorm(n = .self$num, mu = coef(z.out),
                        Sigma = vcov(z.out))
    simparam.local <- list(simparam = simparam.local, simalpha = simalpha.local)
    return(simparam.local)
  }
)

znegbin$methods(
  qi = function(simparam, mm) {
    coeff <- simparam$simparam
    alpha <- simparam$simalpha
    inverse <- family(.self$zelig.out$z.out[[1]])$linkinv
    eta <- coeff %*% t(mm)
    theta <- matrix(inverse(eta), nrow=nrow(coeff))
    ev <- theta
    pv <- matrix(NA, nrow=nrow(theta), ncol=ncol(theta))
    #
    for (i in 1:ncol(ev))
      pv[, i] <- rnegbin(nrow(ev), mu = ev[i, ], theta = alpha[i])
    return(list(ev  = ev, pv = pv))
  }
)

znegbin$methods(
  test = function(b0 = -1, b1 = 1, nsim = 1000, minx = -1, maxx = 1) {
    x.init <- mcunit.init(nsim, minx, maxx)
    
    b0 <- b0
    b1 <- b1 
    sd <- 1
    
    mu.sim <- exp(b1 * x.init[,1] + b0)
    y.sim <- rnbinom(nsim, 1, mu = mu.sim)
    y.true <- exp(b1 * x.init[,2] + b0)
    data.local <- cbind(x.init, y.sim, y.true)
    
    z <- znegbin$new()
    callSuper(z, data.local)
    
  }
)





# 
# n<-6;p<-.3;
# mu.sim <- exp(b1[i]* x.sim + b0[i])
# zeta.sim<-rgamma(nsim , n, p/(1-p))
# lambda.tilde.sim <- mu.sim * zeta.sim
# data$y.sim.negbinom <- rpois(nsim,lambda.tilde.sim)
# data$y.hat.negbinom <- exp(b1[i]* x.sim + b0[i]) * zeta.sim
