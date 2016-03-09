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
  zelig = function(formula, data, ..., weights=NULL, by = NULL, bootstrap = FALSE) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    callSuper(formula=formula, data=data, ..., weights=weights, by = by, bootstrap = bootstrap)
    rse<-plyr::llply(.self$zelig.out$z.out, (function(x) vcovHC(x,type="HC0")))
    .self$test.statistics<- list(robust.se = rse)
  }
)

znegbin$methods(
  param = function(z.out, method="mvn") {
    simalpha.local <- z.out$theta
    if(identical(method,"mvn")){
      simparam.local <- mvrnorm(n = .self$num, mu = coef(z.out),
                        Sigma = vcov(z.out))
      simparam.local <- list(simparam = simparam.local, simalpha = simalpha.local)
      return(simparam.local)
    } else if(identical(method,"point")){
      return(list(simparam = t(as.matrix(coef(z.out))), simalpha = simalpha.local))
    }
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
  mcfun = function(x, b0=0, b1=1, ..., sim=TRUE){
    mu <- exp(b0 + b1 * x)
    if(sim){
        y <- rnbinom(n=length(x), 1, mu=mu)
        return(y)
    }else{
        return(mu)
    }
  }
)

