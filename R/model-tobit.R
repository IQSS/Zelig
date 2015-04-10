#' Linear Regression for a Left-Censored Dependent Variable
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-tobit.html}
#' @import methods
#' @export Zelig-tobit
#' @exportClass Zelig-tobit
#'
#' @include model-zelig.R

ztobit <- setRefClass("Zelig-tobit",
                      contains = "Zelig",
                      fields = list(above = "numeric",
                                    below = "numeric"))

ztobit$methods(
  initialize = function() {
    callSuper()
    .self$name <- "tobit"
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$packageauthors <- "Christian Kleiber, and Achim Zeileis"
    .self$year <- 2011
    .self$description = "Linear regression for Left-Censored Dependent Variable"
    .self$fn <- quote(AER::tobit)
    # JSON
    .self$outcome <- "continous"
    .self$wrapper <- "tobit"
  }
)

ztobit$methods(
  zelig = function(formula, ..., below = 0, above = Inf,
                   robust = FALSE, data, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    .self$below <- below
    .self$above <- above
    .self$model.call$below <- NULL
    .self$model.call$above <- NULL
    .self$model.call$left <- below
    .self$model.call$right <- above
    callSuper(formula = formula, data = data, ..., by = by)
  }
)

ztobit$methods(
  param = function(z.out) {
    mu <- c(coef(z.out), log(z.out$scale))
    simfull <- mvrnorm(n = .self$num, mu = mu, Sigma = vcov(z.out))
    simparam.local <- as.matrix(simfull[, -ncol(simfull)])
    simalpha <- exp(as.matrix(simfull[, ncol(simfull)]))
    simparam.local <- list(simparam = simparam.local, simalpha = simalpha)
    return(simparam.local)
  }
)

ztobit$methods(
  qi = function(simparam, mm) {
    Coeff <- simparam$simparam %*% t(mm)
    SD <- simparam$simalpha
    alpha <- simparam$simalpha
    lambda <- dnorm(Coeff / SD) / (pnorm(Coeff / SD))
    ev <- pnorm(Coeff / SD) * (Coeff + SD * lambda)
    pv <- ev
    pv <- matrix(nrow = nrow(ev), ncol = ncol(ev))
    for (j in 1:ncol(ev)) {
      pv[, j] <- rnorm(nrow(ev), mean = ev[, j], sd = SD)
      pv[, j] <- pmin(pmax(pv[, j], .self$below), .self$above)
    }
    return(list(ev = ev, pv = pv))
  }
)

ztobit$methods(
  test = function(b0 = -1, b1 = 1, nsim = 1000, minx = -1, maxx = 1) {
    
    x.init <- mcunit.init(nsim, minx, maxx)
    
    mu.sim <- b0 + b1 * x.init[,1]
    y.star <- rnorm(nsim, mean = mu.sim, sd = 1)
    y.sim <- (y.star > 0) * y.star #all positive, censor negative to 0
    y.true <- b0 + b1 * x.init[,2]
    data = data.frame(cbind(x.init, y.star, y.sim, y.true))
    
    z <- ztobit$new()
    callSuper(z, data)
    #     z$zelig(y.sim~x.sim, data = data)
  }
)
