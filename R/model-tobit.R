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
    .self$acceptweights <- TRUE
  }
)

ztobit$methods(
  zelig = function(formula, ..., below = 0, above = Inf,
                   robust = FALSE, data, weights = NULL, by = NULL, bootstrap = FALSE) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    .self$below <- below
    .self$above <- above
    .self$model.call$below <- NULL
    .self$model.call$above <- NULL
    .self$model.call$left <- below
    .self$model.call$right <- above
    callSuper(formula = formula, data = data, ..., weights = weights, by = by, bootstrap = bootstrap)

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


ztobit$methods(
  param = function(z.out, method="mvn") {
    if(identical(method,"mvn")){
      mu <- c(coef(z.out), log(z.out$scale))
      simfull <- mvrnorm(n = .self$num, mu = mu, Sigma = vcov(z.out))
      simparam.local <- as.matrix(simfull[, -ncol(simfull)])
      simalpha <- exp(as.matrix(simfull[, ncol(simfull)]))
      simparam.local <- list(simparam = simparam.local, simalpha = simalpha)
      return(simparam.local)
    } else if(identical(method,"point")){
      return(list(simparam = t(as.matrix(coef(z.out))), simalpha = log(z.out$scale) ))
    }
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
  mcfun = function(x, b0=0, b1=1, alpha=1, sim=TRUE){
    mu <- b0 + b1 * x
    ystar <- rnorm(n=length(x), mean=mu, sd=alpha)
    if(sim){
        y <- (ystar>0) * ystar  # censoring from below at zero
        return(y)
    }else{
        y.uncensored.hat.tobit<- mu + dnorm(mu, mean=0, sd=alpha)/pnorm(mu, mean=0, sd=alpha)
        y.hat.tobit<- y.uncensored.hat.tobit * (1- pnorm(0, mean=mu, sd=alpha) )  # expected value of censored outcome
        return(y.hat.tobit)
    }
  }
)

