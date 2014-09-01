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
    .self$year <- 2011
    .self$description = "Linear regression for Left-Censored Dependent Variable"
    .self$fn <- quote(AER::tobit)
    # JSON
    .self$outcome <- "continous"
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
    simparam <- as.matrix(simfull[, -ncol(simfull)])
    simalpha <- exp(as.matrix(simfull[, ncol(simfull)]))
    simparam <- list(simparam = simparam, simalpha = simalpha)
    return(simparam)
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
