#' @include model-zelig.R
ztobit <- setRefClass("Zelig-tobit",
                      contains = "Zelig",
                      fields = list(simalpha = "list",
                                    above = "numeric",
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
  zelig = function(formula, ..., below = 0, above = Inf, robust = FALSE, data) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    .self$below <- below
    .self$above <- above
    .self$model.call$below <- NULL
    .self$model.call$above <- NULL
    .self$model.call$left <- below
    .self$model.call$right <- above
    callSuper(formula = formula, data = data, ...)
  }
)

ztobit$methods(
  param = function(i) {
    mu <- c(coef(.self$zelig.out[[i]]), log(.self$zelig.out[[i]]$scale))
    simfull <- mvrnorm(n = .self$num, mu = mu,
                       Sigma = vcov(.self$zelig.out[[i]]))
    .self$simparam[[i]] <- as.matrix(simfull[, -ncol(simfull)])
    .self$simalpha[[i]] <- exp(as.matrix(simfull[, ncol(simfull)]))
  }
)

ztobit$methods(
  qi = function(i, x) {
    Coeff <- .self$simparam[[i]] %*% t(x)
    SD <- .self$simalpha[[i]]
    alpha <- .self$simalpha[[i]]
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
