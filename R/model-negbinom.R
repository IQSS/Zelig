#' @include model-zelig.R
znegbinom <- setRefClass("Zelig-negbinom",
                         contains="Zelig",
                         field=list(simalpha = "list" # ancillary parameters
                         ))

znegbinom$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(MASS::glm.nb)
    .self$name <- "negbinom"
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$year <- 2008
    .self$category <- "count"
    .self$description <- "Negative Binomial Regression for Event Count Dependent Variables"
    # JSON
    .self$outcome <- "discrete"
  }
)

znegbinom$methods(
  zelig = function(formula, data, ..., weights=NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    callSuper(formula=formula, data=data, ..., weights=NULL, by = by)
  }
)

znegbinom$methods(
  param = function(z.out) {
    simalpha <- z.out$theta
    simparam <- mvrnorm(n = .self$num, mu = coef(z.out),
                        Sigma = vcov(z.out))
    simparam <- list(simparam = simparam, simalpha = simalpha)
    return(simparam)
  }
)

znegbinom$methods(
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
    return(list(ev  = ev, pv = as.factor(pv)))
  }
)

