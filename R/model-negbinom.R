#' @include model-zelig.R
znegbinom <- setRefClass("Zelig-negbinom",
                         contains="Zelig",
                         field=list(simalpha = "numeric" # ancillary parameters
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
  zelig = function(formula, data, ..., weights=NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    callSuper(formula=formula, data=data, ..., weights=NULL)
    .self$zelig.out <- eval(.self$model.call)
  }
)

znegbinom$methods(
  param = function(num, ...) {
    .self$simalpha <- .self$zelig.out$theta
    .self$simparam <- mvrnorm(n = num, mu = coef(.self$zelig.out),
                              Sigma = vcov(.self$zelig.out))
  }
)

znegbinom$methods(
  qi = function(x = NULL, y = NULL, num = 1000, param = NULL) {
    coef <- .self$simparam
    alpha <- .self$simalpha
    inverse <- family(.self$zelig.out)$linkinv
    eta <- coef %*% t(x)
    theta <- matrix(inverse(eta), nrow=nrow(coef))
    ev <- theta
    pr <- matrix(NA, nrow=nrow(theta), ncol=ncol(theta))
    #
    for (i in 1:ncol(ev))
      pr[, i] <- rnegbin(nrow(ev), mu = ev[i, ], theta = alpha[i])
    return(list("Expected Values: E(Y|X)"  = ev,
                "Predicted Values: Y|X"    = as.factor(pr)))
  }
)

