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
  zelig = function(formula, data, ..., weights=NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    callSuper(formula=formula, data=data, ..., weights=NULL)
  }
)

znegbinom$methods(
  param = function(i) {
    .self$simalpha[[i]] <- .self$zelig.out[[i]]$theta
    .self$simparam[[i]] <- mvrnorm(n = .self$num, mu = coef(.self$zelig.out[[i]]),
                                   Sigma = vcov(.self$zelig.out[[i]]))
  }
)

znegbinom$methods(
  qi = function(i, x) {
    coef <- .self$simparam[[i]]
    alpha <- .self$simalpha[[i]]
    inverse <- family(.self$zelig.out[[i]])$linkinv
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

