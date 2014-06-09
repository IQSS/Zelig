#' @include model-zelig.R
#' @include model-glm.R
znormal <- setRefClass("Zelig-normal",
                       contains = "Zelig-glm",
                       field = list("simalpha" = "list"))

znormal$methods(
  initialize = function() {
    callSuper()
    .self$name <- "normal"
    .self$family <- "gaussian"
    .self$link <- "identity"
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$year <- 2008
    .self$category <- "continuous"
    .self$description <- "Normal Regression for Continuous Dependent Variables"
    # JSON
    .self$outcome <- "continuous"
  }
)

znormal$methods(
  param = function(i) {
    degrees.freedom <- .self$zelig.out[[i]]$df.residual
    sig2 <- base::summary(.self$zelig.out[[i]])$dispersion # not to call class summary method
    .self$simparam[[i]] <- mvrnorm(n = .self$num,
                              mu = coef(.self$zelig.out[[i]]),
                              Sigma = vcov(.self$zelig.out[[i]]))
    .self$simalpha[[i]] <- sqrt(degrees.freedom * sig2 / rchisq(.self$num, degrees.freedom))
  }
)

znormal$methods(
  qi = function(i, x) {
    .self$linkinv <- eval(call("binomial", "probit"))$linkinv
    theta <- matrix(.self$simparam[[i]] %*% t(x), nrow = nrow(.self$simparam[[i]]))
    ev <- theta
    pv <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))
    for (j in 1:nrow(ev))
      pv[j, ] <- rnorm(ncol(ev), mean = ev[j, ], sd = .self$simalpha[[i]][j])
    return(list(ev = ev, pv = pv))
  }
)
