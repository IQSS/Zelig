#' @include model-zelig.R
#' @include model-glm.R
znormal <- setRefClass("Zelig-normal",
                       contains = "Zelig-glm",
                       field = "simalpha")

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
  param = function(num, ...) {
    degrees.freedom <- .self$zelig.out$df.residual
    sig2 <- base::summary(.self$zelig.out)$dispersion # not to call class summary method
    .self$simparam <- mvrnorm(n = num,
                              mu = coef(.self$zelig.out),
                              Sigma = vcov(.self$zelig.out))
    .self$simalpha <- sqrt(degrees.freedom * sig2 / rchisq(num, degrees.freedom))
  }
)

znormal$methods(
  qi = function(x) {
    .self$linkinv <- eval(call("binomial", "probit"))$linkinv
    theta <- matrix(.self$simparam %*% t(x), nrow = nrow(.self$simparam))
    ev <- theta
    pv <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))
    for (i in 1:nrow(ev))
      pv[i,] <- rnorm(ncol(ev), mean = ev[i,], sd = .self$simalpha[i])
    return(list(ev = ev, pv = pv))
  }
)
