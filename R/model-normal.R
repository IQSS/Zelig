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
  qi = function(x=NULL, num=1000, param=NULL) {
    .self$linkinv <- eval(call("binomial", "probit"))$linkinv
    # get `num` samples from the underlying distribution
    coef <- .self$simparam
    alpha <- .self$simalpha
    # theta = eta, because inverse of 
    # normal models' link function is
    # the identity
    theta <- matrix(coef %*% t(x), nrow = nrow(coef))
    #
    pr <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))
    #
    ev <- theta
    ev1 <- pr1 <- fd <- NA
    for (i in 1:nrow(ev))
      pr[i,] <- rnorm(ncol(ev), mean = ev[i,], sd = .self$simalpha[i])
    return(list("Expected Values: E(Y|X)"  = ev,
                "Predicted Values: Y|X"    = pr))
  }
)
