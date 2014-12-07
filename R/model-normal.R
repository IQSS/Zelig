#' @include model-zelig.R
#' @include model-glm.R
znormal <- setRefClass("Zelig-normal",
                       contains = "Zelig-glm")

znormal$methods(
  initialize = function() {
    callSuper()
    .self$name <- "normal"
    .self$family <- "gaussian"
    .self$link <- "identity"
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$year <- 2008
    .self$category <- "continuous"
    .self$description <- "Normal Regression for Continuous Dependent Variables"
    # JSON
    .self$outcome <- "continuous"
    .self$wrapper <- "normal"
  }
)

znormal$methods(
  param = function(z.out) {
    degrees.freedom <- z.out$df.residual
    sig2 <- base::summary(z.out)$dispersion # not to call class summary method
    simparam <- mvrnorm(n = .self$num,
                              mu = coef(z.out),
                              Sigma = vcov(z.out))
    simalpha <- sqrt(degrees.freedom * sig2 
                     / rchisq(.self$num, degrees.freedom))
    simparam <- list(simparam = simparam, simalpha = simalpha)
    return(simparam)
  }
)

znormal$methods(
  qi = function(simparam, mm) {
    theta <- matrix(simparam$simparam %*% t(mm),
                    nrow = nrow(simparam$simparam))
    ev <- theta
    pv <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))
    for (j in 1:nrow(ev))
      pv[j, ] <- rnorm(ncol(ev),
                       mean = ev[j, ],
                       sd = simparam$simalpha[j])
    return(list(ev = ev, pv = pv))
  }
)
