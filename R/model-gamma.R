#' @include model-zelig.R
#' @include model-glm.R
zgamma <- setRefClass("Zelig-gamma",
                      contains = "Zelig-glm",
                      field = list(simalpha = "numeric" # ancillary parameters
                      ))

zgamma$methods(
  initialize = function() {
    callSuper()
    .self$name <- "gamma"
    .self$family <- "Gamma"
    .self$link <- "inverse"
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$year <- 2007
    .self$category <- "bounded"
    .self$description <- "Gamma Regression for Continuous, Positive Dependent Variables"
    # JSON
    .self$outcome <- "continous"
  }
)

zgamma$methods(
  param = function(num, ...) {
    shape <- gamma.shape(.self$zelig.out)
    .self$simalpha <- rnorm(n = num, mean = shape$alpha, sd = shape$SE)
    .self$simparam <- mvrnorm(n = num, mu = coef(.self$zelig.out),
                              Sigma = vcov(.self$zelig.out))
  }
)

zgamma$methods(
  qi = function(x) {
    coeff <- .self$simparam
    eta <- coeff %*% t(x)
    theta <- matrix(1 / eta, nrow = nrow(coeff))
    ev <- theta
    pv <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))
    for (i in 1:nrow(ev))
      pv[i, ] <- rgamma(ncol(ev), shape=.self$simalpha[i], 
                        scale = theta[i, ] / .self$simalpha[i])
    return(list(ev = ev, pv = pv))
  }
)

