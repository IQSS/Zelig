#' Gamma Regression for Continuous, Positive Dependent Variables
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-gamma.html}
#' @import methods
#' @export Zelig-gamma
#' @exportClass Zelig-gamma
#' 
#' @include model-zelig.R
#' @include model-glm.R

zgamma <- setRefClass("Zelig-gamma",
                      contains = "Zelig-glm")

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
    .self$wrapper <- "gamma"
  }
)

zgamma$methods(
  param = function(z.out) {
    shape <- gamma.shape(z.out)
    simalpha <- rnorm(n = .self$num, mean = shape$alpha, sd = shape$SE)
    simparam.local <- mvrnorm(n = .self$num, mu = coef(z.out),
                                   Sigma = vcov(z.out))
    simparam.local <- list(simparam = simparam.local, simalpha = simalpha)
    return(simparam.local)
  }
)

zgamma$methods(
  qi = function(simparam, mm) {
    coeff <- simparam$simparam
    eta <- coeff %*% t(mm)
    theta <- matrix(1 / eta, nrow = nrow(coeff))
    ev <- theta
    pv <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))
    for (ii in 1:nrow(ev))
      pv[ii, ] <- rgamma(ncol(ev), shape = simparam$simalpha[ii], 
                         scale = theta[ii] / simparam$simalpha[ii])
    return(list(ev = ev, pv = pv))
  }
)

