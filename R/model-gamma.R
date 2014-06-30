#' @include model-zelig.R
#' @include model-glm.R
zgamma <- setRefClass("Zelig-gamma",
                      contains = "Zelig-glm",
                      field = list(simalpha = "list" # ancillary parameters
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
  param = function(i) {
    shape <- gamma.shape(.self$zelig.out[[i]])
    .self$simalpha[[i]] <- rnorm(n = .self$num, mean = shape$alpha, sd = shape$SE)
    .self$simparam[[i]] <- mvrnorm(n = .self$num, mu = coef(.self$zelig.out[[i]]),
                                   Sigma = vcov(.self$zelig.out[[i]]))
  }
)

zgamma$methods(
  qi = function(i, x) {
    coeff <- .self$simparam[[i]]
    eta <- coeff %*% t(x)
    theta <- matrix(1 / eta, nrow = nrow(coeff))
    ev <- theta
    pv <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))
    for (ii in 1:nrow(ev))
      pv[ii, ] <- rgamma(ncol(ev), shape = .self$simalpha[[i]][ii], 
                         scale = theta[ii] / .self$simalpha[[i]][ii])
    return(list(ev = ev, pv = pv))
  }
)

