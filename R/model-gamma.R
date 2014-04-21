zgamma <- setRefClass("Zelig-gamma",
                      contains="Zelig-glm",
                      field=list(simalpha = "numeric" # ancillary parameters
                      ))

zgamma$methods(
  initialize = function() {
    callSuper()
    .self$model <- "gamma"
    .self$family <- "Gamma"
    .self$link <- "inverse"
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$year <- 2007
    .self$category <- "bounded"
    .self$text <- "Gamma Regression for Continuous, Positive Dependent Variables"
  }
)

zgamma$methods(
  param = function(num, ...) {
    shape <- gamma.shape(.self$zelig.out)
    .self$simalpha <- rnorm(n=num, mean=shape$alpha, sd=shape$SE)
    .self$simparam <- mvrnorm(n=num, mu=coef(.self$zelig.out),
                              Sigma=vcov(.self$zelig.out))
  }
)

zgamma$methods(
  qi = function(x=NULL, y=NULL, param=NULL) {
    coef <- .self$simparam
    eta <- coef %*% t(x)
    theta <- matrix(1 / eta, nrow = nrow(coef))
    ev <- theta
    pr <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))
    for (i in 1:nrow(ev))
      pr[i, ] <- rgamma(ncol(ev), shape=.self$simalpha[i], scale = theta[i, ] / .self$simalpha[i])
    return(list("Expected Values: E(Y|X)"  = ev,
                "Predicted Values: Y|X" = pr))
  }
)

