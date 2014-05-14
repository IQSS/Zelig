zpoisson <- setRefClass("Zelig-poisson",
                        contains = "Zelig-glm")

zpoisson$methods(
  initialize = function() {
    callSuper()
    .self$model <- "poisson"
    .self$family <- "poisson"
    .self$link <- "log"
    .self$linkinv <- eval(call("binomial", "log"))$linkinv
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$year <- 2007
    .self$category <- "count"
    .self$text <- "Poisson Regression for Event Count Dependent Variables"
    # JSON
    .self$outcome <- "discrete"
  }
)

zpoisson$methods(
  qi = function(x = NULL, y = NULL, num = 1000, param = NULL) {
    coef <- .self$simparam
    eta <- coef %*% t(x)
    theta <- matrix(.self$linkinv(eta), nrow = nrow(coef))
    ev <- theta
    pr <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))
    for (i in 1:ncol(ev))
      pr[, i] <- rpois(nrow(ev), lambda = ev[, i])
    return(list("Expected Values: E(Y|X)"  = ev,
                "Predicted Values: Y|X"    = as.factor(pr))
    )
  }
)

zpoisson$methods(
  toJSON = function() {
    .self$json <- list()
    callSuper()

  }
)
