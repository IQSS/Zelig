#' @include model-zelig.R
#' @include model-glm.R
zpoisson <- setRefClass("Zelig-poisson",
                        contains = "Zelig-glm",
                        fields = list(theta = "ANY"))

zpoisson$methods(
  initialize = function() {
    callSuper()
    .self$name <- "poisson"
    .self$family <- "poisson"
    .self$link <- "log"
    .self$linkinv <- eval(call("binomial", "log"))$linkinv
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$year <- 2007
    .self$category <- "count"
    .self$description <- "Poisson Regression for Event Count Dependent Variables"
    # JSON
    .self$outcome <- "discrete"
  }
)

zpoisson$methods(
  qi = function(simparam, x) {
    coef <- simparam
    eta <- coef %*% t(x)
    theta <- matrix(.self$linkinv(eta), nrow = nrow(coef))
    ev <- theta
    pv <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))
    for (i in 1:ncol(theta))
      pv[, i] <- rpois(nrow(theta), lambda = theta[, i])
    return(list(ev = ev, pv = pv))
  }
)
