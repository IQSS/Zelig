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
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$year <- 2007
    .self$category <- "count"
    .self$description <- "Poisson Regression for Event Count Dependent Variables"
    # JSON
    .self$outcome <- "discrete"
  }
)

zpoisson$methods(
  qi = function(simparam, mm) {
    eta <- simparam %*% t(mm)
    theta <- matrix(.self$linkinv(eta), nrow = nrow(simparam))
    ev <- theta
    pv <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))
    for (i in 1:ncol(theta))
      pv[, i] <- rpois(nrow(theta), lambda = theta[, i])
    return(list(ev = ev, pv = pv))
  }
)
