#' Binary Choice object for inheritance across models in Zelig
#'
#' @import methods
#' @export Zelig-binchoice
#' @exportClass Zelig-binchoice
#'
#' @include model-zelig.R
#' @include model-glm.R
zbinchoice <- setRefClass("Zelig-binchoice",
                          contains = "Zelig-glm")
  
zbinchoice$methods(
  initialize = function() {
    callSuper()
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$year <- 2007
    .self$category <- "dichotomous"
    .self$family <- "binomial"
    # JSON
    .self$outcome <- "binary"
  }
)

zbinchoice$methods(
  qi = function(simparam, mm) {
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    coeff <- simparam
    eta <- simparam %*% t(mm)
    eta <- Filter(function (y) !is.na(y), eta)
    theta <- matrix(.self$linkinv(eta), nrow = nrow(coeff))
    ev <- matrix(.self$linkinv(eta), ncol = ncol(theta))
    pv <- matrix(nrow = nrow(ev), ncol = ncol(ev))
    for (j in 1:ncol(ev))
      pv[, j] <- rbinom(length(ev[, j]), 1, prob = ev[, j])
    levels(pv) <- c(0, 1)
    return(list(ev = ev, pv = pv))
  }
)
