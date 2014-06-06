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
  qi = function(x) {
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    coeff <- .self$simparam
    eta <- coeff %*% t(x)
    eta <- Filter(function (y) !is.na(y), eta)
    theta <- matrix(.self$linkinv(eta), nrow = nrow(coeff))
    ev <- matrix(.self$linkinv(eta), ncol = ncol(theta))
    pv <- matrix(nrow = nrow(ev), ncol = ncol(ev))
    for (i in 1:ncol(ev))
      pv[, i] <- rbinom(length(ev[, i]), 1, prob = ev[, i])
    levels(pv) <- c(0, 1)
    return(list(ev = ev, pv = pv))
  }
)
