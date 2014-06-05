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
  qi = function(x = NULL, y = NULL, num = 1000, param = NULL) {
    .self$linkinv <- eval(call(.self$family, .self$link))$linkinv
    compute.ev <- function(x = NULL, num, param = NULL) {
      if (is.null(x))
        return(NA)
      coef <- .self$simparam
      eta <- coef %*% t(x)
      eta <- Filter(function (y) !is.na(y), eta)
      theta <- matrix(.self$linkinv(eta), nrow = nrow(coef))
      ev <- matrix(.self$linkinv(eta), ncol = ncol(theta))
      return(ev)
    }
    ev <- compute.ev(x, num, param)
    pr <- matrix(nrow = nrow(ev), ncol = ncol(ev))
    for (i in 1:ncol(ev))
      pr[, i] <- rbinom(length(ev[, i]), 1, prob = ev[, i])
    levels(pr) <- c(0, 1)
    return(list("Expected Values: E(Y|X)"  = ev,
                "Predicted Values: Y|X"    = pr))
  }
)
