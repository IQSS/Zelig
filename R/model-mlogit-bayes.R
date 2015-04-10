#' Bayesian Multinomial Logistic Regression
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-mlogitbayes.html}
#' @import methods
#' @export Zelig-mlogit-bayes
#' @exportClass Zelig-mlogit-bayes
#'
#' @include model-zelig.R
#' @include model-bayes.R

zmlogitbayes <- setRefClass("Zelig-mlogit-bayes",
                             contains = c("Zelig-bayes"))

zmlogitbayes$methods(
  initialize = function() {
    callSuper()
    .self$name <- "mlogit-bayes"
    .self$year <- 2013
    .self$category <- "discrete"
    .self$authors <- "Ben Goodrich, Ying Lu"
    .self$description = "Bayesian Multinomial Logistic Regression for Dependent Variables with Unordered Categorical Values"
    .self$fn <- quote(MCMCpack::MCMCmnl)
    # JSON from parent
    .self$wrapper <- "mlogit.bayes"
  }
)

zmlogitbayes$methods(
  qi = function(simparam, mm) {
    resp <- model.response(model.frame(.self$formula, data = .self$data))
    level <- length(table(resp))
    p <- dim(model.matrix(eval(.self$formula), data = .self$data))[2]
    coef <- simparam
    eta <- array(NA, c(nrow(coef), level, nrow(mm)))
    eta[, 1, ] <- matrix(0, nrow(coef), nrow(mm))
    for (j in 2:level) {
      ind <- (1:p) * (level - 1) - (level - j)
      eta[, j, ]<- coef[, ind] %*% t(mm)
    }
    eta <- exp(eta)
    ev <- array(NA, c(nrow(coef), level, nrow(mm)))
    pv <- matrix(NA, nrow(coef), nrow(mm))
    colnames(ev) <- rep(NA, level)
    for (k in 1:nrow(mm)) {
      for (j in 1:level)
        ev[, j, k] <- eta[, j, k] / rowSums(eta[, , k])
    }
    for (j in 1:level) {
      colnames(ev)[j] <- paste("P(Y=", j, ")", sep="")
    }
    for (k in 1:nrow(mm)) {             
      probs <- as.matrix(ev[, , k])
      temp <- apply(probs, 1, FUN = rmultinom, n = 1, size = 1)
      temp <- as.matrix(t(temp) %*% (1:nrow(temp)))
      pv <- apply(temp, 2, as.character)
      pv <- as.factor(pv)
    }
    return(list(ev = ev, pv = pv))
  }
)

