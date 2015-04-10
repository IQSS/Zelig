#' Bayesian Ordered Probit Regression
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig-oprobitbayes.html}
#' @import methods
#' @export Zelig-oprobit-bayes
#' @exportClass Zelig-oprobit-bayes
#'
#' @include model-zelig.R
#' @include model-bayes.R

zoprobitbayes <- setRefClass("Zelig-oprobit-bayes",
                            contains = c("Zelig-bayes"))

zoprobitbayes$methods(
  initialize = function() {
    callSuper()
    .self$name <- "oprobit-bayes"
    .self$year <- 2013
    .self$category <- "discrete"
    .self$authors <- "Ben Goodrich, Ying Lu"
    .self$description = "Bayesian Probit Regression for Dichotomous Dependent Variables"
    .self$fn <- quote(MCMCpack::MCMCoprobit)
    # JSON from parent
    .self$wrapper <- "oprobit.bayes"
  }
)

zoprobitbayes$methods(
  param = function(z.out) {
    simparam <- callSuper(z.out)
    # Produce the model matrix in order to get all terms (explicit and implicit)
    # from the regression model.
    mat <- model.matrix(.self$formula, data = .self$data)
    # Response Terms
    p <- ncol(mat)
    # All coefficients
    coefficients <- simparam
    # Coefficients for predictor variables
    beta <- coefficients[, 1:p]
    # Middle values of "gamma" matrix
    mid.gamma <- coefficients[, -(1:p)]
    # ...
    level <- ncol(coefficients) - p + 2
    # Initialize the "gamma" parameters
    gamma <- matrix(NA, nrow(coefficients), level + 1)
    # The first, second and last values are fixed
    gamma[, 1] <- -Inf
    gamma[, 2] <- 0
    gamma[, ncol(gamma)] <- Inf
    # All others are determined by the coef-matrix (now stored in mid.gamma)
    if (ncol(gamma) > 3)
      gamma[, 3:(ncol(gamma) - 1)] <- mid.gamma
    # return
    simparam <- list(simparam = beta, simalpha = gamma)
    return(simparam)
  }
)

zoprobitbayes$methods(
  qi = function(simparam, mm) {
    beta <- simparam$simparam
    gamma <- simparam$simalpha    
    labels <- levels(model.response(model.frame(.self$formula, data = .self$data)))
    # x is implicitly cast into a matrix
    eta <- beta %*% t(mm)
    # **TODO: Sort out sizes of matrices for these things.
    ev <- array(NA, c(nrow(eta), ncol(gamma) - 1, ncol(eta)))
    pv <- matrix(NA, nrow(eta), ncol(eta))
    # Compute Expected Values
    # ***********************
    # Note that the inverse link function is:
    #   pnorm(gamma[, j+1]-eta) - pnorm(gamma[, j]-eta)
    for (j in 1:(ncol(gamma) - 1)) {
      ev[, j, ] <- pnorm(gamma[, j + 1] - eta) - pnorm(gamma[, j] - eta)
    }
    colnames(ev) <- labels
    # Compute Predicted Values
    # ************************
    for (j in 1:nrow(pv)) {
      mu <- eta[j, ]
      pv[j, ] <- as.character(cut(mu, gamma[j, ], labels = labels))
    }
    pv <- as.factor(pv)
    # **TODO: Update summarize to work with at most 3-dimensional arrays
    ev <- ev[, , 1]
    return(list(ev = ev, pv = pv))
  }
)

