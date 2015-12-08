#' Object for Binary Choice outcomes with Survey Weights
#' for inheritance across models in Zelig
#'
#' @import methods
#' @export Zelig-binchoice-survey
#' @exportClass Zelig-binchoice-survey
#'
#' @include model-zelig.R
#' @include model-binchoice.R
#' @include model-survey.R
zbinchoicesurvey <- setRefClass("Zelig-binchoice-survey",
                           contains = c("Zelig-survey",
                                        "Zelig-binchoice"))

zbinchoicesurvey$methods(
  initialize = function() {
    callSuper()
    .self$family <- "binomial"
    .self$category <- "continuous"
    #.self$fn <- quote(geepack::geeglm)
    # JSON from parent
  }
)

zbinchoicesurvey$methods(
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