#' @include utils.R
#' @include model-zelig.R
#' @include model-tobit.R
#' @include model-glm.R
#' @include model-binchoice.R
#' @include model-probit.R
#' @include model-poisson.R
#' @include model-normal.R
#' @include model-negbinom.R
#' @include model-ls.R
#' @include model-lognorm.R
#' @include model-logit.R
#' @include model-gamma.R
#' @include model-exp.R
#' @include model-relogit.R
#' @include model-quantile.R
#' @include model-gee.R
#' @include model-binchoice-gee.R
#' @include model-logit-gee.R
#' @include model-probit-gee.R
#' @include model-gamma-gee.R
#' @include model-normal-gee.R
#' @include model-poisson-gee.R
#' @include model-bayes.R
#' @include model-factor-bayes.R
#' @include model-logit-bayes.R
#' @include model-mlogit-bayes.R
#' @include model-normal-bayes.R
#' @include model-oprobit-bayes.R
#' @include model-poisson-bayes.R
#' @include model-probit-bayes.R

zelig <- function(formula, model, data, ..., by = NULL, cite = TRUE) {
#   .Deprecated("\nz$new() \nz$zelig(...)")
  # Begin: Zelig 5 models
  if (model == "ls")
    z5 <- zls$new()
  else if (model == "logit")
    z5 <- zlogit$new()
  else if (model == "probit")
    z5 <- zprobit$new()
  else if (model == "gamma")
    z5 <- zgamma$new()
  else if (model == "exp")
    z5 <- zexp$new()
  else if (model == "negbin")
    z5 <- znegbin$new()
  else if (model == "normal")
    z5 <- znormal$new()
  else if (model == "poisson")
    z5 <- zpoisson$new()
  else if (model == "expnorm")
    z5 <- zexpnorm$new()
  else if (model == "lognorm")
    z5 <- zlognorm$new()
  else if (model == "tobit")
    z5 <- ztobit$new()
  else if (model == "relogit")
    z5 <- zrelogit$new()
  else if (model == "quantile")
    z5 <- zquantile$new()
  else if (model == "logit.gee")
    z5 <- zlogitgee$new()
  else if (model == "probit.gee")
    z5 <- zprobitgee$new()
  else if (model == "gamma.gee")
    z5 <- zgammagee$new()
  else if (model == "normal.gee")
    z5 <- znormalgee$new()
  else if (model == "poisson.gee")
    z5 <- zpoissongee$new()
  else if (model == "factor.bayes")
    z5 <- zfactorbayes$new()
  else if (model == "logit.bayes")
    z5 <- zlogitbayes$new()
  else if (model == "mlogit.bayes")
    z5 <- zmlogitbayes$new()
  else if (model == "normal.bayes")
    z5 <- znormalbayes$new()
  else if (model == "oprobit.bayes")
    z5 <- zoprobitbayes$new()
  else if (model == "poisson.bayes")
    z5 <- zpoissonbayes$new()
  else if (model == "probit.bayes")
    z5 <- zprobitbayes$new()
  else
    stop("Model '", model,"' not found")
  ## End: Zelig 5 models (more to be linked from, e.g, Zelig5Choice)
  mf <- match.call()
  mf$model <- NULL
  mf$cite <- NULL
  mf[[1]] <- quote(z5$zelig)
  mf <- try(eval(mf, environment()), silent = TRUE)
  if ("try-error" %in% class(mf))
    z5$zelig(formula = formula, data = data, ...)
  if (cite)
    z5$cite()
  return(z5)
}

setx <- function(obj, fn = NULL, data = NULL, cond = FALSE, ...) {
  # .Deprecated("\nz$new() \nz$zelig(...) \nz$setx() or z$setx1 or z$setrange")
  x5 <- obj$copy()
  # This is the length of each argument in '...'s
  s <- list(...)
  if (length(s) > 0) {
    hold <- rep(1, length(s))
    for(i in 1:length(s)) {
      hold[i] <- length(s[i][[1]])
    }
  } else {
    hold <- 1
  }
  if (max(hold) > 1) {
    x5$setrange(...)
  } else {
    x5$setx(...)
  }
  return(x5)
}

sim <- function(obj, x = NULL, x1 = NULL, y = NULL, num = 1000, bootstrap = F, 
                bootfn = NULL, cond.data = NULL, ...) {
  # .Deprecated("\nz$new() \n[...] \nz$sim(...)")
  s5 <- x$copy()
  if (!is.null(x1)) {
    s15 <- x1$copy()
    if (!is.null(s15$setx.out$x)) {
      s5$setx.out$x1 <- s15$setx.out$x
      s5$bsetx1 <- TRUE
    }
    if (!is.null(s15$setx.out$range)) {
      s5$range1<-s15$range
      s5$setx.out$range1 <- s15$setx.out$range
      s5$bsetrange1 <- TRUE
    }
  }
  s5$sim(num = num)
  return(s5)
}
