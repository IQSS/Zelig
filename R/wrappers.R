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
  else if (model == "negbinom")
    z5 <- znegbinom$new()
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
  else
    stop("Model '", model,"' not found")
  ## End: Zelig 5 models (more to be linked from, e.g, Zelig5Choice)
  mf <- match.call()
  mf$model <- NULL
  mf$cite <- NULL
  mf[[1]] <- quote(z5$zelig)
  mf <- try(eval(mf, environment()))
  if ("try-error" %in% class(mf))
    z5$zelig(formula = formula, data = data, ...)
  if (cite)
    z5$cite()
  return(z5)
}

setx <- function(obj, fn = NULL, data = NULL, cond = FALSE, ...) {
#   .Deprecated("\nz$new() \nz$zelig(...) \nz$setx() or z$setx1 or z$setrange")
  x5 <- obj$copy()
  x5$setx(...)
  return(x5)
}

sim <- function(obj, x = NULL, x1 = NULL, y = NULL, num = 1000, bootstrap = F, 
                bootfn = NULL, cond.data = NULL, ...) {
# .Deprecated("\nz$new() \n[...] \nz$sim(...)")
  s5 <- x$copy()
  if (!is.null(x1)) {
    s15 <- x1$copy()
    s5$setx.out$x1 <- s15$setx.out$x
    s5$bsetx1<-TRUE              #JH
  }
  s5$sim(num = num)
  return(s5)
}
