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
#' @include model-bbinchoice.R
#' @include model-blogit.R
#' @include model-bprobit.R
#' @include model-relogit.R
zelig <- function(formula, data, model,..., by = NULL, cite = TRUE) {
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
  else if (model == "blogit")
    z5 <- zblogit$new()
  else if (model == "bprobit")
    z5 <- zbprobit$new()
  else if (model == "expnorm")
    z5 <- zexpnorm$new()
  else if (model == "lognorm")
    z5 <- zlognorm$new()
  else if (model == "tobit")
    z5 <- ztobit$new()
  else
    stop("Model '", model,"' not found")
  ## End: Zelig 5 models
  mf <- match.call()
  mf$model <- NULL
  mf$cite <- NULL
  mf[[1]] <- quote(z5$zelig)
  mf <- try(eval(mf, environment()))
  if (class(mf) == "try-error")
    z5$zelig(formula = formula, data = data, ...)
  if (cite)
    z5$cite()
  z4 <- list()
  z4$z5 <- z5
  class(z4) <- "z4"
  return(z4)
}

setx <- function(z4, ...) {
#   .Deprecated("\nz$new() \nz$zelig(...) \nz$setx() or z$setx1 or z$setrange")
  x4 <- list()
  x4$z5 <- z4$z5$copy()
  x4$z5$setx(...)
  class(x4) <- "z4"
  return(x4)
}

sim <- function(z4, x, x1 = NULL, num = 1000) {
#   .Deprecated("\nz$new() \n[...] \nz$sim(...)")
  s4 <- list()
  s4$z5 <- x$z5$copy()
  if (!is.null(x1)) {
    s14 <- list()
    s14$z5 <- x1$z5$copy()
    s4$z5$setx.out$x1 <- s14$z5$setx.out$x
  }
  s4$z5$sim(num = num)
  class(s4) <- "z4"
  return(s4)
}

summary.z4 <- function(s4) {
  s4$z5$summarize()
}
