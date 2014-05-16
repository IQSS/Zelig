zeligw <- function(formula, data, model,..., by = NULL, cite = FALSE, genv = TRUE) {
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
  else if (model == "bbinchoice")
    z5 <- zbbinchoice$new()
  else if (model == "blogit")
    z5 <- zblogit$new()
  else if (model == "bprobit")
    z5 <- zbprobit$new()
  else if (model == "poisson-bayes")
    z5 <- zpoissonbayes$new()
  else if (model == "poisson-gee")
    z5 <- zpoissongee$new()
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
  mf[[1]] <- quote(z5$zelig)
  mf <- try(eval(mf, environment()))
  if (class(mf) == "try-error")
    z5$zelig(formula = formula, data = data, ...)
  return(z5)
}

setxw <- function(z5, ...) {
  x5 <- z5$copy()
  x5$setx(...)
  return(x5)
}

simw <- function(z5, x5, x15 = NULL, num) {
  s5 <- x5$copy()
  if (!is.null(x15)) {
    s15 <- x15$copy()
    s5$setx.out$x1 <- s15$setx.out$x
  }
  s5$sim(num = num)
  return(s5)
}

summaryw <- function(s5) {
  s5$summarize()
}
