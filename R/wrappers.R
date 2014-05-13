# Proof of concept
zeligw <- function(formula, data, model,..., by = NULL, cite = FALSE, genv = TRUE) {
  if (model == "ls")
    z5 <- zls$new()
  else if (model == "logit")
    z5 <- zlogit$new()
  mf <- match.call()
  mf$model <- NULL
  mf[[1]] <- quote(z5$zelig)
  mf <- eval(mf, environment())
  return(z5)
}

setxw <- function(z5, ...) {
  x5 <- z5$copy()
  x5$setx(...)
  return(x5)
}

simw <- function(z5, x5, num) {
  s5 <- x5$copy()
  s5$sim(num = num)
  return(s5)
}

summaryw <- function(s5) {
  s5$summarize()
}

# Proof of concept
zeligw <- function(formula, data, model,..., by = NULL, cite = FALSE, genv = TRUE) {
  if (model == "ls")
    z5 <- zls$new()
  else if (model == "logit")
    z5 <- zlogit$new()
  else if (model == "probit")
    z5 <- zprobit$new()
  ## else if...: include other models
  mf <- match.call()
  mf$model <- NULL
  mf[[1]] <- quote(z5$zelig)
  mf <- try(eval(mf, environment()))
  if (class(mf) == "try-error")
    z5$zelig(formula=formula, data=data, ...)
  return(z5)
}

setxw2 <- function(z5, ...) {
  x5 <- z5$copy()
  x5$setx(...)
  return(x5)
}

simw2 <- function(z5, x5, num) {
  s5 <- x5$copy()
  s5$sim(num = num)
  return(s5)
}

summaryw2 <- function(s5) {
  s5$summarize()
}