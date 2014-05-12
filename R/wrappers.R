# Proof of concept
zeligw <- function(formula, data, model,..., by = NULL, cite = FALSE) {
  z5 <- zls$new()
  z5$zelig(formula = formula, data = data, ...)
  z5$zelig.call <- match.call(expand.dots = TRUE)
  .self$model.call <- match.call(expand.dots = TRUE,
                                 call = sys.call(sys.parent(n = 1)))
  .self$model.call[[1]] <- .self$fn
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

