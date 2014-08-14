#' @include model-zelig.R
zls <- setRefClass("Zelig-ls", contains = "Zelig")

zls$methods(
  initialize = function() {
    callSuper()
    .self$name <- "ls"
    .self$year <- 2007
    .self$category <- "continuous"
    .self$description <- "Least Squares Regression for Continuous Dependent Variables"
    .self$fn <- quote(stats::lm)
    # JSON
    .self$outcome <- "continous"
  }
)

zls$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    callSuper(formula = formula, data = data, ...,
              weights = NULL, by = by)
  }
)

zls$methods(
  param = function(z.out) {
      return(mvrnorm(.self$num, coef(z.out), vcov(z.out)))
  }
)

zls$methods(
  qi = function(simparam, mm) {
    ev <- simparam %*% t(mm)
    pv <- ev
    return(list(ev = ev, pv = pv))
  }
)

