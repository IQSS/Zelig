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
    callSuper(formula = formula, data = data, ...,
              weights = NULL)
    .self$zelig.out <- eval(.self$model.call, envir = parent.frame(1))
  }
)

zls$methods(
  param = function(num) {
    .self$simparam <- mvrnorm(n = num,
                              mu = coef(.self$zelig.out),
                              Sigma = vcov(.self$zelig.out))
  }
)

zls$methods(
  qi = function(x) {
    ev <- .self$simparam %*% t(x)
    pv <- ev
    return(list(ev = ev, pv = pv))
  }
)
