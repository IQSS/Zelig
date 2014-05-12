zls <- setRefClass("Zelig-ls", contains = "Zelig")

zls$methods(
  initialize = function() {
    callSuper()
    .self$model <- "ls"
    .self$year <- 2007
    .self$category <- "continuous"
    .self$text = "Least Squares Regression for Continuous Dependent Variables"
    .self$fn <- quote(stats::lm)
  }
)

zls$methods(
  zelig = function(formula, data, ..., weights=NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    callSuper(formula = formula, data = data, ...,
              weights = NULL)
    .self$zelig.out <- eval(.self$model.call, envir = parent.frame(1))
#     .self$zelig.out <- eval(.self$model.call, envir = .self$envir)
    .self$zelig.out <- eval(.self$model.call)
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
    return(list("Expected Values: E(Y|X)"  = ev,
                "Predicted Values: Y|X"    = ev))
  }
)

zls$methods(
  toJSON = function() {
    .self$json <- list()
    .self$json$"description" <- "Least Squares Regression for Continuous Dependent Variables"
    callSuper()
  }
)
