#' @include model-zelig.R
zglm <- setRefClass("Zelig-glm",
                    contains = "Zelig",
                    fields = list(family = "character",
                                  link = "character",
                                  linkinv = "function"))
  
zglm$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(stats::glm)
  }
)

zglm$methods(
  zelig = function(formula, data, ..., weights=NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    callSuper(formula = formula, data = data, ..., weights = NULL)
    .self$model.call$family <- call(.self$family, .self$link)
    .self$zelig.out <- eval(.self$model.call)
  }
)

zglm$methods(
  param = function(num, ...) {
    .self$simparam <- mvrnorm(n=num, mu=coef(.self$zelig.out),
                              Sigma=vcov(.self$zelig.out))
  }
)
