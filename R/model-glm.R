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
    .self$model.call <- match.call(expand.dots = TRUE)
    .self$model.call$family <- call(.self$family, .self$link)
    callSuper(formula = formula, data = data, ..., weights = NULL)
  }
)

zglm$methods(
  param = function(i) {
    .self$simparam[[i]] <- mvrnorm(n=.self$num, mu=coef(.self$zelig.out[[i]]),
                                   Sigma=vcov(.self$zelig.out[[i]]))
  }
)
