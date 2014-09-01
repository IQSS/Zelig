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
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    .self$model.call$family <- call(.self$family, .self$link)
    callSuper(formula = formula, data = data, ..., weights = NULL, by = by)
    rse<-llply(.self$zelig.out$z.out, (function(x) vcovHC(x,type="HC0")))
    .self$test.statistics<- list(robust.se = rse)
  }
)

zglm$methods(
  param = function(z.out) {
    return(mvrnorm(.self$num, coef(z.out), vcov(z.out)))
  }
)

