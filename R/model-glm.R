#' Generalized Linear Model object for inheritance across models in Zelig
#'
#' @import methods
#' @export Zelig-glm
#' @exportClass Zelig-glm
#'
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
    .self$packageauthors <- "R Core Team"
    .self$acceptweights <- FALSE # "Why glm refers to the number of trials as weight is a trick question to the developers' conscience."
  }
)

zglm$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL, bootstrap = FALSE) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    .self$model.call$family <- call(.self$family, .self$link)
    callSuper(formula = formula, data = data, ..., weights = weights, by = by, bootstrap = bootstrap)
    rse <- plyr::llply(.self$zelig.out$z.out, (function(x) vcovHC(x, type = "HC0")))
    .self$test.statistics <- list(robust.se = rse)
  }
)
